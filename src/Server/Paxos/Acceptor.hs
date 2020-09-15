{-# LANGUAGE LambdaCase #-}

module Server.Paxos.Acceptor ( Acceptor (..)
                             , create
                             ) where

import Client.WebNodeClient       (LearnClient)
import Entity.AcceptRequest
import Entity.Id
import Entity.Key
import Entity.LearnRequest
import Entity.Nack
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.Promise
import Entity.Proposal
import Entity.SequenceNum
import Entity.Topic
import Entity.ValueResponse
import Node
import Server.Files
import Server.Locks
import Server.Paxos.AcceptorState

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Either              (rights)
import           Data.Maybe               (catMaybes, fromJust)
import qualified Data.Set as S
import           Network.HTTP.Client

data Acceptor m =
    Acceptor { prepare :: !(PrepareRequest -> m PrepareResponse)
             , accept  :: !(AcceptRequest -> m ValueResponseE)
             }

create :: (Show e, MonadIO m) => Id
                              -> Locks Topic
                              -> (Manager -> Node -> LearnClient e)
                              -> IO (Acceptor m)
create myId topicLocks learnBuilder = do

    -- TODO maybe share these HTTP clients
    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 3000000 }

    pure $ Acceptor { prepare = liftIO . prepareService topicLocks getAcceptorState putAcceptorState
                    , accept  = liftIO . acceptService myId (learnBuilder http) topicLocks getAcceptorState putAcceptorState
                    }

    where
    getAcceptorState :: Locked Topic -> SequenceNum -> IO AcceptorState
    getAcceptorState lockedtopic seqNum =
        readState myId lockedtopic seqNum "as" >>= \case
            Just f  -> pure f
            Nothing -> pure $ AcceptorState Nothing Nothing

    putAcceptorState :: Locked Topic -> SequenceNum -> AcceptorState -> IO ()
    putAcceptorState lockedtopic seqNum acceptorState =
        writeState myId lockedtopic seqNum "as" acceptorState

prepareService :: Locks Topic
               -> (Locked Topic -> SequenceNum -> IO AcceptorState)
               -> (Locked Topic -> SequenceNum -> AcceptorState -> IO ())
               -> PrepareRequest
               -> IO PrepareResponse
prepareService topicLocks getAcceptorState putAcceptorState (PrepareRequest (Key topic seqNum) n) =
    -- TODO try limited the scope of this lock
    withLocked topicLocks topic $ \lockedTopic -> do
        state <- getAcceptorState lockedTopic seqNum
        if Just n > acc_notLessThan state
            then do
                putAcceptorState lockedTopic seqNum $! state {acc_notLessThan = Just n}
                pure . PrepareResponse . Right $ Promise n (acc_proposal state)
            else pure . PrepareResponse . Left . Nack . fromJust . acc_notLessThan $ state

acceptService :: Show e => Id
              -> (Node -> LearnClient e)
              -> Locks Topic
              -> (Locked Topic -> SequenceNum -> IO AcceptorState)
              -> (Locked Topic -> SequenceNum -> AcceptorState -> IO ())
              -> AcceptRequest
              -> IO ValueResponseE
acceptService myId learnBuilder topicLocks getAcceptorState putAcceptorState (AcceptRequest nodes key@(Key topic seqNum) n v) = do
    -- TODO try limiting the scope of this lock
    accepted <- withLocked topicLocks topic $ \lockedTopic -> do
        state <- getAcceptorState lockedTopic seqNum
        if Just n >= acc_notLessThan state
            then do
                putAcceptorState lockedTopic seqNum $ state { acc_notLessThan = Just n
                                                            , acc_proposal    = Just (Proposal n v) }
                pure True
            else pure False

    if accepted

        then do

            -- Inform every (responsive) learner
            learnerResponses <- rights
                              . map (fmap (\(ValueResponseM r) -> r))
                            <$> forConcurrently (map learnBuilder $ S.toList nodes) (\c -> c (LearnRequest nodes key myId v))

            -- Consider every consensus claimed by a learner
            pure . ValueResponseE $
                case catMaybes learnerResponses of

                    []     -> Left "No consensus (yet)"

                    -- Sanity check
                    (c:cs) | all (==c) cs -> Right c

                           | otherwise -> Left "Fatal: Inconsistent consensus"

        else pure . ValueResponseE $ Left "acceptService: Not accepted"
