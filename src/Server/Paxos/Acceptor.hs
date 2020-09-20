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
             , accept  :: !(AcceptRequest  -> m ValueResponseE)
             }

create :: (Show e, MonadIO m) => Id
                              -> Locks (Topic, SequenceNum)
                              -> (Manager -> Node -> LearnClient e)
                              -> IO (Acceptor m)
create myId topicLocks learnBuilder = do

    -- TODO maybe share these HTTP clients
    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 10000000 }

    pure $ Acceptor { prepare = liftIO . prepareService topicLocks getAcceptorSubState putAcceptorSubState
                    , accept  = liftIO . acceptService myId (learnBuilder http) topicLocks getAcceptorSubState putAcceptorSubState
                    }

    where
    getAcceptorSubState :: Locked (Topic, SequenceNum) -> IO AcceptorState
    getAcceptorSubState lock@(Locked (_, seqNum)) =
        readSubState myId lock seqNum "as" >>= \case
            Just f  -> pure f
            Nothing -> pure $ AcceptorState Nothing Nothing

    putAcceptorSubState :: Locked (Topic, SequenceNum) -> AcceptorState -> IO ()
    putAcceptorSubState lock@(Locked (_, seqNum)) acceptorState =
        writeSubState myId lock seqNum "as" acceptorState

prepareService :: Locks (Topic, SequenceNum)
               -> (Locked (Topic, SequenceNum) -> IO AcceptorState)
               -> (Locked (Topic, SequenceNum) -> AcceptorState -> IO ())
               -> PrepareRequest
               -> IO PrepareResponse
prepareService topicLocks getAcceptorSubState putAcceptorSubState (PrepareRequest (Key topic seqNum) n) =
    -- TODO try limited the scope of this lock
    -- This scope appears to just be a single topic/seqnum.as
    withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
        state <- getAcceptorSubState lockedTopic
        if Just n > acc_notLessThan state
            then do
                putAcceptorSubState lockedTopic $! state {acc_notLessThan = Just n}
                pure . PrepareResponse . Right $ Promise n (acc_proposal state)
            else pure . PrepareResponse . Left . Nack . fromJust . acc_notLessThan $ state

acceptService :: Show e => Id
              -> (Node -> LearnClient e)
              -> Locks (Topic, SequenceNum)
              -> (Locked (Topic, SequenceNum) -> IO AcceptorState)
              -> (Locked (Topic, SequenceNum) -> AcceptorState -> IO ())
              -> AcceptRequest
              -> IO ValueResponseE
acceptService myId learnBuilder topicLocks getAcceptorSubState putAcceptorSubState (AcceptRequest nodes key@(Key topic seqNum) n v) = do
    -- TODO try limiting the scope of this lock
    -- This scope appears to just be a single topic/seqnum.as
    accepted <- withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
        state <- getAcceptorSubState lockedTopic
        if Just n >= acc_notLessThan state
            then do
                putAcceptorSubState lockedTopic $ state { acc_notLessThan = Just n
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
