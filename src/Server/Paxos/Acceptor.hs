{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase      #-}

module Server.Paxos.Acceptor ( Acceptor (..)
                             , create
                             ) where

import Client.InternalClient (LearnClient)
import Entity.Id
import Entity.Node
import Entity.Proposal
import Entity.ProposalNumber
import Entity.SequenceNum
import Entity.Topic
import Requests.Accept
import Requests.Prepare
import Requests.Learn
import Server.Files
import Server.Locks

import           Codec.Serialise
import           Control.Concurrent.Async (forConcurrently)
import           Control.DeepSeq          (NFData)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           GHC.Generics             (Generic)
import           Data.Either              (rights)
import           Data.Functor             ((<&>))
import           Data.Maybe               (catMaybes, fromJust)
import qualified Data.Set as S

data Acceptor m =
    Acceptor { prepare :: !(PrepareRequest -> m PrepareResponse)
             , accept  :: !(AcceptRequest  -> m AcceptResponse)
             }

data AcceptorState =
    AcceptorState { acc_notLessThan :: !(Maybe ProposalNumber)
                  , acc_proposal    :: !(Maybe Proposal)
                  } deriving (Generic, NFData, Serialise)

create :: (Show e, MonadIO m) => Id
                              -> Locks (Topic, SequenceNum)
                              -> (Node -> LearnClient e)
                              -> IO (Acceptor m)
create myId topicLocks learnBuilder =

    pure $ Acceptor { prepare = liftIO . prepareService topicLocks getAcceptorSubState putAcceptorSubState
                    , accept  = liftIO . acceptService myId learnBuilder topicLocks getAcceptorSubState putAcceptorSubState
                    }

    where
    getAcceptorSubState :: Locked (Topic, SequenceNum) -> IO AcceptorState
    getAcceptorSubState lock@(Locked (_, seqNum)) =
        readSubState myId lock seqNum "as" <&> \case
            Just f  -> f
            Nothing -> AcceptorState Nothing Nothing

    putAcceptorSubState :: Locked (Topic, SequenceNum) -> AcceptorState -> IO ()
    putAcceptorSubState lock@(Locked (_, seqNum)) acceptorState =
        writeSubState myId lock seqNum "as" acceptorState

prepareService :: Locks (Topic, SequenceNum)
               -> (Locked (Topic, SequenceNum) -> IO AcceptorState)
               -> (Locked (Topic, SequenceNum) -> AcceptorState -> IO ())
               -> PrepareRequest
               -> IO PrepareResponse
prepareService topicLocks getAcceptorSubState putAcceptorSubState (PrepareRequest topic seqNum n) = do
    -- TODO try limited the scope of this lock
    -- This scope appears to just be a single topic/seqnum.as
    r <- withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
        state <- getAcceptorSubState lockedTopic
        if Just n > acc_notLessThan state
            then do
                putAcceptorSubState lockedTopic $! state {acc_notLessThan = Just n}
                pure . Right $ Promise n (acc_proposal state)
            else pure . Left . Nack . fromJust . acc_notLessThan $ state
    pure $ PrepareResponse r

acceptService :: Show e => Id
              -> (Node -> LearnClient e)
              -> Locks (Topic, SequenceNum)
              -> (Locked (Topic, SequenceNum) -> IO AcceptorState)
              -> (Locked (Topic, SequenceNum) -> AcceptorState -> IO ())
              -> AcceptRequest
              -> IO AcceptResponse
acceptService myId learnBuilder topicLocks getAcceptorSubState putAcceptorSubState (AcceptRequest nodes topic seqNum n v) = do

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
                              . map (fmap (\(LearnResponse mv) -> mv))
                            <$> forConcurrently (map learnBuilder $ S.toList nodes) (\c -> c (LearnRequest nodes topic seqNum myId v))

            -- Consider every consensus claimed by a learner
            pure . AcceptResponse $
                case catMaybes learnerResponses of

                    []     -> Left "No consensus (yet)"

                    -- Sanity check
                    (c:cs) | all (==c) cs -> Right c

                           | otherwise -> Left "Fatal: Inconsistent consensus"

        else pure . AcceptResponse $ Left "acceptService: Not accepted"
