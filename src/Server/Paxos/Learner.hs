{-# LANGUAGE LambdaCase #-}

module Server.Paxos.Learner ( Learner (..)
                            , create
                            ) where

import           Entity.Id
import           Entity.Key
import           Entity.LearnRequest
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import           Entity.ValueResponse
import           Quorum                    (threshold, majority)
import           Server.Files              (readSubState, writeSubState)
import           Server.Locks
import           Server.Paxos.LearnerState

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Functor             ((<&>))
import qualified Data.Map.Strict as M
import           Data.Maybe               (catMaybes)

data Learner m =
    Learner { learn :: !(LearnRequest -> m ValueResponseM)
            , peek  :: !(Topic -> [SequenceNum] -> IO [(SequenceNum, Val)])
            }

create :: MonadIO m => Id
                    -> Locks (Topic, SequenceNum)
                    -> IO (Learner m)
create myId topicLocks =
    pure $ Learner { learn = liftIO . learnService myId topicLocks
                   , peek  = peekImpl myId topicLocks
                   }

learnService :: Id
             -> Locks (Topic, SequenceNum)
             -> LearnRequest
             -> IO ValueResponseM
learnService myId
             topicLocks
             (LearnRequest nodes (Key topic seqNum) acceptorId value) =

    withLocked topicLocks (topic, seqNum) $ \lockedTopic ->

        getLearnerState myId lockedTopic >>= \case

            Consensus c -> pure . ValueResponseM $ Just c

            AcceptedProposals aps -> do

                let numAcceptors = length nodes
                    aps'   = M.insert acceptorId value aps
                    values = map snd . M.toList $ aps'
                    mMaj   = majority values (threshold numAcceptors)

                let learnerState =
                        case mMaj of

                            Nothing -> --No consensus yet
                                AcceptedProposals aps'

                            Just maj -> -- Consensus achieved
                                Consensus maj

                writeSubState myId lockedTopic seqNum "ls" learnerState

                pure $ ValueResponseM mMaj

-- TODO excessive locking
peekImpl :: Id
         -> Locks (Topic, SequenceNum)
         -> Topic
         -> [SequenceNum]
         -> IO [(SequenceNum, Val)]
peekImpl myId topicLocks topic seqNums =
    catMaybes <$> forConcurrently seqNums (\seqNum ->
        withLocked topicLocks (topic, seqNum) $ \lock ->
            getLearnerState myId lock <&> \case
                Consensus (Value _ _ val) -> Just (seqNum, val)
                _                         -> Nothing)

getLearnerState :: Id -> Locked (Topic, SequenceNum) -> IO LearnerState
getLearnerState myId locked@(Locked (_, seqNum)) =
    readSubState myId locked seqNum "ls" <&> \case
        Just f  -> f
        Nothing -> AcceptedProposals M.empty