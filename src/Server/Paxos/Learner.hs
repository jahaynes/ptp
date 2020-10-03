{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase      #-}

module Server.Paxos.Learner ( Learner (..)
                            , create
                            ) where

import           Entity.Id
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import           Quorum             (threshold, majority)
import           Requests.Learn
import           Server.Files       (readSubState, writeSubState)
import           Server.Locks

import           Codec.Serialise        (Serialise)
import           Control.DeepSeq        (NFData)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Functor           ((<&>))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics           (Generic)

-- import           Text.Printf              (printf)

newtype Learner m =
    Learner { learn :: LearnRequest -> m LearnResponse
            }

data LearnerState = AcceptedProposals !(Map Id Value)
                  | Consensus !Value
                      deriving (Generic, NFData, Serialise)

create :: MonadIO m => Id
                    -> Locks (Topic, SequenceNum)
                    -> (Topic -> SequenceNum -> Value -> IO a)
                    -> IO (Learner m)
create myId topicLocks callback =
    pure $ Learner { learn = liftIO . learnService myId topicLocks callback }

learnService :: Id
             -> Locks (Topic, SequenceNum)
             -> (Topic -> SequenceNum -> Value -> IO a)
             -> LearnRequest
             -> IO LearnResponse
learnService myId
             topicLocks
             callback
             (LearnRequest nodes topic seqNum acceptorId value) = do

    mMaj <- withLocked topicLocks (topic, seqNum) $ \lockedTopic ->

        getLearnerState lockedTopic >>= \case

            Consensus c -> pure $ Just c

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

                case mMaj of
                    Nothing  -> pure ()
                    Just maj -> do
                        -- printf "Learned %s->%s\n" (show seqNum) (show maj)
                        _ <- callback topic seqNum maj
                        pure ()

                pure mMaj

    pure $ LearnResponse mMaj

    where
    getLearnerState :: Locked (Topic, SequenceNum) -> IO LearnerState
    getLearnerState locked@(Locked (_, seqNum')) =
        readSubState myId locked seqNum' "ls" <&> \case
            Just f  -> f
            Nothing -> AcceptedProposals M.empty