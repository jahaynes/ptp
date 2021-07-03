{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase #-}

module Server.Paxos.Learner ( Learner (..)
                            , LearnerState
                            , create
                            ) where

import           Entity.Id
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.TopicSeqnum
import           Entity.Value
import           Quorum              (threshold, majority)
import           Requests.Learn      (LearnRequest (..), LearnResponse (..))
import           Requests.Peek       (PeekRequest (..), PeekResponse (..))
import           Server.Locks        (Locked (..), Locks, newLocks, withLocked)
import           Storage

import           Codec.Serialise            (Serialise, deserialise, serialise)
import           Control.Exception.Safe     (catchAnyDeep)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           RIO hiding (catchAnyDeep) -- TODO resolve
import           RIO.ByteString.Lazy        (fromStrict, toStrict)
import qualified RIO.Map as M
import           Text.Printf                (printf)

data Learner m =
    Learner { learn :: !(LearnRequest -> m LearnResponse)
            , peek  :: !(PeekRequest  -> m PeekResponse)
            }

--todo dedupe
newtype Locked2 a =
    Locked2 a

instance Lock (Locked2 a)

instance StoreValue LearnerState where
    toValBytes = toStrict . serialise
    fromValBytes = deserialise . fromStrict

data LearnerState = AcceptedProposals !(Map Id Value)
                  | Consensus !Value
                      deriving (Generic, NFData, Serialise)

create :: (LockedStorage s, MonadIO m) => s
                                       -> (Topic -> SequenceNum -> Value -> IO ())
                                       -> IO (Learner m)
create storage callback = do
    topicLocks <- newLocks
    let callback' topic seqNum value = catchAnyDeep (liftIO $ callback topic seqNum value) throwE
    pure $ Learner { learn = liftIO . learnService storage topicLocks callback'
                   , peek  = liftIO . peekService storage topicLocks
                   }
 
learnService :: LockedStorage s => s 
                                -> Locks TopicSeqnum
                                -> (Topic -> SequenceNum -> Value -> ExceptT SomeException IO ())
                                -> LearnRequest
                                -> IO LearnResponse
learnService store topicLocks callback (LearnRequest nodes topic seqNum acceptorId value) =
    runExceptT (runLearn $ TopicSeqnum topic seqNum) <&> \case
        Left  ioex -> LearnResponseError $ printf "Learn failed: " (show ioex)
        Right mVal -> LearnResponse mVal

    where
    runLearn :: TopicSeqnum -> ExceptT SomeException IO (Maybe Value)
    runLearn topicSeqnum =

        withLocked topicLocks topicSeqnum $ \lockedTopicSeqnum ->

            getLearnerState lockedTopicSeqnum >>= \case

                Consensus c -> pure $ Just c

                AcceptedProposals aps -> do

                    let numAcceptors = length nodes
                        aps'   = M.insert acceptorId value aps
                        values = map snd . M.toList $ aps'
                        mMaj   = majority values (threshold numAcceptors)

                    let learnerState =

                            case mMaj of

                                --No consensus yet
                                Nothing -> AcceptedProposals aps'

                                -- Consensus achieved
                                Just maj -> Consensus maj

                    writeStoreL (Locked2 topicSeqnum) store topicSeqnum $! learnerState

                    case mMaj of
                        Nothing  -> pure ()
                        Just maj -> callback topic seqNum maj

                    pure mMaj

        where
        getLearnerState :: Locked TopicSeqnum -> ExceptT SomeException IO LearnerState
        getLearnerState (Locked tsn) =
            readStoreL (Locked2 tsn) store tsn <&> \case
                Just f  -> f
                Nothing -> AcceptedProposals M.empty

peekService :: LockedStorage s => s
                               -> Locks TopicSeqnum
                               -> PeekRequest
                               -> IO PeekResponse
peekService store topicLocks (PeekRequest topic seqNums) =

    -- TODO: too granular
    runExceptT (catMaybes <$> mapM runPeek seqNums) <&> \case

        Left x     -> PeekResponseError (show x)

        Right vals -> PeekResponse $ M.fromList vals

    where
    runPeek :: SequenceNum -> ExceptT SomeException IO (Maybe (SequenceNum, Value))
    runPeek seqNum =
        let topicSeqnum = TopicSeqnum topic seqNum
        in
        withLocked topicLocks topicSeqnum $ \_ ->
            readStoreL (Locked2 topicSeqnum) store topicSeqnum <&> \case
                Just (Consensus c) -> pure (seqNum, c)
                _                  -> Nothing