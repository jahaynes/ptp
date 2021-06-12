{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             FlexibleContexts,
             FlexibleInstances,     -- TODO get rid of while unorphaning key (topic,seqnum)
             LambdaCase        #-}

module Server.Paxos.Acceptor ( Acceptor (..)
                             , AcceptorState
                             , create
                             ) where

import           Client.PaxosClient    (LearnClient)
import           Entity.Id
import           Entity.Node
import           Entity.Proposal
import           Entity.ProposalNumber
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import           Requests.Accept
import           Requests.Prepare
import           Requests.Learn
import           Storage
import           Server.Locks

import           Codec.Serialise            (Serialise, deserialise, serialise)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Maybe                 (fromJust)
import           RIO
import           RIO.ByteString.Lazy        (fromStrict, toStrict)
import qualified RIO.Set as S
import           Text.Printf                (printf)

data Acceptor m =
    Acceptor { prepare :: !(PrepareRequest -> m PrepareResponse)
             , accept  :: !(AcceptRequest  -> m AcceptResponse)
             }

data AcceptorState =
    AcceptorState { acc_notLessThan :: !(Maybe ProposalNumber)
                  , acc_proposal    :: !(Maybe Proposal)
                  } deriving (Generic, NFData, Serialise)

--todo dedupe
newtype Locked2 a =
    Locked2 a

instance Lock (Locked2 a)

-- dedupe
-- TODO use a builder / make human readable?
instance Key (Topic, SequenceNum) where
    toKeyBytes (Topic topic, SequenceNum sn) =
        toStrict . serialise $ (topic, sn)

instance StoreValue AcceptorState where
    toValBytes = toStrict . serialise
    fromValBytes = deserialise . fromStrict

create :: (MonadIO m, Show e, LockedStorage s) => Id
                                               -> s
                                               -> (Node -> LearnClient e)
                                               -> IO (Acceptor m)
create myId store learnBuilder = do

    topicLocks <- newLocks

    pure $ Acceptor { prepare = liftIO . prepareService store topicLocks
                    , accept  = liftIO . acceptService myId store topicLocks learnBuilder
                    }

prepareService :: LockedStorage s => s 
                                  -> Locks (Topic, SequenceNum)
                                  -> PrepareRequest
                                  -> IO PrepareResponse
prepareService store topicLocks (PrepareRequest topic seqNum n) =
    runExceptT runPrepare <&> \case
        Left ioex -> PrepareResponseError $ printf "Prepare failed: " (show ioex)
        Right enp -> PrepareResponse enp

    where
    runPrepare :: ExceptT SomeException IO (Either Nack Promise)
    runPrepare =
        withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
            state <- getAcceptorSubState store lockedTopic
            if Just n > acc_notLessThan state
                then do
                    writeStoreL (Locked2 lockedTopic) store (topic, seqNum) $! state {acc_notLessThan = Just n}
                    right $ Promise n (acc_proposal state)
                else left . Nack . fromJust . acc_notLessThan $ state

acceptService :: (Show e, LockedStorage s) => Id
                                           -> s
                                           -> Locks (Topic, SequenceNum)
                                           -> (Node -> LearnClient e)
                                           -> AcceptRequest
                                           -> IO AcceptResponse
acceptService myId store topicLocks learnBuilder (AcceptRequest nodes topic seqNum n v) =
    runExceptT runAccept <&> \case
        Left ioex -> AcceptResponseError $ printf "Accept failed: " (show ioex)
        Right esv -> AcceptResponse esv

    where
    runAccept :: ExceptT SomeException IO (Either String Value)
    runAccept = do
        accepted <- withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
            state <- getAcceptorSubState store lockedTopic
            if Just n >= acc_notLessThan state
                then do
                    writeStoreL (Locked2 lockedTopic) store (topic, seqNum) $! state { acc_notLessThan = Just n
                                                                                     , acc_proposal    = Just (Proposal n v) }
                    pure True
                else pure False

        if accepted

            then do

                -- Inform every (responsive) learner
                let inform = rights
                           . map (fmap (\(LearnResponse mv) -> mv))
                         <$> forConcurrently (map learnBuilder $ S.toList nodes) (\c -> c (LearnRequest nodes topic seqNum myId v))

                learnerResponses <- liftIO inform

                -- Consider every consensus claimed by a learner
                case catMaybes learnerResponses of

                    [] -> left "No consensus (yet)"

                    -- Sanity check
                    (c:cs) | all (==c) cs -> right c

                           | otherwise    -> left "Fatal: Inconsistent consensus"

            else left "acceptService: Not accepted"

getAcceptorSubState :: LockedStorage s => s
                                       -> Locked (Topic, SequenceNum)
                                       -> ExceptT SomeException IO AcceptorState
getAcceptorSubState store (Locked (topic, seqNum)) =
    readStoreL (Locked2 (topic, seqNum)) store (topic, seqNum) <&> \case
        Just f  -> f
        Nothing -> AcceptorState Nothing Nothing

left :: Applicative m => a -> m (Either a b)
left = pure . Left

right :: Applicative m => b -> m (Either a b)
right = pure . Right