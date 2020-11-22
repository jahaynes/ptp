{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             FlexibleContexts,
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
import           Server.Locks
import           Server.Storage         (Storage)
import qualified Server.Storage as S

import           Codec.Serialise            (Serialise)
import           Control.Concurrent.Async   (forConcurrently)
import           Control.DeepSeq            (NFData)
import           Control.Exception          (SomeException)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           GHC.Generics               (Generic)
import           Data.Either                (rights)
import           Data.Functor               ((<&>))
import           Data.Maybe                 (catMaybes, fromJust)
import qualified Data.Set as S
import           Text.Printf                (printf)

data Acceptor m =
    Acceptor { prepare :: !(PrepareRequest -> m PrepareResponse)
             , accept  :: !(AcceptRequest  -> m AcceptResponse)
             }

data AcceptorState =
    AcceptorState { acc_notLessThan :: !(Maybe ProposalNumber)
                  , acc_proposal    :: !(Maybe Proposal)
                  } deriving (Generic, NFData, Serialise)

create :: (Storage s AcceptorState, MonadIO m, Show e) => Id
                                         -> s
                                         -> (Node -> LearnClient e)
                                         -> IO (Acceptor m)
create myId storage learnBuilder = do

    topicLocks <- newLocks

    pure $ Acceptor { prepare = liftIO . prepareService storage topicLocks
                    , accept  = liftIO . acceptService myId storage topicLocks learnBuilder
                    }

prepareService :: Storage s AcceptorState => s
                            -> Locks (Topic, SequenceNum)
                            -> PrepareRequest
                            -> IO PrepareResponse
prepareService storage topicLocks (PrepareRequest topic seqNum n) =
    runExceptT runPrepare <&> \case
        Left ioex -> PrepareResponseError $ printf "Prepare failed: " (show ioex)
        Right enp -> PrepareResponse enp

    where
    runPrepare :: ExceptT SomeException IO (Either Nack Promise)
    runPrepare =
        withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
            state <- getAcceptorSubState storage lockedTopic
            if Just n > acc_notLessThan state
                then do
                    S.writeTopicSequence storage lockedTopic $! state {acc_notLessThan = Just n}
                    right $ Promise n (acc_proposal state)
                else left . Nack . fromJust . acc_notLessThan $ state

acceptService :: (Storage s AcceptorState, Show e) => Id
                                                   -> s
                                                   -> Locks (Topic, SequenceNum)
                                                   -> (Node -> LearnClient e)
                                                   -> AcceptRequest
                                                   -> IO AcceptResponse
acceptService myId storage topicLocks learnBuilder (AcceptRequest nodes topic seqNum n v) =
    runExceptT runAccept <&> \case
        Left ioex -> AcceptResponseError $ printf "Accept failed: " (show ioex)
        Right esv -> AcceptResponse esv

    where
    runAccept :: ExceptT SomeException IO (Either String Value)
    runAccept = do
        accepted <- withLocked topicLocks (topic, seqNum) $ \lockedTopic -> do
            state <- getAcceptorSubState storage lockedTopic
            if Just n >= acc_notLessThan state
                then do
                    S.writeTopicSequence storage lockedTopic $ state { acc_notLessThan = Just n
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

getAcceptorSubState :: Storage s AcceptorState => s
                                               -> Locked (Topic, SequenceNum)
                                               -> ExceptT SomeException IO AcceptorState
getAcceptorSubState storage locked =
    S.readTopicSequence storage locked <&> \case
        Just f  -> f
        Nothing -> AcceptorState Nothing Nothing

left :: Applicative m => a -> m (Either a b)
left = pure . Left

right :: Applicative m => b -> m (Either a b)
right = pure . Right