{-# LANGUAGE LambdaCase #-}

module Server.Paxos.Executor where

import Client.WebNodeClient       (PingClient, pingBuilder, submitBuilder)
import Entity.Node
import Entity.SequenceNum
import Entity.Topic
import Entity.Value
import Journal                    (Journal (..))
import Requests.CreateTopic
import Requests.Join
import Requests.Ping
import Requests.Propose
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.Submit
import Server.CallbackMap         (CallbackMap (..), Notify (..))
import Server.Locks
import Server.Paxos.Proposer      (Proposer (..))

import           Control.Concurrent.STM
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either            (rights)
import           Data.Functor           ((<&>))
import           Data.Set               (Set)
import qualified Data.Set as S
import           Data.Map               (Map)
import qualified Data.Map as M
import           Data.Maybe             (fromMaybe)
import           Network.HTTP.Client    (Manager)
import           Servant                (Handler, runHandler)
import           Servant.Client         (ClientError)
import           Text.Printf            (printf)

data Executor m =
    Executor { join           :: !(JoinRequest -> m JoinResponse)
             , ping           :: !(Ping -> m Pong)
             , createTopic    :: !(CreateTopicRequest -> m CreateTopicResponse)
             , proposeProper  :: !(Topic -> Val -> IO (Either ClientError SequenceNum))
             , submit         :: !(SubmitRequest -> m SubmitResponse)
             , callback       :: !(Mode -> Topic -> SequenceNum -> Value -> IO Mode)
             , readJournal    :: !(ReadJournalRequest -> m ReadJournalResponse)
             , getSequenceNum :: !(SequenceNumRequest -> m SequenceNumResponse)
             }

data Mode = Regular
          | Catchup
              deriving Eq

data ExecutorState =
    ExecutorState { getCluster    :: !(Set Node)
                  , getLeader     :: !(Maybe Node)
                  , getSendSeqNum :: !SequenceNum
                  , getRecvSeqNum :: !SequenceNum
                  , getMode       :: !Mode
                  }

create :: MonadIO m => Node
                    -> Manager
                    -> Journal Value
                    -> Proposer Handler
                    -> CallbackMap
                    -> IO (Executor m)
create self http journal proposer callbackMap = do

    topicLocks <- newLocks

    tvTopicState <- newTVarIO M.empty

    let pingBuilder' = pingBuilder http

    let proper = proposeProperImpl http self tvTopicState callbackMap

    pure $ Executor { join           = liftIO . joinImpl tvTopicState pingBuilder' proper
                    , ping           = liftIO . pingImpl pingBuilder'
                    , createTopic    = liftIO . atomically . createTopicImpl tvTopicState
                    , proposeProper  = proper
                    , submit         = liftIO . submitImpl self proposer tvTopicState
                    , callback       = callbackImpl self callbackMap tvTopicState topicLocks journal
                    , readJournal    = liftIO . readJournalImpl topicLocks journal
                    , getSequenceNum = liftIO . getSequenceNumImpl tvTopicState
                    }

createTopicImpl :: TVar (Map Topic ExecutorState)
                -> CreateTopicRequest
                -> STM CreateTopicResponse
createTopicImpl tvTopicState (CreateTopicRequest topic nodes) = do
    topicState <- readTVar tvTopicState
    case M.lookup topic topicState of

        Just _  ->
            pure . CreateTopicResponse . Left $ printf "Topic %s already exists!\n" (show topic)

        Nothing -> do

            let fresh = ExecutorState { getCluster    = nodes
                                      , getLeader     = Nothing
                                      , getSendSeqNum = SequenceNum 0
                                      , getRecvSeqNum = SequenceNum 0
                                      , getMode       = Catchup
                                      }
            writeTVar tvTopicState (M.insert topic fresh topicState)

            CreateTopicResponse <$> right ()

proposeProperImpl :: Manager
                  -> Node
                  -> TVar (Map Topic ExecutorState)
                  -> CallbackMap
                  -> Topic
                  -> Val
                  -> IO (Either ClientError SequenceNum)
proposeProperImpl http self tvTopicState callbackMap topic value = do

    node <- atomically (getDetails self tvTopicState topic) <&>
                fromMaybe self . getLeader

    go =<< pure node
    where
    go :: Node -> IO (Either ClientError SequenceNum)
    go node = do

        u <- uniq
        submitBuilder http node (SubmitRequest topic u value) >>= \case

            Left err ->
                left err

            Right SubmitRetry ->
                go node

            Right (SubmitElsewhere leader) ->
                go leader

            Right SubmitDone ->
                waitCallback callbackMap u >>= \case
                    NotifyDone seqNum -> right seqNum
                    NotifyRetry       -> go node

submitImpl :: Node
           -> Proposer Handler
           -> TVar (Map Topic ExecutorState)
           -> SubmitRequest
           -> IO SubmitResponse
submitImpl self proposer tvTopicState (SubmitRequest topic u val) = do

    ExecutorState cluster mLeader _ _ _ <- atomically $ getDetails self tvTopicState topic

    case mLeader of

        Nothing ->

            -- No leader
            sendProposal cluster mLeader (CommandValue (ElectLeader self))

        Just leader

            -- We are the leader
            | leader == self ->
                sendProposal cluster mLeader val

            -- Someone else is the leader
            | otherwise ->
                pure $ SubmitElsewhere leader

    where
    sendProposal :: Set Node
                 -> Maybe Node
                 -> Val
                 -> IO SubmitResponse
    sendProposal cluster mLeader val' = do
        seqNum <- atomically bumpSendSeqNum
        let ch       = clusterHash mLeader cluster
            value    = Value ch u val'
            request  = ProposeRequest cluster topic seqNum value
            proposal = propose proposer request

        runHandler proposal >>= \case

            Left l ->
                error $ show l

            Right NoHighestNackRoundNo ->
                pure SubmitRetry

            Right (NotAccepted _) ->
                pure SubmitRetry

            -- Only check to see if need to resend
            -- Everything else is called-back by learner
            Right (Accepted (Value _ u' _) )

                -- Someone else's value
                | u /= u' -> pure SubmitRetry

                -- Our value
                | otherwise -> pure SubmitDone

        where
        bumpSendSeqNum :: STM SequenceNum
        bumpSendSeqNum = do
            topicState <- readTVar tvTopicState
            case M.lookup topic topicState of
                Nothing        -> error "No such topic state"
                Just execState -> do
                    let nextSeqNum = next (getSendSeqNum execState)
                    writeTVar tvTopicState $ M.insert topic (execState { getSendSeqNum = nextSeqNum }) topicState
                    pure nextSeqNum

-- cleanup
-- this function incidentally raises sendSeqNum to keep up with receiveSeqNum
callbackImpl :: Node
             -> CallbackMap
             -> TVar (Map Topic ExecutorState)
             -> Locks Topic
             -> Journal Value
             -> Mode
             -> Topic
             -> SequenceNum
             -> Value
             -> IO Mode
callbackImpl self callbackMap tvTopicState topicLocks journal callbackMode topic seqNum value = do

    (redo, mode) <- atomically $ do

        es@(ExecutorState cluster mLeader sendSeqNum recvSeqNum mode) <- getDetails self tvTopicState topic

        let recvSeqNum' = next recvSeqNum

        -- Could be bad logic
        if callbackMode == Catchup && seqNum < recvSeqNum'
            then pure (False, mode)
            else do

                -- Check/Wait on sequenceNum ordering
                check (seqNum == recvSeqNum')

                let mode' = if callbackMode == Regular
                                then Regular
                                else mode

                case value of

                    -- Abort (copied from other abort)
                    Broken -> do
                        putDetails $! es { getSendSeqNum = max recvSeqNum' sendSeqNum
                                         , getRecvSeqNum =     recvSeqNum'
                                         , getMode       = mode'
                                         }
                        pure (True, mode')

                    Value ch' _ val' ->

                        --Verify cluster hash
                        if clusterHash mLeader cluster /= ch'

                            -- Abort
                            then do
                                putDetails $! es { getSendSeqNum = max recvSeqNum' sendSeqNum
                                                 , getRecvSeqNum =     recvSeqNum'
                                                 , getMode       = mode'
                                                 }
                                pure (True, mode')

                            -- Proceed
                            else

                                case val' of

                                    CommandValue (ElectLeader leader) -> do
                                        putDetails $! es { getLeader     = Just leader
                                                         , getSendSeqNum = max recvSeqNum' sendSeqNum
                                                         , getRecvSeqNum =     recvSeqNum'
                                                         , getMode       = mode'
                                                         }
                                        pure (True, mode')

                                    CommandValue (AddNode joiner) -> do
                                        putDetails $! es { getCluster    = S.insert joiner cluster
                                                         , getSendSeqNum = max recvSeqNum' sendSeqNum
                                                         , getRecvSeqNum =     recvSeqNum'
                                                         , getMode       = mode'
                                                         }
                                        pure (False, mode')

                                    SimpleValue _ -> do
                                        putDetails $! es { getSendSeqNum = max recvSeqNum' sendSeqNum
                                                         , getRecvSeqNum =     recvSeqNum'
                                                         , getMode       = mode'
                                                         }
                                        pure (False, mode')

    withLocked topicLocks topic $ \lockedTopic ->
        writeEntries journal lockedTopic [(seqNum, value)]

    case (callbackMode, value) of
        (Catchup,           _)             -> pure ()
        (      _, Value _ u _) | redo      -> putCallback callbackMap u NotifyRetry
                               | otherwise -> putCallback callbackMap u (NotifyDone seqNum)
        (      _,      Broken)             -> pure ()

    pure mode

    where
    putDetails :: ExecutorState -> STM ()
    putDetails = modifyTVar' tvTopicState . M.insert topic

readJournalImpl :: Locks Topic
                -> Journal Value
                -> ReadJournalRequest
                -> IO ReadJournalResponse
readJournalImpl topicLocks journal (ReadJournalRequest topic seqNums) =
    withLocked topicLocks topic $ \lockedTopic ->
        readEntries journal lockedTopic seqNums <&> ReadJournalResponse

getDetails :: Node
           -> TVar (Map Topic ExecutorState)
           -> Topic
           -> STM ExecutorState
getDetails self tvTopicState topic = do
    topicState <- readTVar tvTopicState
    case M.lookup topic topicState of
        Just execState -> pure execState
        Nothing        -> error $ printf "No state for %s on %s.  Create topic first.\n" (show topic) (show self)

pingImpl :: (Node -> PingClient e)
         -> Ping
         -> IO Pong
pingImpl pingBuilder' ping =
    case ping of
        Ping -> pure Pong
        PingOther other ->
            pingBuilder' other Ping <&> \case
                Left l     -> error "fogo"
                Right Pong -> Pong

joinImpl :: TVar (Map Topic ExecutorState)
         -> (Node -> PingClient e)
         -> (Topic -> Val -> IO (Either ClientError SequenceNum))
         -> JoinRequest
         -> IO JoinResponse
joinImpl tvTopicState pingBuilder' proper (JoinPrepare topic joiner) = do
    topicState <- readTVarIO tvTopicState
    case M.lookup topic topicState of
        Nothing -> pure JoinNoSuchTopic
        Just es -> do
            let cluster = S.toList $ getCluster es
            clusterUnresponsive <- mapM (\node -> pingBuilder' node Ping) cluster <&> rights <&> \xs ->
                                       length xs < length cluster
            if clusterUnresponsive
                then pure JoinClusterNotFullyResponsive
                else do
                    printf "Host can reach rest of cluster\n"
                    joinerUnresponsive <- mapM (\node -> pingBuilder' node (PingOther joiner)) cluster <&> rights <&> \xs ->
                                              length xs < length cluster
                    if joinerUnresponsive
                        then pure JoinerNotResponsive
                        else do
                            printf "Cluster can reach joiner\n"
                            Right seqNum <- proper topic (CommandValue (AddNode joiner))
                            pure $ JoinedAt seqNum

-- TODO should this redirect and ask for leader's executorstate instead?
-- ramifications are for joiner trying to connect to a non-leader node
getSequenceNumImpl :: TVar (Map Topic ExecutorState)
                   -> SequenceNumRequest
                   -> IO SequenceNumResponse
getSequenceNumImpl tvTopicState (SequenceNumRequest topic) =
    readTVarIO tvTopicState <&> M.lookup topic
                            <&> fmap getSendSeqNum
                            <&> SequenceNumResponse

left :: Applicative m => e -> m (Either e a)
left = pure . Left

right :: Applicative m => a -> m (Either e a)
right = pure . Right
