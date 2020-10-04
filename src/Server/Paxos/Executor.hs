{-# LANGUAGE LambdaCase #-}

module Server.Paxos.Executor where

import Client.WebNodeClient
import Entity.Node
import Entity.SequenceNum
import Entity.Topic
import Entity.Value
import Journal                    (Journal (..))
import Requests.Catchup
import Requests.CreateTopic
import Requests.Join
import Requests.JoinCluster
import Requests.Ping
import Requests.Propose
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.SubmitCluster
import Requests.SubmitNode
import Server.CallbackMap         (CallbackMap (..), Notify (..))
import Server.Locks
import Server.Paxos.Proposer      (Proposer (..))

import           Control.Concurrent.STM
import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either            (rights)
import           Data.Functor           ((<&>))
import           Data.List.Split        (chunksOf)
import           Data.Set               (Set)
import qualified Data.Set as S
import           Data.Map               (Map)
import qualified Data.Map as M
import           Data.Maybe             (fromMaybe)
import           Network.HTTP.Client    (Manager)
import           Text.Printf            (printf)

data Executor m =
    Executor { join           :: !(JoinRequest -> m JoinResponse)
             , joinCluster    :: !(JoinClusterRequest -> m JoinClusterResponse)
             , catchup        :: !(CatchupRequest -> m CatchupResponse)
             , ping           :: !(Ping -> m Pong)
             , createTopic    :: !(CreateTopicRequest -> m CreateTopicResponse)
             , submitCluster  :: !(SubmitClusterRequest -> m SubmitClusterResponse)
             , submitNode     :: !(SubmitNodeRequest -> m SubmitNodeResponse)
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
                    -> Proposer
                    -> CallbackMap
                    -> IO (Executor m)
create self http journal proposer callbackMap = do

    topicLocks <- newLocks

    tvTopicState <- newTVarIO M.empty

    let pingBuilder' = pingBuilder http

    let sc = submitClusterImpl http self tvTopicState callbackMap

    let cb = callbackImpl self callbackMap tvTopicState topicLocks journal

    pure $ Executor { join           = liftIO . joinImpl tvTopicState pingBuilder' sc
                    , joinCluster    = liftIO . joinClusterImpl self http cb
                    , catchup        = liftIO . catchupImpl http cb
                    , ping           = liftIO . pingImpl pingBuilder'
                    , createTopic    = liftIO . atomically . createTopicImpl tvTopicState
                    , submitCluster  = liftIO . sc
                    , submitNode     = liftIO . submitNodeImpl self proposer tvTopicState
                    , callback       = cb
                    , readJournal    = liftIO . readJournalImpl topicLocks journal
                    , getSequenceNum = liftIO . getSequenceNumImpl tvTopicState
                    }

-- CATCHUP PHASE
-- remember to set catchup mode
catchupImpl :: Manager
            -> (Mode -> Topic -> SequenceNum -> Value -> IO Mode)
            -> CatchupRequest
            -> IO CatchupResponse
catchupImpl http cb (CatchupRequest host topic) = do

    -- Ask host what sequence number it's up to
    Right (SequenceNumResponse (Just (SequenceNum sn))) <- sequenceNumBuilder http host (SequenceNumRequest topic)
    let gatherRanges = chunksOf 20 [1..sn]
    printf "pre-gathering %s\n" (show gatherRanges)
    -- TODO check all were actually fetched
    forM_ gatherRanges $ \gatherRange ->
        readJournalBuilder http host (ReadJournalRequest topic (map SequenceNum gatherRange)) >>= \case
            Left e -> error $ show e
            Right (ReadJournalResponse responses) ->
                forM_ responses $ \(s',v') ->
                    cb Catchup topic s' v'
    pure CatchupResponse

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

submitClusterImpl :: Manager
                  -> Node
                  -> TVar (Map Topic ExecutorState)
                  -> CallbackMap
                  -> SubmitClusterRequest
                  -> IO SubmitClusterResponse
submitClusterImpl http self tvTopicState callbackMap (SubmitClusterRequest topic value) = do

    node <- atomically (getDetails self tvTopicState topic) <&>
                fromMaybe self . getLeader

    go =<< pure node
    where
    go :: Node -> IO SubmitClusterResponse
    go node = do

        u <- uniq

        submitNodeBuilder http node (SubmitNodeRequest topic u value) >>= \case

            Left err ->
                pure . SubmitClusterResponse . Left $ printf "Could not submit to node %s: %s" (show node) (show err)

            Right SubmitRetry ->
                go node

            Right (SubmitElsewhere leader) ->
                go leader

            Right SubmitDone ->
                waitCallback callbackMap u >>= \case
                    NotifyDone seqNum -> pure . SubmitClusterResponse $ Right seqNum
                    NotifyRetry       -> go node

submitNodeImpl :: Node
               -> Proposer
               -> TVar (Map Topic ExecutorState)
               -> SubmitNodeRequest
               -> IO SubmitNodeResponse
submitNodeImpl self proposer tvTopicState (SubmitNodeRequest topic u val) = do

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
                 -> IO SubmitNodeResponse
    sendProposal cluster mLeader val' = do
        seqNum <- atomically bumpSendSeqNum
        let ch       = clusterHash mLeader cluster
            value    = Value ch u val'
            request  = ProposeRequest cluster topic seqNum value

        propose proposer request >>= \case

            NoHighestNackRoundNo ->
                pure SubmitRetry

            (NotAccepted _) ->
                pure SubmitRetry

            -- Only check to see if need to resend
            -- Everything else is called-back by learner
            (Accepted (Value _ u' _) )

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
         -> (SubmitClusterRequest -> IO SubmitClusterResponse)
         -> JoinRequest
         -> IO JoinResponse
joinImpl tvTopicState pingBuilder' submitCluster (JoinPrepare topic joiner) = do
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
                            SubmitClusterResponse (Right seqNum) <- submitCluster (SubmitClusterRequest topic (CommandValue (AddNode joiner)))
                            pure $ JoinedAt seqNum

joinClusterImpl :: Node
                -> Manager
                -> (Mode -> Topic -> SequenceNum -> Value -> IO Mode)
                -> JoinClusterRequest
                -> IO JoinClusterResponse
joinClusterImpl self http cb (JoinClusterRequest host topic) = do

    -- Tell host to Propose that newNode is now part of the cluster
    joinBuilder http host (JoinPrepare topic self) >>= \case
        Left l -> error $ show l
        Right (JoinedAt (SequenceNum _end)) -> do

            -- Ask new node what sequence number it's up to
            -- TODO - shouldn't need to http becaues asking self
            sequenceNumBuilder http self (SequenceNumRequest topic) >>= \case
                Left l -> error $ show l
                Right (SequenceNumResponse (Just (SequenceNum start))) ->
                    -- TODO: optimise for more than one-at-a-time
                    let loop sn =

                            -- Ask host for the next value
                            readJournalBuilder http host (ReadJournalRequest topic [SequenceNum sn]) >>= \case
                                Left e -> error $ show e
                                Right (ReadJournalResponse responses) ->
                                    forM_ responses $ \(s',v') -> do

                                        -- Journal the asked value
                                        mode <- cb Catchup topic s' v'
                                        when (mode == Catchup) (loop $ sn + 1)
                    in loop (start + 1)
            pure JoinClusterResponse

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
