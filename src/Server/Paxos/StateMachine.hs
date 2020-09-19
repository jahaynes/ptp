{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase,
             ScopedTypeVariables #-}

module Server.Paxos.StateMachine ( StateMachine (..)
                                 , create
                                 ) where

import Client.WebNodeClient       (PeerClient, peerBuilder)
import Entity.CatchupRequest      (CatchupRequest (..))
import Entity.CatchupResponse     (CatchupResponse (..))
import Entity.CreateTopicRequest  (CreateTopicRequest (..))
import Entity.CreateTopicResponse (CreateTopicResponse (..))
import Entity.Key                 (Key (..))
import Entity.PeerRequest         (PeerRequest (..))
import Entity.PeerResponse        (PeerResponse (..))
import Entity.ProposeRequest      (ProposeRequest (..))
import Entity.ProposeResponse     (ProposeResponse (..))
import Entity.SequenceNum         (SequenceNum (..), next)
import Entity.SubmitRequest       (SubmitRequest (..))
import Entity.SubmitResponse      (Reason (..), SubmitResponse (..))
import Entity.Topic               (Topic (..))
import Entity.Value               (Value (..), Val (..))
import Journal                    (Journal (..))
import Node                       (Node (..))
import Server.Locks               (Locks, newLocks, withLocked)
import Server.Paxos.Proposer      (Proposer (..))
import Server.Paxos.Learner       (Learner (..))

import           Codec.Serialise        (Serialise)
import           Control.Concurrent.STM
import           Control.DeepSeq        (NFData)
import           Control.Monad          (forM, forM_, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.List              ((\\))
import           Data.Set               (Set, (\\))
import qualified Data.Set  as S
import           Data.Map               (Map)
import qualified Data.Map  as M
import           Data.UUID.V4           (nextRandom)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client
import           Servant                (Handler, ServerError, runHandler)
import           Servant.Client         (ClientError)
import           Text.Printf            (printf)

data StateMachine m =
    StateMachine { createTopic :: !(CreateTopicRequest -> m CreateTopicResponse)
                 , submit      :: !(SubmitRequest -> m SubmitResponse)
                 , catchup     :: !(CatchupRequest -> m CatchupResponse)
                 , peer        :: !(PeerRequest -> m PeerResponse)
                 , dump        :: !(Topic -> ((SequenceNum, Val) -> IO ()) -> IO ())
                 }

data MachineState =
    MachineState { getCluster  :: !(Set Node)
                 , getLeaderAt :: !(Maybe (Node, SequenceNum))
                 , getSeqNum   :: !SequenceNum
                 } deriving (Generic, Serialise, NFData)

create :: MonadIO m => Node
                    -> Manager
                    -> Journal
                    -> Proposer Handler
                    -> Learner Handler
                    -> IO (StateMachine m)
create node http journal proposer learner = do

    journalLocks <- newLocks

    tvMachineStates <- newTVarIO M.empty

    let peerer = peerBuilder http

    pure $ StateMachine { createTopic = liftIO . createTopicImpl tvMachineStates
                        , submit      = liftIO . submitImpl node proposer journalLocks journal tvMachineStates
                        , catchup     = liftIO . catchupImpl tvMachineStates learner peerer journalLocks journal
                        , peer        = liftIO . _peerImpl journalLocks journal
                        , dump        = dumpImpl journalLocks journal
                        }

-- TODO catch deep
-- reject if already exist
createTopicImpl :: TVar (Map Topic MachineState)
                -> CreateTopicRequest
                -> IO CreateTopicResponse
createTopicImpl tvMachineStates (CreateTopicRequest nodes topic) = do
    atomically . modifyTVar' tvMachineStates
               . M.insert topic
               $ MachineState { getCluster  = nodes
                              , getLeaderAt = Nothing
                              , getSeqNum   = SequenceNum 0
                              }
    pure CreateTopicResponse

data Retry = Retry SequenceNum Val
                deriving (Generic, NFData)

submitImpl :: Node
           -> Proposer Handler
           -> Locks Topic
           -> Journal
           -> TVar (Map Topic MachineState)
           -> SubmitRequest
           -> IO SubmitResponse
submitImpl node proposer journalLocks journal tvMachineStates (SubmitRequest topic val) = loop

    where
    loop = do

        (cluster, mLeader, seqNum) <- atomically getDeets

        case mLeader of

            -- Tell the caller to submit elsewhere
            Just (leader, _) | leader /= node ->
                pure . SubmitResponse $ Left (SubmitElsewhere leader)

            _ -> do

                u <- show <$> nextRandom

                let key      = Key topic seqNum
                let value    = Value node u val
                let proposal = ProposeRequest cluster key value

                doProposal proposal >>= \case

                    Left l -> do error $ "FOo" ++ show l -- printf "Some Error %s\n" (show l)

                    Right (NotAccepted _) -> do -- printf "Not Accepted: %s %s %s\n" (show x) (show seqNum) (show value)
                                                loop

                    Right NoHighestNackRoundNo -> do -- printf "No Highest Nack: %s %s\n" (show seqNum) (show value)
                                                     loop

                    Right (Accepted (Value sender u' val')) -> do

                        handleMessage sender seqNum val'

                        if u' == u

                            -- Our message. Done.
                            then pure . SubmitResponse $ Right (seqNum, val')

                            -- Not our message, try again
                            else loop

        where
        getDeets :: STM (Set Node, Maybe (Node, SequenceNum), SequenceNum)
        getDeets = do
            machineStates <- readTVar tvMachineStates
            case M.lookup topic machineStates of
                Nothing -> error "No machine state"
                Just (MachineState cluster mLeader seqNum) -> do
                    let seqNum' = next seqNum
                    writeTVar tvMachineStates (M.insert topic (MachineState cluster mLeader seqNum') machineStates)
                    pure (cluster, mLeader, seqNum')

        doProposal :: ProposeRequest -> IO (Either ServerError ProposeResponse)
        doProposal = runHandler . propose proposer

        handleMessage :: Node -> SequenceNum -> Val -> IO ()
        handleMessage sender seqNum' val' = do

            atomically $ do

                machineStates <- readTVar tvMachineStates

                let Just (MachineState cluster mLeader seqNum) = M.lookup topic machineStates

                let leader' = case mLeader of
                                  Nothing                         -> (sender, seqNum')
                                  Just x@(_, lsn) | seqNum' > lsn -> (sender, seqNum')
                                                  | otherwise     -> x

                let seqNum'' = max seqNum seqNum'

                writeTVar tvMachineStates $ M.insert topic (MachineState cluster (Just leader') seqNum'') machineStates

            withLocked journalLocks topic $ \lockedTopic ->
                writeEntries journal lockedTopic [(seqNum', val')]

catchupImpl :: TVar (Map Topic MachineState)
            -> Learner m
            -> (Node -> PeerClient ClientError)
            -> Locks Topic
            -> Journal
            -> CatchupRequest
            -> IO CatchupResponse
catchupImpl tvMachineStates learner peerer journalLocks journal (CatchupRequest topic seqNums) = do

    -- 1) First pass - check own paxos logs for the values
    peeked <- S.fromList <$> peek learner topic seqNums

    let nonLocal = S.fromList seqNums \\ S.map fst peeked

    fromPeers <- if null nonLocal

                     then pure mempty

                     else do
                         printf "STILL MISSING: %s\n" (show nonLocal)
                         Just machineState <- M.lookup topic <$> readTVarIO tvMachineStates
                         xs <- forM (S.toList . getCluster $ machineState) $ \other -> do
                             Right (PeerResponse x) <- peerer other (PeerRequest topic seqNums)
                             pure x
                         pure $ mconcat xs

    -- 2) TODO (unless null)...
    --    write out discovered values
    withLocked journalLocks topic $ \lockedTopic ->
        writeEntries journal lockedTopic $ S.toList peeked

    -- 4) TODO check if all requesteds were responded
    pure . CatchupResponse . S.toList $ peeked

_peerImpl :: Locks Topic
          -> Journal
          -> PeerRequest
          -> IO PeerResponse
_peerImpl journalLocks journal (PeerRequest topic seqNums) =
    withLocked journalLocks topic $ \lockedTopic ->
        PeerResponse <$> readEntries journal lockedTopic seqNums

dumpImpl :: Locks Topic -> Journal -> Topic -> ((SequenceNum, Val) -> IO ()) -> IO ()
dumpImpl machineTopicLocks journal topic f =
    withLocked machineTopicLocks topic $ \lockedTopic ->
        dumpJournal journal lockedTopic f

