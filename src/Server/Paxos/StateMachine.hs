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
import Entity.SubmitResponse      (Reason (..), SubmitResponse (..), fromServerError)
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
import           Control.Monad          (forM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Set               (Set, (\\))
import qualified Data.Set  as S
import           Data.Map               (Map)
import qualified Data.Map  as M
import           Data.UUID.V4           (nextRandom)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client
import           Servant                (Handler, ServerError, runHandler)
import           Servant.Client         (ClientError)

data StateMachine m =
    StateMachine { createTopic :: !(CreateTopicRequest -> m CreateTopicResponse)
                 , submit      :: !(SubmitRequest      -> m SubmitResponse)
                 , catchup     :: !(CatchupRequest     -> m CatchupResponse)
                 , peer        :: !(PeerRequest        -> m PeerResponse)
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
    journalLocks    <- newLocks
    tvMachineStates <- newTVarIO M.empty
    pure $ StateMachine { createTopic = liftIO . createTopicImpl tvMachineStates
                        , submit      = liftIO . submitImpl node proposer journalLocks journal tvMachineStates
                        , catchup     = liftIO . catchupImpl node tvMachineStates learner (peerBuilder http) journalLocks journal
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
    loop =

        atomically getDeets >>= \case

            Left l -> left l

            Right (cluster, mLeader, seqNum) -> do

                case mLeader of

                    -- Tell the caller to submit elsewhere
                    Just (leader, _) | leader /= node ->
                        pure . SubmitResponse $ Left (SubmitElsewhere leader)

                    -- Submit here
                    _ -> do

                        let key      = Key topic seqNum
                        u <- show <$> nextRandom
                        let value    = Value node u val
                        let proposal = ProposeRequest cluster key value

                        doProposal proposal >>= \case

                            Left serverError -> left $ fromServerError serverError

                            Right (NotAccepted _) -> loop

                            Right NoHighestNackRoundNo -> loop

                            Right (Accepted (Value sender u' val')) -> do

                                handleMessage sender seqNum val'

                                if u' == u

                                    -- Our message. Done.
                                    then pure . SubmitResponse $ Right (seqNum, val')

                                    -- Not our message, try again
                                    else loop

        where
        left = pure . SubmitResponse . Left

        getDeets :: STM (Either Reason (Set Node, Maybe (Node, SequenceNum), SequenceNum))
        getDeets = do
            machineStates <- readTVar tvMachineStates
            case M.lookup topic machineStates of
                Nothing -> pure . Left $ NotDefined topic
                Just (MachineState cluster mLeader seqNum) -> do
                    let seqNum' = next seqNum
                    writeTVar tvMachineStates (M.insert topic (MachineState cluster mLeader seqNum') machineStates)
                    pure $ Right (cluster, mLeader, seqNum')

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

catchupImpl :: Node
            -> TVar (Map Topic MachineState)
            -> Learner m
            -> (Node -> PeerClient ClientError)
            -> Locks Topic
            -> Journal
            -> CatchupRequest
            -> IO CatchupResponse
catchupImpl node tvMachineStates learner peerer journalLocks journal (CatchupRequest topic seqNums) =

    (M.lookup topic <$> readTVarIO tvMachineStates) >>= \case

        Nothing -> pure $ ErrNoSuchTopic topic

        Just machineState -> do

            let otherNodes = filter (/=node) . S.toList . getCluster $ machineState

            let seqNumsSet = S.fromList seqNums

            -- Check own paxos logs for the values
            learnedLocally <- S.fromList <$> peek learner topic seqNums

            let nonLocal = seqNumsSet \\ S.map fst learnedLocally

            -- Ask peers for values
            learnedFromPeers <- if null nonLocal
                                    then pure mempty
                                    else do
                                        xs <- forM otherNodes $ \other -> do
                                            Right (PeerResponse x) <- peerer other (PeerRequest topic seqNums)
                                            pure $! S.fromList x
                                        pure $ mconcat xs

            let learned = learnedLocally <> learnedFromPeers

            -- Record if any new values were learned
            unless (null learned) $
                withLocked journalLocks topic $ \lockedTopic ->
                    writeEntries journal lockedTopic $ S.toList learned

            case S.toList (seqNumsSet \\ S.map fst learned) of
                [] -> pure CaughtUp
                sm -> pure $ StillMissing sm

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

