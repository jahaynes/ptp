{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase,
             ScopedTypeVariables #-}

module Server.Paxos.StateMachine ( StateMachine (..)
                                 , create
                                 ) where

import Client.WebNodeClient       (PeerClient, peerBuilder)
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
import Entity.Value               (Command (..), Value (..), Val (..))
import Journal                    (Journal (..))
import Node                       (Node (..))
import Server.Files               (Machine (..), readTopicDetail, writeTopicDetail)
import Server.Locks               (Locked (..), Locks, newLocks, withLocked)
import Server.Paxos.Proposer      (Proposer (..))

import Codec.Serialise        (Serialise)
import Control.DeepSeq        (NFData)
import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set               (Set)
import Data.UUID.V4           (nextRandom)
import GHC.Generics           (Generic)
import Network.HTTP.Client
import Servant                (Handler, ServerError, runHandler)
import Servant.Client         (ClientError)

data StateMachine m =
    StateMachine { createTopic :: !(CreateTopicRequest -> m CreateTopicResponse)
                 , peer        :: !(PeerRequest -> m PeerResponse)
                 , submit      :: !(SubmitRequest -> m SubmitResponse)
                 , dump        :: !(Topic -> ((SequenceNum, Val) -> IO ()) -> IO ())
                 }

data MachineState =
    MachineState { cluster    :: !(Set Node)
                 , getLeader  :: !(Maybe Node)
                 , lastSeqNum :: !SequenceNum
                 } deriving (Generic, Serialise, NFData)

create :: MonadIO m => Node
                    -> Journal
                    -> Proposer Handler
                    -> IO (StateMachine m)
create n@(Node ident _) journal proposer = do

    topicLocks        <- newLocks
    machineTopicLocks <- newLocks

    let writeMachineImpl topic ms =
            withLocked machineTopicLocks (topic, Machine) $ \lockedTopicMachine ->
                writeTopicDetail ident lockedTopicMachine "ms" ms

    http <- newManager $ defaultManagerSettings
        { managerResponseTimeout = responseTimeoutMicro 10000000 }

    pure $ StateMachine { createTopic = liftIO . createTopicImpl writeMachineImpl
                        , peer        = liftIO . peerImpl topicLocks journal
                        , submit      = liftIO . submitImpl n proposer (peerBuilder http) topicLocks machineTopicLocks journal
                        , dump        = dumpImpl topicLocks journal
                        }

-- TODO catch deep
createTopicImpl :: (Topic -> MachineState -> IO ())
                -> CreateTopicRequest
                -> IO CreateTopicResponse
createTopicImpl writeMachine (CreateTopicRequest nodes topic) = do
    writeMachine topic $! MachineState { cluster    = nodes
                                       , getLeader  = Nothing
                                       , lastSeqNum = SequenceNum 1
                                       }
    pure CreateTopicResponse

data Retry = Retry (Maybe Node) SequenceNum Val
                deriving (Generic, NFData)

peerImpl :: Locks Topic
         -> Journal
         -> PeerRequest
         -> IO PeerResponse
peerImpl topicLocks journal (PeerRequest topic seqNums) =
    withLocked topicLocks topic $ \lockedTopic ->
        PeerResponse <$> readEntries journal lockedTopic seqNums

submitImpl :: Node
           -> Proposer Handler
           -> (Node -> PeerClient ClientError)
           -> Locks Topic
           -> Locks (Topic, Machine)
           -> Journal
           -> SubmitRequest
           -> IO SubmitResponse
submitImpl node@(Node ident _) proposer peerer topicLocks machineLocks journal (SubmitRequest topic val) = loop
    where
    loop = withLocked machineLocks (topic, Machine) submitImpl2 >>= \case

               -- TODO even fallbackier - tell the CALLER to ask the leader instead
               Left (Retry Nothing s v) -> do withLocked topicLocks topic (writeJournal s v)
                                              loop

               Left (Retry (Just leader) s v) -> do

                   withLocked topicLocks topic (writeJournal s v)

                   -- Not not the leader, keep polling the leader
                   unless (leader == node) $
                       let gallop (SequenceNum sn) = do
                               let sns = map SequenceNum [(sn+1) .. (sn+10)]
                               Right (PeerResponse pr) <- peerer leader (PeerRequest topic sns)
                               unless (null pr) $ do
                                   withLocked topicLocks topic (writeJournals pr)
                                   gallop (SequenceNum $ sn + 10)
                       in gallop s

                   -- Still haven't written our own value yet
                   loop

               Right r@(SubmitResponse e) ->

                   case e of

                       Left (NotDefined (Topic t)) -> error $ "Not defined: " ++ t

                       Left NotAcceptedR -> loop

                       Right (s, v) -> do withLocked topicLocks topic (writeJournal s v)
                                          pure r

    writeJournal :: SequenceNum -> Val -> Locked Topic -> IO ()
    writeJournal seqNum val' lockedTopic = writeEntries journal lockedTopic [(seqNum, val')]

    --TODO dedupe
    writeJournals :: [(SequenceNum, Val)] -> Locked Topic -> IO ()
    writeJournals seqVals lockedTopic = writeEntries journal lockedTopic seqVals

    submitImpl2 :: Locked (Topic, Machine)
                -> IO (Either Retry SubmitResponse)
    submitImpl2 lockedMachineTopic =

        readMachine lockedMachineTopic >>= \case

            Nothing -> left $ NotDefined topic

            Just machine -> do

                let seqNum = next (lastSeqNum machine)
                u <- show <$> nextRandom

                case getLeader machine of

                    -- Elect self
                    Nothing -> do
                        r <- doProposal (ProposeRequest (cluster machine) (Key topic seqNum) (Value u (ControlValue (ElectLeader node))))
                        handleValue machine u seqNum r

                    -- Any leader is fine
                    Just _ -> do
                        r <- doProposal (ProposeRequest (cluster machine) (Key topic seqNum) (Value u val))
                        handleValue machine u seqNum r

        where
        handleValue _ _ _ (Left (_ :: ServerError)) = error "ServerError" 
        handleValue machine u seqNum (Right (Accepted (Value u' val'))) = do

            -- *Some* value was accepted
            case val' of

                SimpleValue _ ->
                    writeMachine lockedMachineTopic machine { lastSeqNum = seqNum }

                ControlValue (ElectLeader leader) ->
                    writeMachine lockedMachineTopic machine { lastSeqNum = seqNum
                                                            , getLeader  = Just leader
                                                            } 

            -- If it was *our* value
            if u' == u

                -- ...then we're done
                then right seqNum val'

                -- ...otherwise return what we learned and keep trying
                else retry (getLeader machine) seqNum val'

        handleValue _ _ _ _ = left NotAcceptedR -- TODO handle?

        readMachine :: Locked (Topic, Machine) -> IO (Maybe MachineState)
        readMachine lockedTopicMachine =
            readTopicDetail ident lockedTopicMachine "ms"

        writeMachine :: Locked (Topic, Machine) -> MachineState -> IO ()
        writeMachine lockedTopicMachine machine = do
            writeTopicDetail ident lockedTopicMachine "ms" machine

        doProposal :: ProposeRequest -> IO (Either ServerError ProposeResponse)
        doProposal = runHandler . propose proposer

        left :: Reason -> IO (Either l SubmitResponse)
        left = pure . Right . SubmitResponse . Left

        right :: SequenceNum -> Val -> IO (Either l SubmitResponse)
        right s v = pure . Right . SubmitResponse . Right $ (s, v)

        retry :: (Maybe Node) -> SequenceNum -> Val -> IO (Either Retry a)
        retry mLeader s v = pure . Left $ Retry mLeader s v

dumpImpl :: Locks Topic -> Journal -> Topic -> ((SequenceNum, Val) -> IO ()) -> IO ()
dumpImpl machineTopicLocks journal topic f =
    withLocked machineTopicLocks topic $ \lockedTopic ->
        dumpJournal journal lockedTopic f

