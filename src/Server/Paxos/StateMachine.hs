{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase,
             ScopedTypeVariables #-}

module Server.Paxos.StateMachine ( StateMachine (..)
                                 , create
                                 ) where

import Server.Paxos.Proposer      (Proposer (..))
import Entity.CreateTopicRequest  (CreateTopicRequest (..))
import Entity.CreateTopicResponse (CreateTopicResponse (..))
import Entity.Id                  (Id (..))
import Entity.Key                 (Key (..))
import Entity.ProposeRequest      (ProposeRequest (..))
import Entity.ProposeResponse     (ProposeResponse (..))
import Entity.SequenceNum         (SequenceNum (..), next)
import Entity.SubmitRequest       (SubmitRequest (..))
import Entity.SubmitResponse      (SubmitResponse (..))
import Entity.Topic               (Topic)
import Entity.Value               (Value (..), Val (..))
import Journal                    (Journal (..))
import Node                       (Node (..))
import Server.Files               (readTopic, writeTopic)
import Server.Locks               (Locked, Locks, withLocked)

import Codec.Serialise        (Serialise)
import Control.DeepSeq        (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set               (Set)
import Data.UUID.V4           (nextRandom)
import GHC.Generics           (Generic)
import Servant                (Handler, runHandler)
import Text.Printf            (printf)

data StateMachine m =
    StateMachine { createTopic :: !(CreateTopicRequest -> m CreateTopicResponse)
                 , submit      :: !(SubmitRequest -> m SubmitResponse)
                 , dump        :: !(Topic -> ((SequenceNum, Val) -> IO ()) -> IO ())
                 }

data MachineState =
    MachineState { cluster    :: !(Set Node)
                 , lastSeqNum :: !SequenceNum
                 } deriving (Generic, Serialise, NFData)

create :: MonadIO m => Node
                    -> Journal
                    -> Locks Topic
                    -> Proposer Handler
                    -> IO (StateMachine m)
create (Node ident _) journal machineTopicLocks proposer = do

    let readMachine lockedTopic =
            readTopic ident lockedTopic "ms"

    let doProposal =
            runHandler . propose proposer

    pure $ StateMachine { createTopic = liftIO . createTopicImpl (writeMachineImpl ident machineTopicLocks)
                        , submit      = liftIO . submitImpl ident doProposal machineTopicLocks readMachine journal
                        , dump        = dumpImpl machineTopicLocks journal
                        }

-- TODO catch deep
createTopicImpl :: (Topic -> MachineState -> IO ())
                -> CreateTopicRequest
                -> IO CreateTopicResponse
createTopicImpl writeMachine (CreateTopicRequest nodes topic) = do
    -- Warn. No checking before overwrite
    -- maybe get consensus on a 'master' topic
    writeMachine topic $! MachineState { cluster    = nodes
                                       , lastSeqNum = SequenceNum 1
                                       }
    pure CreateTopicResponse

submitImpl :: Id
           -> (ProposeRequest -> IO (Either l ProposeResponse))
           -> Locks Topic
           -> (Locked Topic -> IO (Maybe MachineState))
           -> Journal
           -> SubmitRequest
           -> IO SubmitResponse
submitImpl ident doProposal machineTopicLocks readMachine journal (SubmitRequest topic val) =

    withLocked machineTopicLocks topic $ \lockedTopic ->

        let retry =

                readMachine lockedTopic >>= \case

                    Nothing -> left $ printf "%s not defined." (show topic)

                    Just machine -> do

                        u <- show <$> nextRandom
                        let seqNum = next (lastSeqNum machine)
                        Right r <- doProposal $ ProposeRequest (cluster machine)
                                                               (Key topic seqNum)
                                                               (Value u val)

                        case r of

                            Accepted v@(Value u' val') -> do

                                -- *Some* value was accepted
                                writeTopic ident lockedTopic "ms" $! machine { lastSeqNum = seqNum }
                                writeEntries journal lockedTopic [(seqNum, val')]

                                if u' == u

                                    -- If it was *our* value, we're done
                                    then right seqNum v

                                    -- Otherwise keep trying to submit
                                    else gallop

                            _ -> left "Not accepted"

            gallop = retry -- TODO

        in retry

dumpImpl :: Locks Topic -> Journal -> Topic -> ((SequenceNum, Val) -> IO ()) -> IO ()
dumpImpl machineTopicLocks journal topic f =
    withLocked machineTopicLocks topic $ \lockedTopic ->
        dumpJournal journal lockedTopic f

writeMachineImpl :: Serialise a => Id -> Locks Topic -> Topic -> a -> IO ()
writeMachineImpl ident machineTopicLocks topic ms =
    withLocked machineTopicLocks topic $ \lockedTopic ->
        writeTopic ident lockedTopic "ms" ms

left :: String -> IO SubmitResponse
left = pure . SubmitResponse . Left

right :: SequenceNum -> Value -> IO SubmitResponse
right s v = pure . SubmitResponse . Right $ (s, v)