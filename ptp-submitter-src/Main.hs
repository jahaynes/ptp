{-# LANGUAGE LambdaCase,
             ScopedTypeVariables #-}

import           Client.SubmitterClient
import           Entity.Decree
import           SubmitterNode as SN
import           Requests.CreateTopic
import           Requests.Submit
import           Requests.Sync

import           Entity.Host
import           Entity.Id
import           Entity.Node
import           Entity.Port
import           Entity.Topic

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           Text.Printf              (printf)

topic :: Topic
topic = Topic "test"

main :: IO ()
main = do

    putStrLn "\n-------------------------\n"

    http <- newManager defaultManagerSettings

    let paxosCluster = [ Node (Id "paxos-1") localHost (Port 8080)
                       , Node (Id "paxos-2") localHost (Port 8081)
                       , Node (Id "paxos-3") localHost (Port 8082)
                       ]

    let submitterNodes = [ Node (Id "submitter-1") localHost (Port 8180)
                         , Node (Id "submitter-2") localHost (Port 8181)
                         ]

    mapM_ (SN.create http) submitterNodes

    forConcurrently_ submitterNodes $ \subNode -> do

        -- Create topic
        _ <- (createTopicBuilder http subNode) (CreateTopicRequest paxosCluster topic)

        -- Sync
        (syncBuilder http subNode) (SyncRequest topic) >>= \case

            Left e ->
                printf "Could not sync.  Shutting down: %s\n" (show e)

            Right (SyncResponse lo hi) ->
                printf "Synced on %s from: %s to %s\n" (show topic) (show lo) (show hi)

        -- Generate data
        producer http subNode

newtype Backoff =
    Backoff Int

producer :: Manager
         -> Node
         -> IO ()
producer http sn = go 0 sn (Backoff 10000)

    where
    go :: Int
       -> Node
       -> Backoff
       -> IO ()
    go  n subNode (Backoff bo) = do
    
        threadDelay 1000000
        printf "Attempt %d towards %s: %d: " n (show subNode) n

        (submitBuilder http subNode) (SubmitRequest (Topic "test") (ValueDecree $ printf "%s %d" (show $ getId subNode) n)) >>= \case

            Left e ->
                error $ "fatal: " ++ show e

            Right Submitted -> do
                printf "submitted\n"
                go (n+1) subNode better

            Right RetryRequested -> do
                printf "retry needed\n"
                go n subNode better

            Right (OtherLeader leader) -> do
                printf "switching leader %s -> %s\n" (show subNode) (show leader)
                go n leader better

            Right (SubmitError e) -> do
                threadDelay bo
                printf "Submission error: %s\n" e
                go n subNode worse

        where
        better :: Backoff
        better = Backoff $! max 10000 (bo `div` 8)

        worse :: Backoff
        worse  = Backoff $! min 640000 (bo * 2)
