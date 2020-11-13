{-# LANGUAGE LambdaCase,
             ScopedTypeVariables #-}

import           Client.SubmitterClient
import           Entity.Decree
import           SubmitterNode as SN
import           Requests.CreateTopic
import           Requests.Submit
import           Requests.Sync

import           Entity.Id
import           Entity.Node
import           Entity.Port
import           Entity.Topic
import           Entity.Uniq

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad            (foldM_, replicateM)
import           Data.Functor             ((<&>))
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           Text.Printf              (printf)

topic :: Topic
topic = Topic "test"

main :: IO ()
main = do

    putStrLn "\n-------------------------\n"

    http <- newManager defaultManagerSettings

    ids               <- replicateM 3 (uniq <&> \(Uniq u) -> Id u)  -- TODO these IDs aren't exactly used!
    let ports          = map Port . take 3 $ [8080..]
        defaultCluster = zipWith Node ids ports                     -- TODO these IDs aren't exactly used!

    let submitNode1 = Node (Id "submitter-1") (Port 8180)
    let submitNode2 = Node (Id "submitter-2") (Port 8181)

    _ <- SN.create http submitNode1
    _ <- SN.create http submitNode2

    forConcurrently_ [submitNode1, submitNode2] $ \subNode -> do

        -- Create topic
        _ <- (createTopicBuilder http subNode) (CreateTopicRequest defaultCluster topic)

        -- Sync
        Right (SyncResponse lo hi) <- (syncBuilder http subNode) (SyncRequest topic)
        printf "Synced on %s from: %s to %s\n" (show topic) (show lo) (show hi)

        -- Generate data
        producer http subNode

newtype Backoff =
    Backoff Int

producer :: Manager -> Node -> IO ()
producer http sn = foldM_ f (sn, Backoff 10000, 1) [1..]
    where
    f :: (Node, Backoff, Int) -> Int -> IO (Node, Backoff, Int)
    f (subNode, Backoff bo, n) i = do

        printf "Attempt %d towards %s: %d: " i (show subNode) n
        (submitBuilder http subNode) (SubmitRequest (Topic "test") (ValueDecree $ "msg: " ++ show n)) >>= \case

            Left e -> error $ show e

            Right Submitted -> do
                printf "submitted\n"
                pure (subNode, better, n + 1)

            Right RetryRequested -> do
                printf "retry needed\n"
                pure (subNode, better, n)

            Right (OtherLeader leader) -> do
                printf "switching leader %s -> %s\n" (show subNode) (show leader)
                pure (leader, better, n)

            Right (SubmitError e) -> do
                threadDelay bo
                printf "%s\n" e
                pure (subNode, worse, n)

        where
        better :: Backoff
        better = Backoff $! max 10000 (bo `div` 8)

        worse :: Backoff
        worse  = Backoff $! min 640000 (bo * 2)
