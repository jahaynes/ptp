{-# LANGUAGE LambdaCase,
             ScopedTypeVariables #-}

import           Entity.Decree
import           Submitter         as S

import           Entity.Id
import           Entity.Node
import           Entity.Port
import           Entity.Topic
import           Entity.Uniq

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad            (foldM_, replicateM)
import           Data.Functor             ((<&>))
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
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

    submitter1 <- S.create (Node (Id "submitter-1") (Port 30)) http
    submitter2 <- S.create (Node (Id "submitter-2") (Port 30)) http

    forConcurrently_ [submitter1, submitter2] $ \sub -> do

        -- Create topic
        createTopic sub topic defaultCluster

        -- Sync
        (lo, hi) <- sync sub topic
        printf "Synced on %s from: %s to %s\n" (show topic) (show lo) (show hi)

        -- Generate data
        producer sub

    pure ()

newtype Backoff =
    Backoff Int

producer :: Submitter -> IO ()
producer submitter = foldM_ f (Backoff 10000, 1) [1..]
    where
    f :: (Backoff, Int) -> Int -> IO (Backoff, Int)
    f (Backoff bo, n) i = do

        printf "Attempt %d: %d: " i n
        submit submitter (Topic "test") (ValueDecree $ "msg: " ++ show n) >>= \case

            Submitted -> do
                printf "submitted\n"
                pure (better, n + 1)

            RetryRequested -> do
                printf "retry needed\n"
                pure (better, n)

            OtherLeader leader -> do
                error "not leader"

            SubmitError e -> do
                threadDelay bo
                printf "%s\n" e
                pure (worse, n)

        where
        better :: Backoff
        better = Backoff $! max 10000 (bo `div` 8)

        worse :: Backoff
        worse  = Backoff $! min 640000 (bo * 2)
