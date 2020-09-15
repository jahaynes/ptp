module Runner (runTests) where

import Client.WebNodeClient
import Entity.CreateTopicRequest
import Entity.SubmitRequest
import Entity.Topic
import Entity.Value
import Node
import Server.Paxos.StateMachine

import           Control.Concurrent.Async (replicateConcurrently_)
import           Control.Monad            (forM_, replicateM_)
import           Data.IORef
import           Data.Map.Strict
import qualified Data.Set as S
import           Network.HTTP.Client
import           System.Random            (randomRIO)
import           Text.Printf              (printf)

runTests :: [Node] -> Topic -> [StateMachine m] -> IO ()
runTests startingNodes topic stateMachines = do

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 3000000 }

    printf "\nCreating topic: %s...\n" (show topic)
    mapM_ (\node -> createTopicBuilder http node (CreateTopicRequest (S.fromList startingNodes) topic)) startingNodes

    -- _runSimpleTest http
    _runParallelTest http

    checkResults

    where
    _runSimpleTest http =
        runBulkInsert http 40

    _runParallelTest http =
        replicateConcurrently_ 4 $ _runSimpleTest http

    checkResults :: IO ()
    checkResults = do

        ioMap <- newIORef empty
        forM_ stateMachines $ \sm ->
            dump sm topic $ \(s,v) ->
                modifyIORef' ioMap $ \m -> alter (f s v) s m

        results <- toList <$> readIORef ioMap
        mapM_ print results

        where
        f :: (Eq v, Show s, Show v) => s -> v -> Maybe (Int, v) -> Maybe (Int, v)
        f _ v  Nothing                   = Just (1, v)
        f s v (Just (n, v')) | v /= v'   = error $ printf "MISMATCH in %s: %s %s" (show s) (show v) (show v')
                            | otherwise = Just (n + 1, v')

    runBulkInsert :: Manager -> Int -> IO ()
    runBulkInsert http n = do
        printf "Sending messages...\n"
        replicateM_ n $ do
            r <- randomRIO (0::Int, 1000)
            let val = SimpleValue (printf "hello_%d" r)
            chosen <- choice startingNodes
            printf "Submitting to %s value %s\n" (show chosen) (show val)
            Right _ <- submitBuilder http chosen (SubmitRequest topic val)
            pure ()

        where
        choice :: [a] -> IO a
        choice xs = do
            i <- randomRIO (0, length xs - 1)
            pure $ xs !! i
