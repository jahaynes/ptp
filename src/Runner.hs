{-# LANGUAGE LambdaCase #-}

module Runner (runTests) where

import Client.WebNodeClient
import Entity.CreateTopicRequest
import Entity.SubmitRequest
import Entity.Topic
import Entity.Value
import Node
import Server.Paxos.StateMachine

import           Control.Concurrent.Async (replicateConcurrently_)
import           Control.Concurrent.STM
import           Control.Monad            (forM_, replicateM_)
import           Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Network.HTTP.Client
import           System.Random            (randomRIO)
import           Text.Printf              (printf)

runTests :: TVar [Node] -> TVar [Node] -> Topic -> [StateMachine m] -> IO ()
runTests tvActiveNodes tvInactiveNodes topic stateMachines = do

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 10000000 }

    allNodes <- (++) <$> readTVarIO tvActiveNodes
                     <*> readTVarIO tvInactiveNodes

    printf "\nCreating topic: %s...\n" (show topic)
    mapM_ (\node -> createTopicBuilder http node (CreateTopicRequest (S.fromList allNodes) topic)) allNodes

    -- _runSimpleTest http
    _runParallelTest http

    putStrLn "Checking results"
    checkResults

    where
    _runSimpleTest http =
        runBulkInsert http 100

    _runParallelTest http =
        replicateConcurrently_ 4 $ _runSimpleTest http

    checkResults :: IO ()
    checkResults = do

        ioMap <- newIORef M.empty
        forM_ stateMachines $ \sm ->
            dump sm topic $ \(s,v) ->
                modifyIORef' ioMap $ \m -> M.alter (f s v) s m

        results <- M.toList <$> readIORef ioMap
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

            -- TODO this is not a proper leader election?
            -- Because activeNodes/tvActiveNodes is a local variable, not shared state
            addOrRemoveNode

            activeNodes <- readTVarIO tvActiveNodes
            r <- randomRIO (0::Int, 100000)
            let val = SimpleValue (printf "hello_%d" r)

            --let chosen = minimum activeNodes
            chosen <- choice activeNodes

            x <- submitBuilder http chosen (SubmitRequest topic val)

            -- TODO: On success notify others ?

            print x

        where
        choice :: [a] -> IO a
        choice xs = do
            i <- randomRIO (0, length xs - 1)
            pure $ xs !! i

    addOrRemoveNode :: IO ()
    addOrRemoveNode = do

        r <- randomRIO (1::Int, 10)

        atomically $ do

            activeNodes <- readTVar tvActiveNodes

            inactiveNodes <- readTVar tvInactiveNodes

            case r of

                -- Drop one
                1 | length activeNodes > 3 -> transfer tvActiveNodes tvInactiveNodes

                -- Add one
                2 | not (null inactiveNodes) -> transfer tvInactiveNodes tvActiveNodes

                _ -> pure ()

        where
        transfer :: TVar [a] -> TVar [a] -> STM ()
        transfer src dst =
            readTVar src >>= \case
                []       -> error "Nope"
                (s:srcs) -> do
                    writeTVar src srcs
                    modifyTVar' dst (s:)
                    