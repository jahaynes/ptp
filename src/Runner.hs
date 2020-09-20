{-# LANGUAGE LambdaCase #-}

module Runner (runTests) where

import Client.WebNodeClient
import Entity.CatchupRequest
import Entity.CreateTopicRequest
-- import Entity.SequenceNum
import Entity.SubmitRequest
import Entity.SubmitResponse
import Entity.Topic
import Entity.Value
import Node
import Server.Paxos.StateMachine

import           Control.Concurrent.Async (forConcurrently_, replicateConcurrently_)
import           Control.Monad            (forM_, replicateM_)
import           Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Network.HTTP.Client
import           System.Random            (randomRIO)
import           Text.Printf              (printf)

runTests :: [Node] -> Topic -> [StateMachine m] -> IO ()
runTests nodes topic stateMachines = do

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 10000000 }

    printf "\nCreating topic: %s...\n" (show topic)
    mapM_ (\node -> createTopicBuilder http node (CreateTopicRequest (S.fromList nodes) topic)) nodes

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

            rand <- randomRIO (0::Int, 100000)
            let val = SimpleValue (printf "hello_%d" rand)

            chosen <- choice nodes

            let loop node =

                    submitBuilder http node (SubmitRequest topic val) >>= \case

                        Left l -> error $ show l

                        Right (SubmitResponse (Left (SubmitElsewhere leader))) -> loop leader

                        Right (SubmitResponse (Right (s, v))) -> do

                            -- On success notify others
                            forConcurrently_ (filter (/=chosen) nodes) $ \other -> do
                                _ <- catchupBuilder http other (CatchupRequest topic [s])
                                pure ()

                            print (s,v)

            loop chosen

        where
        choice :: [a] -> IO a
        choice xs = do
            i <- randomRIO (0, length xs - 1)
            pure $ xs !! i
