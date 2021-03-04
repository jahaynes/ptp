{-# LANGUAGE FlexibleInstances,
             LambdaCase,
             MultiParamTypeClasses #-}

module Main where

import           Client.PaxosClient
import           Entity.Host
import           Entity.Id
import           Entity.Node
import           Entity.Port
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Uniq
import           Entity.Value
import           Requests.Propose
import           Server.Paxos.Acceptor (AcceptorState)
import           Server.Paxos.Learner  (LearnerState)
import qualified Server.Paxos.PaxosNode  as P
import           Server.InMemStorage     as I

import           Control.Concurrent.Async (Async, cancel, forConcurrently_, replicateConcurrently)
import           Control.Concurrent.STM
import           Control.Monad            (forM_, when)
import           Data.Functor             ((<&>))
import           Data.List.Split          (chunksOf)
import           Data.Set                 (fromList)
import           ListT                    (toList)
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import qualified StmContainers.Map as M
import           System.Random            (randomRIO)
import           System.Timeout           (timeout)

data Cluster =
    Cluster { jobs  :: ![Async ()]
            , nodes :: ![Node]
            }

clusterSize :: Int
clusterSize = 3

startCluster :: Manager
             -> (Topic -> SequenceNum -> Value -> IO ())
             -> IO Cluster
startCluster http callback =
    replicateConcurrently clusterSize startNode <&> \xs ->
        Cluster (map fst xs) (map snd xs)
    where
    startNode = do
        Uniq u <- uniq
        let myId = Id u
        randomPort <- Port <$> randomRIO (20000, 30000)
        -- as <- I.create myId "as" :: IO (LocalFileStorage AcceptorState)
        as <- I.create           :: IO (InMemStorage AcceptorState)
        -- ls <- I.create myId "ls" :: IO (LocalFileStorage LearnerState)
        ls <- I.create           :: IO (InMemStorage LearnerState)
        a  <- P.create http randomPort as ls callback
        pure (a, Node myId localHost randomPort)

main :: IO ()
main = do

    http <- newManager defaultManagerSettings

    singleValueTest http
    putStrLn "Single value test passed"

    sequenceOfValuesTest http
    putStrLn "Sequence of values test passed"

    competingValuesTest http
    putStrLn "Competing values test passed"

    slowpokeTest http
    putStrLn "Slowpoke test passed"

    where
    singleValueTest :: Manager -> IO ()
    singleValueTest http = do

        -- Setup
        tvResults <- newTVarIO []

        cluster <- startCluster http $ \topic seqNum val ->
            atomically $ modifyTVar' tvResults ((topic, seqNum, val):)

        node <- choice (nodes cluster)

        let topic  = Topic "test"
            seqNum = SequenceNum 1
        input <- randomValue

        -- When
        Right (Accepted output) <- proposeBuilder http node $
            ProposeRequest (fromList (nodes cluster)) topic seqNum input

        -- Then
        Just results <- timeout 25000 . atomically $ do
            results <- readTVar tvResults
            if length results < clusterSize
                then retry
                else pure results

        _ <- same $ (topic, seqNum, input)
                  : (topic, seqNum, output)
                  : results

        -- Teardown
        mapM_ cancel (jobs cluster)

    sequenceOfValuesTest :: Manager -> IO ()
    sequenceOfValuesTest http = do

        -- Setup
        results <- M.newIO

        cluster <- startCluster http $ \topic seqNum val ->
            atomically $ do
                M.lookup (topic, seqNum) results >>= \case
                    Nothing                      -> M.insert (val, 1::Int) (topic, seqNum) results
                    Just (val', n) | val == val' -> M.insert (val,    n+1) (topic, seqNum) results
                                   | otherwise   -> error "mismatch"

        let topic   = Topic "test"
            numMsgs = 200

        -- When
        forConcurrently_ (chunksOf 40 $ map (SequenceNum . fromIntegral) [1..numMsgs]) $ \batch ->
            forM_ batch $ \seqNum -> do
                input <- randomValue
                node  <- choice (nodes cluster)
                Right (Accepted _) <- proposeBuilder http node $
                    ProposeRequest (fromList (nodes cluster)) topic seqNum input
                pure ()

        -- Then
        Just _ <- timeout 25000 . atomically $ do
            xs <- toList . M.listT $ results
            let ls = map (snd . snd) xs
            if sum ls == clusterSize * numMsgs && all (==clusterSize) ls
                then pure ()
                else retry

        -- Teardown
        mapM_ cancel (jobs cluster)

    competingValuesTest :: Manager -> IO ()
    competingValuesTest http = do

        -- Setup
        results <- M.newIO

        cluster <- startCluster http $ \topic seqNum val ->
            atomically $ do
                M.lookup (topic, seqNum) results >>= \case
                    Nothing                      -> M.insert (val, 1::Int) (topic, seqNum) results
                    Just (val', n) | val == val' -> M.insert (val,    n+1) (topic, seqNum) results
                                   | otherwise   -> error "mismatch"

        let topic   = Topic "test"
            numMsgs = 200

        -- When
        forM_ (map (SequenceNum . fromIntegral) [1..numMsgs]) $ \seqNum ->
            forM_ (nodes cluster) $ \node -> do
                input <- randomValue
                Right (Accepted _x) <- proposeBuilder http node $
                    ProposeRequest (fromList (nodes cluster)) topic seqNum input
                -- printf "seqNum %s -> %s -> %s\n" (show seqNum) (show input) (show _x)
                pure ()

        -- Then
        Just _ <- timeout 25000 . atomically $ do
            xs <- toList . M.listT $ results
            let ls = map (snd . snd) xs
            if sum ls == clusterSize * numMsgs && all (==clusterSize) ls
                then pure ()
                else retry

        -- Teardown
        mapM_ cancel (jobs cluster)

    slowpokeTest :: Manager -> IO ()
    slowpokeTest http = do

        tvCount <- newTVarIO (0 :: Int)
        cluster <- startCluster http $ \_ _ _ ->
            atomically $ modifyTVar' tvCount (+1)

        node <- choice (nodes cluster)

        let topic  = Topic "test"
            seqNum = SequenceNum 1

        -- Send an early message
        earlyMessage <- randomValue
        Right (Accepted earlyOutput) <- proposeBuilder http node $
            ProposeRequest (fromList (nodes cluster)) topic seqNum earlyMessage

        -- Wait for the 3x hits
        atomically $ do
            count <- readTVar tvCount
            when (count < 3) retry

        assert "Did not learn early message" $
            earlyMessage == earlyOutput

        -- Send a later message to the same seqNum
        laterMessage <- randomValue
        Right (Accepted lateOutput) <- proposeBuilder http node $
            ProposeRequest (fromList (nodes cluster)) topic seqNum laterMessage

        assert "Did not keep earlier message" $
            earlyMessage == lateOutput

        -- Teardown
        mapM_ cancel (jobs cluster)

same :: (Eq a, Show a) => [a] -> IO a
same     [] = error "No value"
same (x:xs) | all (==x) xs = pure x
            | otherwise    = error $ show (x:xs)

randomValue :: IO Value
randomValue = uniq <&> \(Uniq u) -> Value u

choice :: [a] -> IO a
choice [] = error "No choice"
choice xs = do
    r <- randomRIO (0, length xs - 1)
    pure $ xs !! r

assert :: String -> Bool -> IO ()
assert   _  True = pure ()
assert msg False = error msg