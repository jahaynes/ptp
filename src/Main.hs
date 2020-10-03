{-# LANGUAGE LambdaCase #-}

import           Client.WebNodeClient
import           Entity.Id                      (Id (..))
import           Entity.Node                    (Node (..))
import           Entity.Port                    (Port (..))
import           Entity.SequenceNum
import           Entity.Topic                   (Topic (..))
import           Entity.Value
import           Journal                   as J (create)
import           Requests.CreateTopic
import           Requests.Join
import           Requests.ReadJournal
import           Requests.SequenceNum
import           Requests.SubmitCluster
import qualified Server.CallbackMap        as C (CallbackMap (..), create)
import           Server.Locks                   (newLocks)
import           Server.NodeApi                 (NodeApi)
import qualified Server.Paxos.Acceptor     as A (Acceptor (..), create)
import qualified Server.Paxos.Executor     as E
import qualified Server.Paxos.Learner      as L (Learner (..), create)
import qualified Server.Paxos.Proposer     as P (Proposer (..), create)

import           Control.Concurrent         (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async   (async, forConcurrently, forConcurrently_, mapConcurrently, wait)
import           Control.Monad              (forM_, when)
import           Data.List.Split            (chunksOf)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set as S
import           Data.Word                  (Word64)
import           Network.HTTP.Client hiding (Proxy, host, port)
import           Network.Wai.Handler.Warp   (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           Servant                    (Handler, serve)
--import           Servant.Client             (ClientError)
import           Servant.API
import           System.Random
import           Text.Printf                (printf)

createIds :: Int -> [Node]
createIds n = do
    let range = take n [8080..]
        ports = map Port range
        ids   = map (Id . show) range
    zipWith Node ids ports

runExecutor :: Manager
            -> C.CallbackMap
            -> Node
            -> IO (E.Executor Handler)
runExecutor http callbackMap node@(Node ident (Port port)) = do

    proposer      <- P.create (prepareBuilder http) (acceptBuilder http)

    acceptorLocks <- newLocks
    acceptor      <- A.create ident acceptorLocks (learnBuilder http)

    journal       <- J.create ident

    executor      <- E.create node http journal proposer callbackMap

    learnerLocks  <- newLocks
    learner       <- L.create ident learnerLocks (E.callback executor E.Regular)

    serverReady   <- newEmptyMVar

    let settings = setPort port
                 . setBeforeMainLoop (putMVar serverReady ())
                 $ defaultSettings

    _ <- async . runSettings settings $
        serve (Proxy :: Proxy NodeApi) $ E.join executor
                                    :<|> E.ping executor
                                    :<|> E.createTopic executor
                                    :<|> E.submitCluster executor
                                    :<|> E.submitNode executor
                                    :<|> E.readJournal executor
                                    :<|> E.getSequenceNum executor
                                    :<|> P.propose proposer
                                    :<|> A.prepare acceptor
                                    :<|> A.accept acceptor
                                    :<|> L.learn learner

    takeMVar serverReady

    pure executor

main :: IO ()
main = do

    let (newNode:allNodes) = createIds 3

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 10000000 }

    callbackMap <- C.create
    _   <- mapConcurrently (runExecutor http callbackMap) allNodes

    newExecutor <- runExecutor http callbackMap newNode

    -- Create sample data
    let topics      = map (\n -> Topic ("topic_" ++ show n)) [(1::Int)..3]
    let firstBatch  = 100
    let secondBatch = 100
    let thirdBatch  = 100
    let numData     = sum [firstBatch, secondBatch, thirdBatch]

    -- Create the topics
    forM_ topics $ \topic ->
        forM_ allNodes $ \node ->
            createTopicBuilder http node (CreateTopicRequest topic (S.fromList allNodes)) >>= \case
                Left e -> error $ show e
                Right (CreateTopicResponse (Right ())) -> printf "%s created on node %s\n" (show topic) (show node)

    -- Spam the values
    j1 <- async . forConcurrently_ topics $ \topic ->
        forM_ [(1::Word64),6..firstBatch] $ \n -> do
            let sampleValues = map (SimpleValue . printf "foo_%d") [n..n+5]
            print sampleValues
            node <- choice allNodes
            forM_ sampleValues $ \v -> submitClusterBuilder http node (SubmitClusterRequest topic v)

    -- From newNode's POV
    j2 <- async . forConcurrently_ topics $ \topic -> do
            Right _ <- createTopicBuilder http newNode (CreateTopicRequest topic (S.fromList allNodes))
            pure ()

    wait j1
    wait j2

    host <- choice allNodes

    -- Pre-join catchup phase
    j3 <- async . forConcurrently_ topics $ \topic -> do
        Right (SequenceNumResponse (Just (SequenceNum sn))) <- sequenceNumBuilder http host (SequenceNumRequest topic)
        let gatherRanges = chunksOf 20 [1..sn]
        printf "pre-gathering %s\n" (show gatherRanges)
        -- TODO check all were actually fetched
        forM_ gatherRanges $ \gatherRange ->
            readJournalBuilder http host (ReadJournalRequest topic (map SequenceNum gatherRange)) >>= \case
                Left e -> error $ show e
                Right (ReadJournalResponse responses) ->
                    forM_ responses $ \(s',v') -> E.callback newExecutor E.Catchup topic s' v'

    -- Spam the values
    j4 <- async . forConcurrently_ topics $ \topic ->
        forM_ [(1::Word64),6..secondBatch] $ \n -> do
            let sampleValues = map (SimpleValue . printf "foo_%d") [n..n+5]
            print sampleValues
            node <- choice allNodes
            forM_ sampleValues $ \v -> submitClusterBuilder http node (SubmitClusterRequest topic v)

    wait j3
    wait j4

    -- Join and post-join catchup phase
    j5 <- async . forConcurrently_ topics $ \topic -> do
        joinBuilder http host (JoinPrepare topic newNode) >>= \case
            Left l -> error $ show l
            Right (JoinedAt (SequenceNum _end)) ->
                sequenceNumBuilder http newNode (SequenceNumRequest topic) >>= \case
                    Left l -> error $ show l
                    Right (SequenceNumResponse (Just (SequenceNum start))) ->
                        let loop sn = do
                                readJournalBuilder http host (ReadJournalRequest topic [SequenceNum sn]) >>= \case
                                    Left e -> error $ show e
                                    Right (ReadJournalResponse responses) ->
                                        forM_ responses $ \(s',v') -> do
                                            mode <- E.callback newExecutor E.Catchup topic s' v'
                                            when (mode == E.Catchup) (loop $ sn + 1)
                        in loop (start + 1)

    -- Spam the values
    j6 <- async . forConcurrently_ topics $ \topic ->
        forM_ [(1::Word64),6..thirdBatch] $ \n -> do
            let sampleValues = map (SimpleValue . printf "foo_%d") [n..n+5]
            print sampleValues
            node <- choice allNodes
            forM_ sampleValues $ \v -> submitClusterBuilder http node (SubmitClusterRequest topic v)

    wait j5
    wait j6

    forConcurrently_ topics $ \topic -> do
        printf "Checking %s\n" (show topic)
        forM_ [1..numData] $ \n -> do
            _ <- same =<< forConcurrently (newNode:allNodes) (\node -> do
                              Right (ReadJournalResponse x) <- readJournalBuilder http node (ReadJournalRequest topic [SequenceNum n])
                              pure x)
            pure ()

same :: (Eq a, Show a) => [a] -> IO a
same     [] = error "No value"
same (x:xs) | all (==x) xs = pure x
            | otherwise    = error $ show (x:xs)

choice :: [a] -> IO a
choice xs = do
    i <- randomRIO (0, length xs - 1)
    pure $ xs !! i
