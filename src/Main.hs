{-# LANGUAGE LambdaCase, TypeOperators #-}

import           Client.ExternalClient
import           Client.InternalClient
import           Entity.Id                      (Id (..))
import           Entity.Node                    (Node (..))
import           Entity.Port                    (Port (..))
import           Entity.SequenceNum
import           Entity.Topic                   (Topic (..))
import           Entity.Value
import           Journal                   as J (create)
import           Requests.Catchup
import           Requests.CreateTopic
import           Requests.JoinCluster
import           Requests.ReadJournal
import           Requests.SubmitCluster
import qualified Server.CallbackMap        as C (CallbackMap (..), create)
import           Server.ExternalApi             (ExternalApi)
import           Server.InternalApi             (InternalApi)
import           Server.Locks                   (newLocks)

import qualified Server.Paxos.Acceptor     as A (Acceptor (..), create)
import qualified Server.Paxos.Executor     as E
import qualified Server.Paxos.Learner      as L (Learner (..), create)
import qualified Server.Paxos.Proposer     as P (create)

import           Control.Concurrent         (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async   (async, forConcurrently, forConcurrently_, mapConcurrently_, wait)
import           Control.Monad              (forM_)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set as S
import           Data.Word                  (Word64)
import           Network.HTTP.Client hiding (Proxy, host, port)
import           Network.Wai.Handler.Warp   (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           Servant                    (serve)
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
            -> IO ()
runExecutor http callbackMap node@(Node ident (Port port)) = do

    proposer      <- P.create (prepareBuilder http) (acceptBuilder http)

    acceptorLocks <- newLocks
    acceptor      <- A.create ident acceptorLocks (learnBuilder http)

    journal       <- J.create ident

    executor      <- E.create node http journal proposer callbackMap

    learnerLocks  <- newLocks
    learner       <- L.create ident learnerLocks (E.callback executor E.Regular)

    ready <- newEmptyMVar

    let settings = setPort port
                 . setBeforeMainLoop (putMVar ready ())
                 $ defaultSettings

        internalRoutes = E.join executor
                    :<|> E.ping executor
                    :<|> E.catchup executor
                    :<|> E.submitNode executor
                    :<|> E.readJournal executor
                    :<|> E.getSequenceNum executor
                    :<|> A.prepare acceptor
                    :<|> A.accept acceptor
                    :<|> L.learn learner

        externalRoutes = E.joinCluster executor
                    :<|> E.createTopic executor
                    :<|> E.submitCluster executor

    _ <- async . runSettings settings $
        serve (Proxy :: Proxy (InternalApi :<|> ExternalApi)) $ internalRoutes :<|> externalRoutes

    takeMVar ready

main :: IO ()
main = do

    let (newNode:allNodes) = createIds 3

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 10000000 }

    callbackMap <- C.create
    mapConcurrently_ (runExecutor http callbackMap) allNodes

    runExecutor http callbackMap newNode

    -- Create sample data
    let topics      = map (\n -> Topic ("topic_" ++ show n)) [(1::Int)..3]
    let firstBatch  = 100
    let secondBatch = 100
    let thirdBatch  = 100
    let numData     = sum [firstBatch, secondBatch, thirdBatch]

    -- Create the topics
    forConcurrently_ topics $ \topic ->
        forConcurrently_ allNodes $ \node ->
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

    -- Tell newNode to create topics
    j2 <- async . forConcurrently_ topics $ \topic -> do
            Right _ <- createTopicBuilder http newNode (CreateTopicRequest topic (S.fromList allNodes))
            pure ()

    wait j1
    wait j2

    host <- choice allNodes

    -- Pre-join catchup phase
    j3 <- async . forConcurrently_ topics $ \topic ->
        catchupBuilder http newNode (CatchupRequest host topic)

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
    j5 <- async . forConcurrently_ topics $ \topic ->
        joinClusterBuilder http newNode (JoinClusterRequest host topic)

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
