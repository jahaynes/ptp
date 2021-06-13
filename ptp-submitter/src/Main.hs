{-# LANGUAGE LambdaCase #-}

import           Client.SubmitterClient
import           Entity.Decree
import           SubmitterNode as SN
import           Requests.CreateTopic
import           Requests.ForgetLeader
import           Requests.Submit
import           Requests.Sync

import           Entity.Host
import           Entity.Id
import           Entity.Node
import           Entity.Port
import           Entity.Topic

import           Data.List.Split          (splitOn)
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           RIO
import           Servant.Client           (ClientError (ConnectionError))
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

topic :: Topic
topic = Topic "test"

getPaxosCluser :: IO (Port, [Node])
getPaxosCluser =
    getArgs >>= \case
        []        -> error "No paxos nodes supplied. Format is hostname1:port1 hostname2:port2"
        (p:nodes) -> pure (Port $ read p, map parseNode nodes)
    where
    parseNode :: String -> Node
    parseNode strNode =
        case splitOn ":" strNode of
            [h, p] -> let p' = read p in Node (Id (printf "%s:%d" h p')) (host h) (Port p')
            _      -> error $ printf "invalid paxos node %s" strNode

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    printf "Submitter node started\n"
    (port, paxosCluster) <- getPaxosCluser
    printf "Connecting to %s\n" (show paxosCluster)

    http <- newManager defaultManagerSettings

    let submitterNodes = [ Node (Id "submitter-1") localHost port]

    mapM_ (SN.create http) submitterNodes

    forConcurrently_ submitterNodes $ \subNode -> do

        -- Create topic
        printf "Creating topic %s\n" (show topic)
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
    go n subNode (Backoff bo) = do

        -- threadDelay 1000000

        printf "Attempt %d towards %s: %d: " n (show subNode) n

        fire >>= \case

            Left ConnectionError{} -> do
                threadDelay bo
                printf "Bad connection (to other submitter node? Will try to forget leader.\n"
                forgetLeader >>= \case
                    Left l -> error $ "Could not forget leader: " ++ show l
                    Right ForgetLeaderResponse ->
                        go n sn worse

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
        fire =
            let client = submitBuilder http subNode
                decree = ValueDecree $ printf "%s %d" (show $ getId subNode) n
                req    = SubmitRequest (Topic "test") decree
            in client req

        forgetLeader = 
            let client = forgetLeaderBuilder http sn
                req    = ForgetLeaderRequest (Topic "test")
            in client req

        better :: Backoff
        better = Backoff $! max 10000 (bo `div` 8)

        worse :: Backoff
        worse  = Backoff $! min 640000 (bo * 2)
