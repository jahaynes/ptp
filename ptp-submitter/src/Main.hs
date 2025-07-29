{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

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

import           Codec.Serialise          (serialise)
import           Data.List.Split          (splitOn)
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           RIO
import           RIO.Text                 (pack)
import           Servant.Client           (ClientError (ConnectionError))
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

getPaxosCluser :: IO (Topic, Port, [Node])
getPaxosCluser =
    getArgs >>= \case
        []        -> error "No paxos nodes supplied. Format is localport hostname1:port1 hostname2:port2 ..."
        (topic:p:nodes) -> pure (Topic $ pack topic, Port $ read p, map parseNode nodes)
    where
    parseNode :: String -> Node
    parseNode strNode =
        case splitOn ":" strNode of
            [h, p] -> let p' = read p
                          i' = pack $ printf "%s:%d" h p'
                      in Node (Id i') (host h) (Port p')
            _      -> error $ printf "invalid paxos node %s" strNode

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    printf "Submitter node started\n"
    (topic, port, paxosCluster) <- getPaxosCluser
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
        producer http topic subNode

newtype Backoff =
    Backoff Int

producer :: Manager
         -> Topic
         -> Node
         -> IO ()
producer http topic sn = go 0 sn (Backoff 10000)

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
                value  = printf "%s %d" (show $ getId subNode) n :: String
                decree = ValueDecree $ serialise value
                req    = SubmitRequest topic decree
            in client req

        forgetLeader = 
            let client = forgetLeaderBuilder http sn
                req    = ForgetLeaderRequest topic
            in client req

        better :: Backoff
        better = Backoff $! max 10000 (bo `div` 8)

        worse :: Backoff
        worse  = Backoff $! min 640000 (bo * 2)
