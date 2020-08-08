import Client.WebNodeClient
import ProposalNumber           (Uniq (..), randomUniq)
import SequenceNum
import Server.WebNode           (Node (..), create)
import Topic
import Types                    (Id (..), Key (..), ProposeRequest (..), Value (..))

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently, mapConcurrently_)
import Control.Monad            (forM_, unless)
import Data.Either              (fromRight)
import Data.Maybe               (fromJust)
import Network.HTTP.Client

main :: IO ()
main = do

    let ports = [8080..8086]

    -- Create clients
    http <- newManager $ defaultManagerSettings
                             {managerResponseTimeout = responseTimeoutMicro 3000000 }
    let clients = map (makeClients http "127.0.0.1") ports

    -- Create servers
    nodes <- mapM (\p -> create (length clients) p (Id $ show p) clients) ports 
    mapM_ (async . runNode) nodes

    threadDelay 10000

    mapConcurrently_ (runGroup clients) ["alpha", "beta", "gamma", "delta"]

    where
    runGroup clients group = do

        let proposalClients =  map getProposeClient clients
        let checkClients    =  map getCheckClient clients
        let purgeClients    =  map getPurgeLearnerClient clients
                            ++ map getPurgeAcceptorClient clients

        let topics = map (\x -> Key (Topic x) (SequenceNum 0))
                   . map (\n -> group ++ "_" ++ show n)
                   $ [(1::Int)..]

        forM_ topics $ \topic -> do

            -- Propose
            mapConcurrently_ (\client -> do
                (Uniq r) <- randomUniq
                let val = Value $ take 5 r
                client $ ProposeRequest topic val) proposalClients

            -- Check
            (r:esults) <- map (fromJust . fromRight undefined) <$> mapConcurrently (\c -> c topic) checkClients
            unless (all (==r) esults) $ error $ "CRASH: " ++ show (r:esults)

            print r

            -- Cleanup
            mapConcurrently_ (\client -> client topic) purgeClients
