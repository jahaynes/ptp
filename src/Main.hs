import Client.WebNodeClient     (getCheckClient, getProposeClient, makeClients)
import ProposalNumber           (Uniq (..), randomUniq)
import Server.WebNode           (Node (..), create)
import Types                    (Id (..), ProposeRequest (..), Value (..))

import Control.Concurrent.Async (async, mapConcurrently)
import Network.HTTP.Client

main :: IO ()
main = do

    let ports = [8080..8091]

    -- Create clients
    http <- newManager $ defaultManagerSettings
                             {managerResponseTimeout = responseTimeoutMicro 3000000 }
    let clients = map (makeClients http "127.0.0.1") ports
    let proposalClients = map getProposeClient clients
    let checkClients = map getCheckClient clients

    -- Create servers
    nodes <- mapM (\p -> create (length clients) p (Id $ show p) clients) ports 
    mapM_ (async . runNode) nodes

    putStrLn "Proposing"
    mapM_ print =<< mapConcurrently (\client -> do
        (Uniq r) <- randomUniq
        client $ ProposeRequest (Value $ take 3 r)) proposalClients

    putStrLn "Checking"
    mapM_ print =<< mapConcurrently id checkClients
