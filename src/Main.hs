{-# LANGUAGE OverloadedStrings #-}

import ProposalNumber           (Uniq (..), randomUniq)
import Types                    (ProposeRequest (..), Value (..))
import WebNode                  (runNode)
import WebNodeClient            (getProposeClient, makeClients)

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently)
import Network.HTTP.Client

main :: IO ()
main = do

    let ports = [8080..8091]

    http <- newManager $ defaultManagerSettings
                             {managerResponseTimeout = responseTimeoutMicro 3000000 }

    let clients = map (makeClients http "127.0.0.1") ports

    mapM_ (\p -> async . runNode (length clients) (show p) clients $ p) ports 

    threadDelay 10000

    xs <- mapConcurrently (\c -> do
        (Uniq r) <- randomUniq
        getProposeClient c $ ProposeRequest (Value $ take 3 r)) clients

    mapM_ print xs
