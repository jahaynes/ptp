{-# LANGUAGE LambdaCase #-}

module Main where

import           Entity.Id
import           Entity.Port
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import qualified Server.Paxos.PaxosNode as P
import qualified SqliteStorage as SS

import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           RIO
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    getArgs >>= \case

        [myId, strPort] -> do
            let port = read strPort
            http <- newManager defaultManagerSettings
            printf "Paxos node started (%s %d)\n" myId port
            createPaxosNode http (Id myId) (Port port) $ \t s v ->
                printf "%s/%s -> %s\n" (show t) (show s) (show v)

        _ -> error "Must specify id and port"

createPaxosNode :: Manager
                -> Id
                -> Port
                -> (Topic -> SequenceNum -> Value -> IO ())
                -> IO ()
createPaxosNode http myId@(Id i) port callback = do
    -- ls <- F.create myId "ls" :: IO (LocalFileStorage LearnerState)
    ls <- SS.create $ i <> "learner"
    as <- SS.create $ i <> "acceptor" -- HMS.create
    wait =<< P.create http port as ls callback
