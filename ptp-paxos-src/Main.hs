{-# LANGUAGE LambdaCase #-}

module Main where

import           Entity.Id
import           Entity.Port
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import           Server.LocalFileStorage as F
import           Server.Paxos.Acceptor        (AcceptorState)
import           Server.Paxos.Learner         (LearnerState)
import qualified Server.Paxos.PaxosNode  as P

import           Control.Concurrent.Async (wait)
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           System.Environment       (getArgs)
import           System.IO                (BufferMode (..), hSetBuffering, stdout)
import           Text.Printf              (printf)

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    getArgs >>= \case

        [myId, strPort] -> do
            let port = read strPort
            http <- newManager defaultManagerSettings
            createPaxosNode http (Id myId) (Port port) $ \t s v ->
                printf "%s/%s -> %s\n" (show t) (show s) (show v)

        _ -> error "Must specify id and port"

createPaxosNode :: Manager
                -> Id
                -> Port
                -> (Topic -> SequenceNum -> Value -> IO ())
                -> IO ()
createPaxosNode http myId port callback = do
    as <- F.create myId "as" :: IO (LocalFileStorage AcceptorState)
    ls <- F.create myId "ls" :: IO (LocalFileStorage LearnerState)
    wait =<< P.create http port as ls callback
