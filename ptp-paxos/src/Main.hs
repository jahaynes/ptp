{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import           Entity.Id
import           Entity.Port
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Value
import qualified Server.Paxos.PaxosNode as P
import qualified SqliteStorage as SS

import           Control.Exception        (AsyncException (..))
import           Control.Exception.Safe   (tryAsync)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           RIO
import           RIO.Text                 (pack, unpack)
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

main :: IO ()
main = do

    hSetBuffering stdout LineBuffering

    (myId, port) <- getArgs <&> \case
        [myId, strPort] -> (pack myId, read strPort)
        _               -> error "Must specify id and port"

    printf "Starting paxos node (%s %d)\n" myId port
    createPaxosNode (Id myId) (Port port) printCallback

printCallback :: Topic -> SequenceNum -> Value -> IO ()
printCallback t s v = printf "%s/%s -> %s\n" (show t) (show s) (show v)

createPaxosNode :: Id
                -> Port
                -> (Topic -> SequenceNum -> Value -> IO ())
                -> IO ()
createPaxosNode (Id i) port callback = do

    http <- newManager defaultManagerSettings
    ls <- SS.create $ unpack i <> "learner"
    as <- SS.create $ unpack i <> "acceptor"
    service <- P.create http port as ls callback

    tryAsync (P.start service) >>= \case

        Left (_ :: AsyncException) -> do
            P.shutdown service
            a <- async $ SS.shutdown ls
            b <- async $ SS.shutdown as
            wait a
            wait b

        Right () -> do
            putStrLn "Normal exit"