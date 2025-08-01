{-# LANGUAGE LambdaCase #-}

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
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           RIO hiding (tryAnyDeep)
import           RIO.Text                 (pack, unpack)
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
            createPaxosNode http (Id $ pack myId) (Port port) $ \t s v ->
                printf "%s/%s -> %s\n" (show t) (show s) (show v)

        _ -> error "Must specify id and port"

createPaxosNode :: Manager
                -> Id
                -> Port
                -> (Topic -> SequenceNum -> Value -> IO ())
                -> IO ()
createPaxosNode http (Id i) port callback = do
    ls <- SS.create $ unpack i <> "learner"
    as <- SS.create $ unpack i <> "acceptor"

    result <- tryAsync $ do
                a <- P.create http port as ls callback
                wait a

    shutdown ls as result

shutdown :: SS.SqliteStorage -> SS.SqliteStorage -> Either AsyncException () -> IO ()
shutdown ls as result = do

    -- shutdown sentinel
    -- Prevent future writes from here?

    let cleanup =
            case result of
                Right ()           -> True
                Left StackOverflow -> False
                Left HeapOverflow  -> False
                Left ThreadKilled  -> True
                Left UserInterrupt -> True

    when cleanup $ do
        putStrLn "Cleaning up Acceptor state"
        SS.vacuumAndClose as
        putStrLn "Cleaning up Learner state"
        SS.vacuumAndClose ls
        putStrLn "Done"
