{-# LANGUAGE FlexibleContexts #-}

module Server.Paxos.PaxosNode where

import           Client.PaxosClient          (acceptBuilder, learnBuilder, prepareBuilder)
import           Entity.Id                   (Id (..))
import           Entity.Port                 (Port (..))
import           Entity.SequenceNum          (SequenceNum (..))
import           Entity.Topic                (Topic (..))
import           Entity.Uniq                 (Uniq (..), uniq)
import           Entity.Value                (Value (..))
import qualified Server.Paxos.Acceptor  as A (Acceptor (..), create)
import qualified Server.Paxos.Learner   as L (Learner (..), create)
import qualified Server.Paxos.Proposer  as P (Proposer (..), create)
import           Server.PaxosApi             (PaxosApi)
import qualified Storage as SS

import           Control.Concurrent.STM (retry)
import           Control.Exception.Safe
import           Network.HTTP.Client  (Manager)
import           Network.Wai (Request, Response, ResponseReceived)
import           Network.Wai.Handler.Warp   (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           RIO hiding (bracket_)
import           Servant

data ServantService =
    ServantService { start    :: !(IO ())
                   , shutdown :: !(IO ())
                   }

create :: ( SS.LockedStorage sl,
            SS.LockedStorage sa) => Manager
                           -> Port
                           -> sa
                           -> sl
                           -> (Topic -> SequenceNum -> Value -> IO ())
                           -> IO ServantService
create http (Port port) acceptorStorage learnerStorage learnedCallback = do

    Uniq u <- uniq
    let myId = Id u

    proposer <- P.create (prepareBuilder http) (acceptBuilder http)
    acceptor <- A.create myId acceptorStorage (learnBuilder http)
    learner  <- L.create learnerStorage learnedCallback

    let settings = setPort port
               --  . setBeforeMainLoop (putMVar ready ())
                 $ defaultSettings

        paxosRoutes = P.propose proposer
                 :<|> A.prepare acceptor
                 :<|> A.accept acceptor
                 :<|> L.learn learner
                 :<|> L.peek learner

    servantState <- ServantState <$> newTVarIO False <*> newTVarIO 0

    let start = runSettings settings
              . middleware servantState
              . serve (Proxy :: Proxy PaxosApi)
              $ paxosRoutes

    let shutdown = do
            atomically $ writeTVar (tHalting servantState) True
            atomically $ do
                ifl <- readTVar (inflight servantState)
                when (ifl > 0) retry

    pure $ ServantService { start    = start
                          , shutdown = shutdown }

data ServantState =
    ServantState { tHalting :: !(TVar Bool)
                 , inflight :: !(TVar Int)
                 }

middleware :: ServantState -> Application -> Application
middleware servantState baseApp req respf = do
    halting <- atomically . readTVar $ tHalting servantState
    if halting
        then error "shutting down"
        else bracket_ a 
                      b 
                      c 
    where
    a = (atomically $ modifyTVar' (inflight servantState) (+1))
    b = (atomically $ modifyTVar' (inflight servantState) (\i -> i - 1))
    c = baseApp req respf