{-# LANGUAGE FlexibleContexts #-}

module Server.Paxos.PaxosNode where

import           Client.PaxosClient          (acceptBuilder, learnBuilder, prepareBuilder)
import           Entity.Id                   (Id (..))
import           Entity.Port                 (Port (..))
import           Entity.SequenceNum          (SequenceNum (..))
import           Entity.Topic                (Topic (..))
import           Entity.Uniq                 (Uniq (..), uniq)
import           Entity.Value                (Value (..))
import qualified Server.Paxos.Acceptor  as A (Acceptor (..), AcceptorState, create)
import qualified Server.Paxos.Learner   as L (Learner (..), LearnerState, create)
import qualified Server.Paxos.Proposer  as P (Proposer (..), create)
import           Server.PaxosApi             (PaxosApi)
import           Server.Storage              (Storage)

import           Control.Concurrent         (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async   (Async, async)
import           Network.HTTP.Client        (Manager)
import           Network.Wai.Handler.Warp   (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           Servant

create :: ( Storage sa A.AcceptorState,
            Storage sl L.LearnerState) => Manager
                                       -> Port
                                       -> sa
                                       -> sl
                                       -> (Topic -> SequenceNum -> Value -> IO ())
                                       -> IO (Async ())
create http (Port port) acceptorStorage learnerStorage learnedCallback = do

    Uniq u <- uniq
    let myId = Id u

    proposer <- P.create (prepareBuilder http) (acceptBuilder http)
    acceptor <- A.create myId acceptorStorage (learnBuilder http)
    learner  <- L.create learnerStorage learnedCallback

    ready <- newEmptyMVar

    let settings = setPort port
                 . setBeforeMainLoop (putMVar ready ())
                 $ defaultSettings

        paxosRoutes = P.propose proposer
                 :<|> A.prepare acceptor
                 :<|> A.accept acceptor
                 :<|> L.learn learner
                 :<|> L.peek learner

    a <- async . runSettings settings $
        serve (Proxy :: Proxy PaxosApi) paxosRoutes

    takeMVar ready

    pure a
