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

import           Network.HTTP.Client        (Manager)
import           Network.Wai.Handler.Warp hiding (Port)  -- (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           Network.Wai.Internal (Request) -- todo don't internal
import           RIO
import           Servant

import qualified Storage as SS

{-
    TODO return this instead of a raw Async()
    this safeShutdown should:
        safely wait on router
        safely wait on acceptor
        safely wait on learner
-}

-- TODO do we really need a beforemainloop?

data PaxosNode =
    PaxosNode { safeShutdown :: !(IO ()) }

create :: ( SS.LockedStorage sl,
            SS.LockedStorage sa) => Manager
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

    -- readyToServe <- newEmptyMVar

    -- readyToClose <- newEmptyMVar

    let settings = setPort port
          --     . setBeforeMainLoop (putMVar readyToServe ())
          --     . setInstallShutdownHandler (shutdownHandler readyToClose)
          --     . setOnClose onClose
                 $ defaultSettings

        paxosRoutes = P.propose proposer
                 :<|> A.prepare acceptor
                 :<|> A.accept acceptor
                 :<|> L.learn learner
                 :<|> L.peek learner

    a <- async . runSettings settings $
        serve (Proxy :: Proxy PaxosApi) paxosRoutes

    -- takeMVar readyToServe

    pure a

{-
shutdownHandler readyToClose closeSocket = do
    _ <- async $ do
        takeMVar readyToClose
        closeSocket
    pure ()
-}