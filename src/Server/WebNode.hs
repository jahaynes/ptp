module Server.WebNode where

import           Client.WebNodeClient       (NodeClient)
import           Server.NodeApi             (NodeApi)
import qualified Server.Paxos.Acceptor as A
import qualified Server.Paxos.Learner  as L
import qualified Server.Paxos.Proposer as P
import           Types                      (Id)

import Data.Proxy               (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Servant                  (serve)
import Servant.API

data Node e = Node { runNode :: IO () }

create :: Show e => Int
                 -> Int
                 -> Id
                 -> [NodeClient e]
                 -> IO (Node e)
create numAcceptors port myId clients = do
    proposer <- P.create clients
    acceptor <- A.create myId clients
    learner  <- L.create myId numAcceptors
    pure $ Node { runNode = run port $ serve (Proxy :: Proxy NodeApi)
                                     $    P.propose proposer
                                     :<|> A.prepare acceptor
                                     :<|> A.accept acceptor
                                     :<|> A.dbgPurgeKey acceptor
                                     :<|> L.learn learner
                                     :<|> L.check learner
                                     :<|> L.dbgPurgeKey learner
                }
