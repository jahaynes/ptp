import           Client.WebNodeClient             (acceptBuilder, learnBuilder, prepareBuilder)
import           Entity.Id                        (Id (..))
import           Entity.Topic                     (Topic (..))
import qualified Journal                   as J   (create)
import           Node                             (Node (..))
import           Port                             (Port (..))
import           Runner                           (runTests)
import qualified Server.Paxos.Acceptor     as A   (accept, create, prepare)
import qualified Server.Paxos.Learner      as L   (create, learn)
import qualified Server.Paxos.Proposer     as P   (create, propose)
import qualified Server.Paxos.StateMachine as SM  (StateMachine (..), create)
import           Server.Locks                     (newLocks)
import           Server.NodeApi                   (NodeApi)

import Control.Concurrent.Async   (async)
import Data.Proxy                 (Proxy (Proxy))
import Network.Wai.Handler.Warp   (run)
import Servant                    (Handler, serve)
import Servant.API

createIds :: Int -> [Node]
createIds n = do
    let range = take n [8080..]
        ports = map Port range
        ids   = map (Id . show) range
    zipWith Node ids ports

runNode :: Node -> IO (SM.StateMachine Handler)
runNode node@(Node ident (Port port)) = do

    proposer <- P.create prepareBuilder acceptBuilder

    acceptorLocks <- newLocks
    acceptor      <- A.create ident acceptorLocks learnBuilder

    learnerLocks <- newLocks
    learner <- L.create ident learnerLocks

    journal      <- J.create ident
    machineLocks <- newLocks
    stateMachine <- SM.create node journal machineLocks proposer

    _ <- async . run port $ serve (Proxy :: Proxy NodeApi) $ P.propose proposer
                                                        :<|> A.prepare acceptor
                                                        :<|> A.accept acceptor
                                                        :<|> L.learn learner
                                                        :<|> SM.createTopic stateMachine
                                                        :<|> SM.submit stateMachine
    pure stateMachine

main :: IO ()
main = do

    let startingNodes = createIds 5

    stateMachines <- mapM runNode startingNodes

    let topic = Topic "some-topic"

    runTests startingNodes topic stateMachines
