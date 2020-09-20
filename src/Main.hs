import           Client.WebNodeClient             (acceptBuilder, learnBuilder, prepareBuilder)
import           Entity.Id                        (Id (..))
import           Entity.Topic                     (Topic (..))
import qualified Journal                   as J   (create)
import           Node                             (Node (..))
import           Port                             (Port (..))
import           Runner                           (runTests)
import qualified Server.Paxos.Acceptor     as A   (Acceptor (..), create)
import qualified Server.Paxos.Learner      as L   (Learner (..), create)
import qualified Server.Paxos.Proposer     as P   (Proposer (..), create)
import qualified Server.Paxos.StateMachine as SM  (StateMachine (..), create)
import           Server.Locks                     (newLocks)
import           Server.NodeApi                   (NodeApi)

import Control.Concurrent.Async   (async)
import Data.Proxy                 (Proxy (Proxy))
import Network.HTTP.Client hiding (Proxy, port)
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
    learner      <- L.create ident learnerLocks

    http         <- newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000000 }
    journal      <- J.create ident
    stateMachine <- SM.create node http journal proposer learner

    _ <- async . run port $ serve (Proxy :: Proxy NodeApi) $ P.propose proposer
                                                        :<|> A.prepare acceptor
                                                        :<|> A.accept acceptor
                                                        :<|> L.learn learner
                                                        :<|> SM.catchup stateMachine
                                                        :<|> SM.createTopic stateMachine
                                                        :<|> SM.peer stateMachine
                                                        :<|> SM.submit stateMachine
    pure stateMachine

main :: IO ()
main = do

    let allNodes = createIds 5

    stateMachines <- mapM runNode allNodes

    let topic = Topic "some-topic"

    runTests allNodes topic stateMachines
