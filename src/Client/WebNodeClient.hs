module Client.WebNodeClient ( AcceptClient
                            , LearnClient
                            , PrepareClient
                            , PingClient
                            , acceptBuilder
                            , catchupBuilder
                            , createTopicBuilder
                            , joinBuilder
                            , joinClusterBuilder
                            , learnBuilder
                            , prepareBuilder
                            , pingBuilder
                            , readJournalBuilder
                            , sequenceNumBuilder
                            , submitClusterBuilder
                            , submitNodeBuilder
                            ) where

import Entity.Node
import Entity.Port
import Requests.Accept
import Requests.Catchup
import Requests.CreateTopic
import Requests.Join
import Requests.JoinCluster
import Requests.Learn
import Requests.Ping
import Requests.Prepare
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.SubmitCluster
import Requests.SubmitNode
import Server.NodeApi

import Network.HTTP.Client      (Manager)
import Servant
import Servant.Client

type JoinClient          e =           JoinRequest -> IO (Either e JoinResponse)
type JoinClusterClient   e =    JoinClusterRequest -> IO (Either e JoinClusterResponse)
type PingClient          e =                  Ping -> IO (Either e Pong)
type CatchupClient       e =        CatchupRequest -> IO (Either e CatchupResponse)
type CreateTopicClient   e =    CreateTopicRequest -> IO (Either e CreateTopicResponse)
type SubmitClusterClient e =  SubmitClusterRequest -> IO (Either e SubmitClusterResponse)
type SubmitNodeClient    e =     SubmitNodeRequest -> IO (Either e SubmitNodeResponse)
type ReadJournalClient   e =    ReadJournalRequest -> IO (Either e ReadJournalResponse)
type SequenceNumClient   e =    SequenceNumRequest -> IO (Either e SequenceNumResponse)
type PrepareClient       e =        PrepareRequest -> IO (Either e PrepareResponse)
type AcceptClient        e =         AcceptRequest -> IO (Either e AcceptResponse)
type LearnClient         e =          LearnRequest -> IO (Either e LearnResponse)

join          ::  JoinRequest          -> ClientM JoinResponse
joinCluster   ::  JoinClusterRequest   -> ClientM JoinClusterResponse
ping          ::  Ping                 -> ClientM Pong
catchup       ::  CatchupRequest       -> ClientM CatchupResponse
createTopic   ::  CreateTopicRequest   -> ClientM CreateTopicResponse
submitCluster ::  SubmitClusterRequest -> ClientM SubmitClusterResponse
submitNode    ::  SubmitNodeRequest    -> ClientM SubmitNodeResponse
readJournal   ::  ReadJournalRequest   -> ClientM ReadJournalResponse
sequenceNum   ::  SequenceNumRequest   -> ClientM SequenceNumResponse
prepare       :: PrepareRequest        -> ClientM PrepareResponse
accept        ::  AcceptRequest        -> ClientM AcceptResponse
learn         ::   LearnRequest        -> ClientM LearnResponse
join
    :<|> joinCluster
    :<|> ping
    :<|> catchup
    :<|> createTopic
    :<|> submitCluster
    :<|> submitNode
    :<|> readJournal
    :<|> sequenceNum
    :<|> prepare
    :<|> accept
    :<|> learn = client (Proxy :: Proxy NodeApi)

joinBuilder :: Manager -> Node -> JoinClient ClientError
joinBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\jr -> runClientM (join jr) env)

joinClusterBuilder :: Manager -> Node -> JoinClusterClient ClientError
joinClusterBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\jcr -> runClientM (joinCluster jcr) env)

pingBuilder :: Manager -> Node -> PingClient ClientError
pingBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\pr -> runClientM (ping pr) env)

catchupBuilder :: Manager -> Node -> CatchupClient ClientError
catchupBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\cr -> runClientM (catchup cr) env)

createTopicBuilder :: Manager -> Node -> CreateTopicClient ClientError
createTopicBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\ctr -> runClientM (createTopic ctr) env)

submitClusterBuilder :: Manager -> Node -> SubmitClusterClient ClientError
submitClusterBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sr -> runClientM (submitCluster sr) env)

submitNodeBuilder :: Manager -> Node -> SubmitNodeClient ClientError
submitNodeBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sr -> runClientM (submitNode sr) env)

readJournalBuilder :: Manager -> Node -> ReadJournalClient ClientError
readJournalBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\rj -> runClientM (readJournal rj) env)

sequenceNumBuilder :: Manager -> Node -> SequenceNumClient ClientError
sequenceNumBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\snr -> runClientM (sequenceNum snr) env)

prepareBuilder :: Manager -> Node -> PrepareClient ClientError
prepareBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\prep -> runClientM (prepare prep) env)

acceptBuilder :: Manager -> Node -> AcceptClient ClientError
acceptBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\acc -> runClientM (accept acc) env)

learnBuilder :: Manager -> Node -> LearnClient ClientError
learnBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\lrn -> runClientM (learn lrn) env)
