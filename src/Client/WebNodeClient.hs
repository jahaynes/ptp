module Client.WebNodeClient where

import Entity.Node
import Entity.Port
import Requests.Accept
import Requests.CreateTopic
import Requests.Join
import Requests.Learn
import Requests.Ping
import Requests.Prepare
import Requests.Propose
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.SubmitCluster
import Requests.SubmitNode
import Server.NodeApi

import Network.HTTP.Client      (Manager)
import Servant
import Servant.Client

type JoinClient        e =        JoinRequest -> IO (Either e JoinResponse)
type PingClient        e =               Ping -> IO (Either e Pong)
type CreateTopicClient e = CreateTopicRequest -> IO (Either e CreateTopicResponse)
type SubmitClusterClient  e =  SubmitClusterRequest -> IO (Either e SubmitClusterResponse)
type SubmitNodeClient  e =  SubmitNodeRequest -> IO (Either e SubmitNodeResponse)
type ReadJournalClient e = ReadJournalRequest -> IO (Either e ReadJournalResponse)
type SequenceNumClient e = SequenceNumRequest -> IO (Either e SequenceNumResponse)
type ProposeClient     e =     ProposeRequest -> IO (Either e ProposeResponse)
type PrepareClient     e =     PrepareRequest -> IO (Either e PrepareResponse)
type AcceptClient      e =      AcceptRequest -> IO (Either e AcceptResponse)
type LearnClient       e =       LearnRequest -> IO (Either e LearnResponse)

join          ::  JoinRequest          -> ClientM JoinResponse
ping          ::  Ping                 -> ClientM Pong
createTopic   ::  CreateTopicRequest   -> ClientM CreateTopicResponse
submitCluster ::  SubmitClusterRequest -> ClientM SubmitClusterResponse
submitNode    ::  SubmitNodeRequest    -> ClientM SubmitNodeResponse
readJournal   ::  ReadJournalRequest   -> ClientM ReadJournalResponse
sequenceNum   ::  SequenceNumRequest   -> ClientM SequenceNumResponse
propose       :: ProposeRequest        -> ClientM ProposeResponse
prepare       :: PrepareRequest        -> ClientM PrepareResponse
accept        ::  AcceptRequest        -> ClientM AcceptResponse
learn         ::   LearnRequest        -> ClientM LearnResponse
join
    :<|> ping
    :<|> createTopic
    :<|> submitCluster
    :<|> submitNode
    :<|> readJournal
    :<|> sequenceNum
    :<|> propose
    :<|> prepare
    :<|> accept
    :<|> learn = client (Proxy :: Proxy NodeApi)

joinBuilder :: Manager -> Node -> JoinClient ClientError
joinBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\jr -> runClientM (join jr) env)

pingBuilder :: Manager -> Node -> PingClient ClientError
pingBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\pr -> runClientM (ping pr) env)

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

proposeBuilder :: Manager -> Node -> ProposeClient ClientError
proposeBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\prop -> runClientM (propose prop) env)

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
