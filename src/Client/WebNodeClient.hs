module Client.WebNodeClient where

import Entity.AcceptRequest
import Entity.CreateTopicRequest
import Entity.CreateTopicResponse
import Entity.LearnRequest
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.ProposeRequest
import Entity.ProposeResponse
import Entity.SubmitRequest
import Entity.SubmitResponse
import Entity.ValueResponse
import Node
import Port
import Server.NodeApi

import Network.HTTP.Client      (Manager)
import Servant
import Servant.Client

type ProposeClient     e =     ProposeRequest -> IO (Either e ProposeResponse)
type PrepareClient     e =     PrepareRequest -> IO (Either e PrepareResponse)
type AcceptClient      e =      AcceptRequest -> IO (Either e ValueResponseE)
type LearnClient       e =       LearnRequest -> IO (Either e ValueResponseM)
type CreateTopicClient e = CreateTopicRequest -> IO (Either e CreateTopicResponse)
type SubmitClient      e =      SubmitRequest -> IO (Either e SubmitResponse)

propose     ::     ProposeRequest -> ClientM ProposeResponse
prepare     ::     PrepareRequest -> ClientM PrepareResponse
accept      ::      AcceptRequest -> ClientM ValueResponseE
learn       ::       LearnRequest -> ClientM ValueResponseM
createTopic :: CreateTopicRequest -> ClientM CreateTopicResponse
submit      ::      SubmitRequest -> ClientM SubmitResponse
propose
    :<|> prepare
    :<|> accept
    :<|> learn
    :<|> createTopic
    :<|> submit = client (Proxy :: Proxy NodeApi)

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

createTopicBuilder :: Manager -> Node -> CreateTopicClient ClientError
createTopicBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\ct -> runClientM (createTopic ct) env)

submitBuilder :: Manager -> Node -> SubmitClient ClientError
submitBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sbm -> runClientM (submit sbm) env)