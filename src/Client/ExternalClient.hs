module Client.ExternalClient where

import Entity.Node
import Entity.Port
import Requests.CreateTopic
import Requests.JoinCluster
import Requests.SubmitCluster
import Server.ExternalApi

import Network.HTTP.Client      (Manager)
import Servant
import Servant.Client

type JoinClusterClient   e =    JoinClusterRequest -> IO (Either e JoinClusterResponse)
type CreateTopicClient   e =    CreateTopicRequest -> IO (Either e CreateTopicResponse)
type SubmitClusterClient e =  SubmitClusterRequest -> IO (Either e SubmitClusterResponse)

joinCluster   ::  JoinClusterRequest   -> ClientM JoinClusterResponse
createTopic   ::  CreateTopicRequest   -> ClientM CreateTopicResponse
submitCluster ::  SubmitClusterRequest -> ClientM SubmitClusterResponse
joinCluster
    :<|> createTopic
    :<|> submitCluster = client (Proxy :: Proxy ExternalApi)

joinClusterBuilder :: Manager -> Node -> JoinClusterClient ClientError
joinClusterBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\jcr -> runClientM (joinCluster jcr) env)

createTopicBuilder :: Manager -> Node -> CreateTopicClient ClientError
createTopicBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\ctr -> runClientM (createTopic ctr) env)

submitClusterBuilder :: Manager -> Node -> SubmitClusterClient ClientError
submitClusterBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sr -> runClientM (submitCluster sr) env)
