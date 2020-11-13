module Client.SubmitterClient ( CreateTopicClient
                              , SyncClient
                              , SubmitClient
                              , createTopicBuilder
                              , syncBuilder
                              , submitBuilder ) where

import Requests.CreateTopic (CreateTopicRequest, CreateTopicResponse)
import Requests.Submit      (SubmitRequest, SubmitResponse)
import Requests.Sync        (SyncRequest, SyncResponse)
import SubmitterApi         (SubmitterApi)

import Entity.Node (Node (Node))
import Entity.Port (Port (Port))

import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

type CreateTopicClient e = CreateTopicRequest -> IO (Either e CreateTopicResponse)
type SyncClient e        = SyncRequest        -> IO (Either e SyncResponse)
type SubmitClient e      = SubmitRequest      -> IO (Either e SubmitResponse)

createTopic :: CreateTopicRequest -> ClientM CreateTopicResponse
sync        :: SyncRequest        -> ClientM SyncResponse
submit      :: SubmitRequest      -> ClientM SubmitResponse
createTopic
    :<|> sync
    :<|> submit = client (Proxy :: Proxy SubmitterApi)

createTopicBuilder :: Manager -> Node -> CreateTopicClient ClientError
createTopicBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\ctr -> runClientM (createTopic ctr) env)

syncBuilder :: Manager -> Node -> SyncClient ClientError
syncBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sr -> runClientM (sync sr) env)

submitBuilder :: Manager -> Node -> SubmitClient ClientError
submitBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\sr -> runClientM (submit sr) env)
