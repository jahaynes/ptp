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

import Entity.Host (getHostSafe)
import Entity.Node (Node (..))
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
createTopicBuilder http node = do
    let h = getHostSafe $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\ctr -> runClientM (createTopic ctr) env)

syncBuilder :: Manager -> Node -> SyncClient ClientError
syncBuilder http node = do
    let h = getHostSafe $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\sr -> runClientM (sync sr) env)

submitBuilder :: Manager -> Node -> SubmitClient ClientError
submitBuilder http node = do
    let h = getHostSafe $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\sr -> runClientM (submit sr) env)

