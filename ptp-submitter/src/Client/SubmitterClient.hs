module Client.SubmitterClient ( CreateTopicClient
                              , SyncClient
                              , SubmitClient
                              , StateClient
                              , createTopicBuilder
                              , forgetLeaderBuilder
                              , syncBuilder
                              , submitBuilder
                              , stateBuilder ) where

import Requests.CreateTopic  (CreateTopicRequest, CreateTopicResponse)
import Requests.ForgetLeader (ForgetLeaderRequest, ForgetLeaderResponse)
import Requests.State        (StateResponse)
import Requests.Submit       (SubmitRequest, SubmitResponse)
import Requests.Sync         (SyncRequest, SyncResponse)
import SubmitterApi          (SubmitterApi)

import Entity.Host (getHostString)
import Entity.Node (Node (..))
import Entity.Port (Port (Port))

import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

type CreateTopicClient e  = CreateTopicRequest  -> IO (Either e CreateTopicResponse)
type SyncClient e         = SyncRequest         -> IO (Either e SyncResponse)
type SubmitClient e       = SubmitRequest       -> IO (Either e SubmitResponse)
type StateClient e        =                        IO (Either e StateResponse)
type ForgetLeaderClient e = ForgetLeaderRequest -> IO (Either e ForgetLeaderResponse)

createTopic  :: CreateTopicRequest  -> ClientM CreateTopicResponse
sync         :: SyncRequest         -> ClientM SyncResponse
submit       :: SubmitRequest       -> ClientM SubmitResponse
state        ::                        ClientM StateResponse
forgetLeader :: ForgetLeaderRequest -> ClientM ForgetLeaderResponse
createTopic
    :<|> sync
    :<|> submit
    :<|> state
    :<|> forgetLeader = client (Proxy :: Proxy SubmitterApi)

createTopicBuilder :: Manager -> Node -> CreateTopicClient ClientError
createTopicBuilder http node = do
    let h = getHostString $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\ctr -> runClientM (createTopic ctr) env)

syncBuilder :: Manager -> Node -> SyncClient ClientError
syncBuilder http node = do
    let h = getHostString $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\sr -> runClientM (sync sr) env)

submitBuilder :: Manager -> Node -> SubmitClient ClientError
submitBuilder http node = do
    let h = getHostString $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\sr -> runClientM (submit sr) env)

stateBuilder :: Manager -> Node -> StateClient ClientError
stateBuilder http node = do
    let h = getHostString $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    runClientM state env

forgetLeaderBuilder :: Manager -> Node -> ForgetLeaderClient ClientError
forgetLeaderBuilder http node = do
    let h = getHostString $ getHost node
        Port p = getPort node
        env = mkClientEnv http (BaseUrl Http h p "")
    (\flr -> runClientM (forgetLeader flr) env)