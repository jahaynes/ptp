module Client.WebNodeClient where

import Entity.AcceptRequest
import Entity.EmptyResponse
import Entity.Key
import Entity.LearnRequest
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.ProposeRequest
import Entity.ValueResponse
import Server.NodeApi

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)

type ProposeClient       e = ProposeRequest -> IO (Either e ValueResponseE)
type PrepareClient       e = PrepareRequest -> IO (Either e PrepareResponse)
type AcceptClient        e =  AcceptRequest -> IO (Either e ValueResponseE)
type PurgeAcceptorClient e =            Key -> IO (Either e EmptyResponse)
type LearnClient         e =   LearnRequest -> IO (Either e ValueResponseM)
type CheckClient         e =            Key -> IO (Either e ValueResponseM)
type PurgeLearnerClient  e =            Key -> IO (Either e EmptyResponse)

data NodeClient e = NodeClient
                  { getProposeClient       :: !(ProposeClient e)
                  , getPrepareClient       :: !(PrepareClient e)
                  , getAcceptClient        :: !(AcceptClient e)
                  , getPurgeAcceptorClient :: !(PurgeAcceptorClient e)
                  , getLearnClient         :: !(LearnClient e)
                  , getCheckClient         :: !(CheckClient e)
                  , getPurgeLearnerClient  :: !(PurgeLearnerClient e)
                  }

propose       :: ProposeRequest -> ClientM ValueResponseE
prepare       :: PrepareRequest -> ClientM PrepareResponse
accept        ::  AcceptRequest -> ClientM ValueResponseE
purgeAcceptor ::            Key -> ClientM EmptyResponse
learn         ::   LearnRequest -> ClientM ValueResponseM
check         ::            Key -> ClientM ValueResponseM
purgeLearner  ::            Key -> ClientM EmptyResponse
propose
    :<|> prepare
    :<|> accept
    :<|> purgeAcceptor
    :<|> learn
    :<|> check
    :<|> purgeLearner = client (Proxy :: Proxy NodeApi)

makeClients :: Manager -> String -> Int -> NodeClient ClientError
makeClients http hostname port =
    let env = mkClientEnv http (BaseUrl Http hostname port "")
    in NodeClient (\prop -> runClientM (propose prop)      env)
                  (\prep -> runClientM (prepare prep)      env)
                  (\acc  -> runClientM (accept acc)        env)
                  (\tpc  -> runClientM (purgeAcceptor tpc) env)
                  (\lrn  -> runClientM (learn lrn)         env)
                  (\tpc  -> runClientM (check tpc)         env)
                  (\tpc  -> runClientM (purgeLearner tpc)  env)
