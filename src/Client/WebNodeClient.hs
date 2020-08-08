module Client.WebNodeClient where

import Server.NodeApi
import Types

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)


type ProposeClient       e = ProposeRequest -> IO (Either e (Either String Value))
type PrepareClient       e = PrepareRequest -> IO (Either e (Either Nack Promise))
type AcceptClient        e =  AcceptRequest -> IO (Either e (Either String Value))
type PurgeAcceptorClient e =            Key -> IO (Either e ())
type LearnClient         e =   LearnRequest -> IO (Either e (Maybe Value))
type CheckClient         e =            Key -> IO (Either e (Maybe Value))
type PurgeLearnerClient  e =            Key -> IO (Either e ())

data NodeClient e = NodeClient
                  { getProposeClient       :: !(ProposeClient e)
                  , getPrepareClient       :: !(PrepareClient e)
                  , getAcceptClient        :: !(AcceptClient e)
                  , getPurgeAcceptorClient :: !(PurgeAcceptorClient e)
                  , getLearnClient         :: !(LearnClient e)
                  , getCheckClient         :: !(CheckClient e)
                  , getPurgeLearnerClient  :: !(PurgeLearnerClient e)
                  }


propose       :: ProposeRequest -> ClientM (Either String Value)
prepare       :: PrepareRequest -> ClientM (Either Nack Promise)
accept        ::  AcceptRequest -> ClientM (Either String Value)
purgeAcceptor ::            Key -> ClientM ()
learn         ::   LearnRequest -> ClientM (Maybe Value)
check         ::            Key -> ClientM (Maybe Value)
purgeLearner  ::            Key -> ClientM ()
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

