module Client.WebNodeClient where

import Server.NodeApi
import Types

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)

type ProposeClient e = ProposeRequest -> IO (Either e (Either String Value))

type PrepareClient e = PrepareRequest -> IO (Either e (Either Nack Promise))

type AcceptClient e = AcceptRequest -> IO (Either e (Either String Value))

type LearnClient e = LearnRequest -> IO (Either e (Maybe Value))

type CheckClient e = IO (Either e (Maybe Value))

data NodeClient e = NodeClient
                  { getProposeClient :: !(ProposeClient e)
                  , getPrepareClient :: !(PrepareClient e)
                  , getAcceptClient  :: !(AcceptClient e)
                  , getLearnClient   :: !(LearnClient e)
                  , getCheckClient   :: !(CheckClient e)
                  }

propose :: ProposeRequest -> ClientM (Either String Value)
prepare :: PrepareRequest -> ClientM (Either Nack Promise)
accept  ::  AcceptRequest -> ClientM (Either String Value)
learn   ::   LearnRequest -> ClientM (Maybe Value)
check   ::                   ClientM (Maybe Value)
propose :<|> prepare :<|> accept :<|> learn :<|> check = client (Proxy :: Proxy NodeApi)

makeClients :: Manager -> String -> Int -> NodeClient ClientError
makeClients http hostname port =
    let env = mkClientEnv http (BaseUrl Http hostname port "")
    in NodeClient (\prop -> runClientM (propose prop) env)
                  (\prep -> runClientM (prepare prep) env)
                  (\acc  -> runClientM (accept acc)   env)
                  (\lrn  -> runClientM (learn lrn)    env)
                           (runClientM  check         env)
