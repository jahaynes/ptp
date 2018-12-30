module WebNodeClient where

import NodeApi
import Types

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)

type ProposeClient = ProposeRequest -> IO (Either ServantError (Either String Value))

type PrepareClient = PrepareRequest -> IO (Either ServantError (Either Nack Promise))

type AcceptClient = AcceptRequest -> IO (Either ServantError (Either String Value))

type LearnClient = LearnRequest -> IO (Either ServantError (Maybe Value))

data NodeClient = NodeClient
                { getProposeClient :: ProposeClient
                , getPrepareClient :: PrepareClient
                , getAcceptClient  :: AcceptClient
                , getLearnClient   :: LearnClient
                }

propose :: ProposeRequest -> ClientM (Either String Value)
prepare :: PrepareRequest -> ClientM (Either Nack Promise)
accept ::  AcceptRequest  -> ClientM (Either String Value)
learn ::   LearnRequest   -> ClientM (Maybe Value)
propose :<|> prepare :<|> accept :<|> learn = client (Proxy :: Proxy NodeApi)

makeClients :: Manager -> String -> Int -> NodeClient
makeClients http hostname port =
    let env = mkClientEnv http (BaseUrl Http hostname port "")
    in NodeClient (\prop -> runClientM (propose prop) env)
                  (\prep -> runClientM (prepare prep) env)
                  (\acc  -> runClientM   (accept acc) env)
                  (\lrn  -> runClientM    (learn lrn) env)

