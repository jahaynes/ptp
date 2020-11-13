module Client.PaxosClient ( ProposeClient
                          , PrepareClient
                          , AcceptClient
                          , LearnClient
                          , PeekClient
                          , proposeBuilder
                          , prepareBuilder
                          , acceptBuilder
                          , learnBuilder
                          , peekBuilder
                          ) where

import Entity.Node      (Node (Node))
import Entity.Port      (Port (Port))
import Requests.Accept  (AcceptRequest, AcceptResponse)
import Requests.Learn   (LearnRequest, LearnResponse)
import Requests.Peek    (PeekRequest, PeekResponse)
import Requests.Prepare (PrepareRequest, PrepareResponse)
import Requests.Propose (ProposeRequest, ProposeResponse)
import Server.PaxosApi  (PaxosApi)

import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

type ProposeClient e = ProposeRequest -> IO (Either e ProposeResponse)
type PrepareClient e = PrepareRequest -> IO (Either e PrepareResponse)
type AcceptClient  e =  AcceptRequest -> IO (Either e AcceptResponse)
type LearnClient   e =   LearnRequest -> IO (Either e LearnResponse)
type PeekClient    e =   PeekRequest  -> IO (Either e PeekResponse)

propose :: ProposeRequest -> ClientM ProposeResponse
prepare :: PrepareRequest -> ClientM PrepareResponse
accept  ::  AcceptRequest -> ClientM AcceptResponse
learn   ::   LearnRequest -> ClientM LearnResponse
peek    ::    PeekRequest -> ClientM PeekResponse
propose
    :<|> prepare
    :<|> accept
    :<|> learn
    :<|> peek = client (Proxy :: Proxy PaxosApi)

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

peekBuilder :: Manager -> Node -> PeekClient ClientError
peekBuilder http (Node _ (Port p)) = do
    let env = mkClientEnv http (BaseUrl Http "127.0.0.1" p "")
    (\pr -> runClientM (peek pr) env)