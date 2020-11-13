{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.PaxosApi where

import Requests.Accept
import Requests.Learn
import Requests.Peek
import Requests.Prepare
import Requests.Propose
import Servant.API

type PaxosApi = "proposer" :> "propose" :> ReqBody '[OctetStream] ProposeRequest :> Post '[OctetStream] ProposeResponse

           :<|> "acceptor" :> "prepare" :> ReqBody '[OctetStream] PrepareRequest :> Post '[OctetStream] PrepareResponse

           :<|> "acceptor" :> "accept"  :> ReqBody '[OctetStream] AcceptRequest  :> Post '[OctetStream] AcceptResponse

           :<|> "learner"  :> "learn"   :> ReqBody '[OctetStream] LearnRequest   :> Post '[OctetStream] LearnResponse

           :<|> "learner"  :> "peek"    :> ReqBody '[OctetStream] PeekRequest    :> Post '[OctetStream] PeekResponse
