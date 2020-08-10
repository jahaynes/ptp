{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.NodeApi where

import Entity.AcceptRequest
import Entity.EmptyResponse
import Entity.Key
import Entity.LearnRequest
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.ProposeRequest
import Entity.ValueResponse

import Servant.API

type NodeApi = "proposer" :> "propose" :> ReqBody '[OctetStream] ProposeRequest :> Post '[OctetStream] ValueResponseE

          :<|> "acceptor" :> "prepare" :> ReqBody '[OctetStream] PrepareRequest :> Post '[OctetStream] PrepareResponse

          :<|> "acceptor" :> "accept"  :> ReqBody '[OctetStream] AcceptRequest  :> Post '[OctetStream] ValueResponseE

          :<|> "acceptor" :> "purge"   :> ReqBody '[OctetStream] Key            :> Post '[OctetStream] EmptyResponse

          :<|> "learner"  :> "learn"   :> ReqBody '[OctetStream] LearnRequest   :> Post '[OctetStream] ValueResponseM

          :<|> "learner"  :> "check"   :> ReqBody '[OctetStream] Key            :> Get  '[OctetStream] ValueResponseM

          :<|> "learner"  :> "purge"   :> ReqBody '[OctetStream] Key            :> Post '[OctetStream] EmptyResponse
