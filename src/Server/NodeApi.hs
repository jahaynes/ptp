{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.NodeApi where

import Entity.AcceptRequest
import Entity.CatchupRequest
import Entity.CatchupResponse
import Entity.CreateTopicRequest
import Entity.CreateTopicResponse
import Entity.LearnRequest
import Entity.PeerRequest
import Entity.PeerResponse
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.ProposeRequest
import Entity.ProposeResponse
import Entity.SubmitRequest
import Entity.SubmitResponse
import Entity.ValueResponse

import Servant.API

type NodeApi = "proposer" :> "propose"     :> ReqBody '[OctetStream] ProposeRequest     :> Post '[OctetStream] ProposeResponse

          :<|> "acceptor" :> "prepare"     :> ReqBody '[OctetStream] PrepareRequest     :> Post '[OctetStream] PrepareResponse

          :<|> "acceptor" :> "accept"      :> ReqBody '[OctetStream] AcceptRequest      :> Post '[OctetStream] ValueResponseE

          :<|> "learner"  :> "learn"       :> ReqBody '[OctetStream] LearnRequest       :> Post '[OctetStream] ValueResponseM

          :<|> "machine"  :> "catchup"     :> ReqBody '[OctetStream] CatchupRequest     :> Post '[OctetStream] CatchupResponse

          :<|> "machine"  :> "createTopic" :> ReqBody '[OctetStream] CreateTopicRequest :> Post '[OctetStream] CreateTopicResponse

          :<|> "machine"  :> "peerRequest" :> ReqBody '[OctetStream] PeerRequest        :> Post '[OctetStream] PeerResponse

          :<|> "machine"  :> "submit"      :> ReqBody '[OctetStream] SubmitRequest      :> Post '[OctetStream] SubmitResponse
