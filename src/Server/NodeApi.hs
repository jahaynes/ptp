{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.NodeApi where

import Requests.Accept
import Requests.Catchup
import Requests.CreateTopic
import Requests.Join
import Requests.JoinCluster
import Requests.Learn
import Requests.Ping
import Requests.Prepare
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.SubmitCluster
import Requests.SubmitNode
import Servant.API

-- TODO internal/external API

type NodeApi = "executor" :> "join"          :> ReqBody '[OctetStream] JoinRequest          :> Post '[OctetStream] JoinResponse

          :<|> "executor" :> "joinCluster"   :> ReqBody '[OctetStream] JoinClusterRequest   :> Post '[OctetStream] JoinClusterResponse

          :<|> "executor" :> "ping"          :> ReqBody '[OctetStream] Ping                 :> Post '[OctetStream] Pong

          :<|> "executor" :> "catchup"       :> ReqBody '[OctetStream] CatchupRequest       :> Post '[OctetStream] CatchupResponse

          :<|> "executor" :> "createTopic"   :> ReqBody '[OctetStream] CreateTopicRequest   :> Post '[OctetStream] CreateTopicResponse

          :<|> "executor" :> "submitCluster" :> ReqBody '[OctetStream] SubmitClusterRequest :> Post '[OctetStream] SubmitClusterResponse

          :<|> "executor" :> "submitNode"    :> ReqBody '[OctetStream] SubmitNodeRequest    :> Post '[OctetStream] SubmitNodeResponse

          :<|> "executor" :> "readJournal"   :> ReqBody '[OctetStream] ReadJournalRequest   :> Post '[OctetStream] ReadJournalResponse

          :<|> "executor" :> "sequenceNum"   :> ReqBody '[OctetStream] SequenceNumRequest   :> Post '[OctetStream] SequenceNumResponse

          :<|> "acceptor" :> "prepare"       :> ReqBody '[OctetStream] PrepareRequest       :> Post '[OctetStream] PrepareResponse

          :<|> "acceptor" :> "accept"        :> ReqBody '[OctetStream] AcceptRequest        :> Post '[OctetStream] AcceptResponse

          :<|> "learner"  :> "learn"         :> ReqBody '[OctetStream] LearnRequest         :> Post '[OctetStream] LearnResponse
