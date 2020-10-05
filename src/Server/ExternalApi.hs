{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.ExternalApi where

import Requests.CreateTopic
import Requests.JoinCluster
import Requests.SubmitCluster
import Servant.API

type ExternalApi = "executor" :> "joinCluster"   :> ReqBody '[OctetStream] JoinClusterRequest   :> Post '[OctetStream] JoinClusterResponse

              :<|> "executor" :> "createTopic"   :> ReqBody '[OctetStream] CreateTopicRequest   :> Post '[OctetStream] CreateTopicResponse

              :<|> "executor" :> "submitCluster" :> ReqBody '[OctetStream] SubmitClusterRequest :> Post '[OctetStream] SubmitClusterResponse
