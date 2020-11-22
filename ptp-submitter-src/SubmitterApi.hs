{-# LANGUAGE DataKinds,
             TypeOperators #-}

module SubmitterApi where

import Requests.CreateTopic
import Requests.Submit
import Requests.Sync

import Servant.API

type SubmitterApi = "submitter" :> "createTopic" :> ReqBody '[OctetStream] CreateTopicRequest :> Post '[OctetStream] CreateTopicResponse

               :<|> "submitter" :> "sync"        :> ReqBody '[OctetStream] SyncRequest        :> Post '[OctetStream] SyncResponse

               :<|> "submitter" :> "submit"      :> ReqBody '[OctetStream] SubmitRequest      :> Post '[OctetStream] SubmitResponse
