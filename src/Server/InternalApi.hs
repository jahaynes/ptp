{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Server.InternalApi where

import Requests.Accept
import Requests.Catchup
import Requests.Join
import Requests.Learn
import Requests.Ping
import Requests.Prepare
import Requests.ReadJournal
import Requests.SequenceNum
import Requests.SubmitNode
import Servant.API

type InternalApi = "executor" :> "join"          :> ReqBody '[OctetStream] JoinRequest          :> Post '[OctetStream] JoinResponse

              :<|> "executor" :> "ping"          :> ReqBody '[OctetStream] Ping                 :> Post '[OctetStream] Pong

              :<|> "executor" :> "catchup"       :> ReqBody '[OctetStream] CatchupRequest       :> Post '[OctetStream] CatchupResponse

              :<|> "executor" :> "submitNode"    :> ReqBody '[OctetStream] SubmitNodeRequest    :> Post '[OctetStream] SubmitNodeResponse

              :<|> "executor" :> "readJournal"   :> ReqBody '[OctetStream] ReadJournalRequest   :> Post '[OctetStream] ReadJournalResponse

              :<|> "executor" :> "sequenceNum"   :> ReqBody '[OctetStream] SequenceNumRequest   :> Post '[OctetStream] SequenceNumResponse

              :<|> "acceptor" :> "prepare"       :> ReqBody '[OctetStream] PrepareRequest       :> Post '[OctetStream] PrepareResponse

              :<|> "acceptor" :> "accept"        :> ReqBody '[OctetStream] AcceptRequest        :> Post '[OctetStream] AcceptResponse

              :<|> "learner"  :> "learn"         :> ReqBody '[OctetStream] LearnRequest         :> Post '[OctetStream] LearnResponse
