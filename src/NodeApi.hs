{-# LANGUAGE DataKinds,
             TypeOperators #-}

module NodeApi where

import Types

import Servant.API

type NodeApi = "proposer" :> "propose" :> ReqBody '[JSON] ProposeRequest :> Post '[JSON] (Either String Value)

          :<|> "acceptor" :> "prepare" :> ReqBody '[JSON] PrepareRequest :> Post '[JSON] (Either Nack Promise)

          :<|> "acceptor" :> "accept"  :> ReqBody '[JSON] AcceptRequest  :> Post '[JSON] (Either String Value)

          :<|> "learner"  :> "learn"   :> ReqBody '[JSON] LearnRequest   :> Post '[JSON] (Maybe Value)
