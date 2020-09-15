{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Server.Paxos.LearnerState where

import Entity.Id
import Entity.Value

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import GHC.Generics    (Generic)

data LearnerState = AcceptedProposals !(Map Id Value)
                  | Consensus !Value
                      deriving (Generic, NFData, Serialise)
