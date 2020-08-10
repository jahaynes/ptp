{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Server.Paxos.LearnerState where

import Entity.Id
import Entity.Value

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import GHC.Generics    (Generic)

data LearnerState =
    LearnerState { lrn_acceptedProposals :: !(Map Id Value)
                 , lrn_consensus         :: !(Maybe Value) }
                     deriving (Generic, NFData, Serialise)