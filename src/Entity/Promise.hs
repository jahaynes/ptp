{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Promise where

import Entity.Proposal
import Entity.ProposalNumber

import Codec.Serialise
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

data Promise = Promise { prom_notLessThan     :: !ProposalNumber
                       , prom_highestProposal :: !(Maybe Proposal) }
                           deriving (Generic, NFData, Serialise)