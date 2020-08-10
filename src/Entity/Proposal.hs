{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Proposal where

import Entity.ProposalNumber
import Entity.Value

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

data Proposal = Proposal { getProposalNumber :: !ProposalNumber
                         , getProposalValue  :: !Value }
                             deriving (Generic, Serialise, NFData, Show)
