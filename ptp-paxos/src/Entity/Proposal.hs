{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Proposal where

import Entity.ProposalNumber
import Entity.Value

import Codec.Serialise (Serialise)
import RIO

data Proposal = Proposal { getProposalNumber :: !ProposalNumber
                         , getProposalValue  :: !Value
                         } deriving (Generic, Serialise, NFData)
