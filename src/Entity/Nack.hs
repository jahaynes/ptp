{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Nack where

import Entity.ProposalNumber

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype Nack =
    Nack ProposalNumber
        deriving (Generic, NFData, Serialise, Show)
