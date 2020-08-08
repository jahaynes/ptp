{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Types where

import ProposalNumber
import SequenceNum
import Topic

import Codec.Serialise
import Control.DeepSeq (NFData)
import Data.Aeson      (FromJSON, ToJSON)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

newtype Id =
    Id String
        deriving (Eq, Ord, Generic, FromJSON, ToJSON, NFData, Serialise, Show)

data ProposeRequest =
    ProposeRequest !Key !Value
        deriving (Generic, FromJSON, ToJSON)

data PrepareRequest =
    PrepareRequest !Key !ProposalNumber
        deriving (Generic, FromJSON, ToJSON)

data AcceptRequest =
    AcceptRequest !Key !ProposalNumber !Value
        deriving (Generic, FromJSON, ToJSON)

data Key =
    Key !Topic !SequenceNum
        deriving (Eq, Generic, Hashable, FromJSON, ToJSON)

newtype Value = Value String
                    deriving (Eq, Generic, Serialise, FromJSON, ToJSON, NFData, Show)

data Proposal = Proposal { getProposalNumber :: !ProposalNumber
                         , getProposalValue  :: !Value }
                             deriving (Generic, Serialise, FromJSON, ToJSON, NFData, Show)

data Promise = Promise { prom_notLessThan     :: !ProposalNumber
                       , prom_highestProposal :: !(Maybe Proposal) }
                           deriving (Generic, FromJSON, ToJSON)

data LearnRequest =
    LearnRequest !Key !Id !Value
        deriving (Generic, FromJSON, ToJSON)

newtype Nack = Nack ProposalNumber
                   deriving (Generic, FromJSON, ToJSON, Show)

newtype ErrMsg = ErrMsg String deriving Show
