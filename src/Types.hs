{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Types where

import ProposalNumber

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Id = Id String
                 deriving (Eq, Ord, Generic, FromJSON, ToJSON)

newtype ProposeRequest = ProposeRequest Value
                             deriving (Generic, FromJSON, ToJSON)

newtype PrepareRequest = PrepareRequest ProposalNumber
                             deriving (Generic, FromJSON, ToJSON)

data AcceptRequest = AcceptRequest !ProposalNumber !Value
                          deriving (Generic, FromJSON, ToJSON)

newtype Value = Value String
                    deriving (Eq, Generic, FromJSON, ToJSON, Show)

data Proposal = Proposal { getProposalNumber :: !ProposalNumber
                         , getProposalValue  :: !Value }
                             deriving (Generic, FromJSON, ToJSON)

data Promise = Promise { prom_notLessThan     :: !ProposalNumber
                       , prom_highestProposal :: !(Maybe Proposal) }
                           deriving (Generic, FromJSON, ToJSON)

data LearnRequest = LearnRequest !Id !Value
                           deriving (Generic, FromJSON, ToJSON)

newtype Nack = Nack ProposalNumber
                   deriving (Generic, FromJSON, ToJSON, Show)

newtype ErrMsg = ErrMsg String deriving Show
