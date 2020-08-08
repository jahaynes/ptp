{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Server.Paxos.AcceptorState where

import ProposalNumber
import Types

import Control.DeepSeq (NFData)
import Codec.Serialise
import GHC.Generics    (Generic)  

data AcceptorState =
    AcceptorState { acc_notLessThan :: !(Maybe ProposalNumber)
                  , acc_proposal    :: !(Maybe Proposal) }
        deriving (Generic, NFData, Serialise, Show)
