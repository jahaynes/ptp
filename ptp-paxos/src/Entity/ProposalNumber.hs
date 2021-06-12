{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.ProposalNumber where

import Entity.Uniq (Uniq, uniq)

import Codec.Serialise (Serialise)
import RIO

data ProposalNumber = ProposalNumber
                    { getRoundNo :: !Word64
                    , getUniq    :: !Uniq
                    } deriving (Eq, Ord, Generic, Serialise, NFData)

newProposalNumber :: IO ProposalNumber
newProposalNumber = ProposalNumber 1 <$> uniq
