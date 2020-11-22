{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.ProposalNumber where

import Entity.Uniq (Uniq, uniq)

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Word       (Word64)
import GHC.Generics    (Generic)

data ProposalNumber = ProposalNumber
                    { getRoundNo :: !Word64
                    , getUniq    :: !Uniq
                    } deriving (Eq, Ord, Generic, Serialise, NFData)

newProposalNumber :: IO ProposalNumber
newProposalNumber = ProposalNumber 1 <$> uniq
