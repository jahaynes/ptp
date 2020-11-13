{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.SequenceNum where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Hashable   (Hashable)
import Data.Word       (Word64)
import GHC.Generics    (Generic)

newtype SequenceNum =
    SequenceNum Word64
        deriving (Eq, Ord, Hashable, Generic, Serialise, NFData, Show)

next :: SequenceNum -> SequenceNum
next (SequenceNum a) = SequenceNum $! a + 1