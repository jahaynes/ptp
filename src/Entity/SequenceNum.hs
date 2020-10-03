{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.SequenceNum where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Word       (Word64)
import GHC.Generics    (Generic)

newtype SequenceNum =
    SequenceNum Word64
        deriving (Eq, Ord, Generic, Serialise, NFData, Show)

next :: SequenceNum -> SequenceNum
next (SequenceNum n) = SequenceNum $! n + 1

prev :: SequenceNum -> SequenceNum
prev (SequenceNum n) = SequenceNum $! n - 1