{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.SequenceNum where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype SequenceNum =
    SequenceNum Word64
        deriving (Eq, Ord, Hashable, Generic, Serialise, NFData, Show, FromJSON, ToJSON)

next :: SequenceNum -> SequenceNum
next (SequenceNum a) = SequenceNum $ a + 1