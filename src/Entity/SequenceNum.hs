{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.SequenceNum where

import Codec.Serialise (Serialise)
import Data.Word       (Word64)
import GHC.Generics    (Generic)

newtype SequenceNum =
    SequenceNum Word64
        deriving (Eq, Ord, Generic, Serialise)
