{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Id where

import Codec.Serialise
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype Id =
    Id String
        deriving (Eq, Ord, Generic, NFData, Serialise, Show)