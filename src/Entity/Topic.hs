{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Topic where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype Topic =
    Topic String
        deriving (Eq, Ord, Generic, Serialise, NFData, Show)
