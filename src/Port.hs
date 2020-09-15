{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Port where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype Port =
    Port Int
        deriving (Eq, Ord, Generic, Serialise, NFData, Show)
