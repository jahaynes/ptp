{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Port where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype Port =
    Port Int
        deriving (Eq, Ord, Generic, Serialise, NFData)
