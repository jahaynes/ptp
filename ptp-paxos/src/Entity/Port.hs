{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Port where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson      (FromJSON, ToJSON)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

newtype Port =
    Port Int
        deriving (Eq, Ord, Generic, Hashable, Read, Show, Serialise, NFData, FromJSON, ToJSON)
