{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Topic where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson      (FromJSON, ToJSON)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

newtype Topic =
    Topic String
        deriving (Eq, Ord, Hashable, Generic, Serialise, NFData, Show, FromJSON, ToJSON)
