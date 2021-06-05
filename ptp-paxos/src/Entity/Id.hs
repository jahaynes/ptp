{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Id where

import Codec.Serialise
import Control.DeepSeq (NFData)
import Data.Aeson      (FromJSON, ToJSON)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

newtype Id =
    Id String
        deriving (Eq, Ord, Generic, Hashable, NFData, Read, Serialise, Show, FromJSON, ToJSON)