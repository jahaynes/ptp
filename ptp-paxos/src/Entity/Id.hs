{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Id where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype Id =
    Id String
        deriving (Eq, Ord, Generic, Hashable, NFData, Read, Serialise, Show, FromJSON, ToJSON)