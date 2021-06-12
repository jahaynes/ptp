{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Topic where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype Topic =
    Topic String
        deriving (Eq, Ord, Hashable, Generic, Serialise, NFData, Show, FromJSON, ToJSON)
