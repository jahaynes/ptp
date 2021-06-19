{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving #-}

module Entity.Topic where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype Topic =
    Topic Text
        deriving (Eq, Hashable, Generic, Serialise, NFData, Show, FromJSON, ToJSON)
