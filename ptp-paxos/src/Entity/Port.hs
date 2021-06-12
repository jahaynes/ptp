{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Port where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype Port =
    Port Int
        deriving (Eq, Ord, Generic, Hashable, Read, Show, Serialise, NFData, FromJSON, ToJSON)
