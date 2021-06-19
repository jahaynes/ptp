{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving #-}

module Entity.Id where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

newtype Id =
    Id Text
        deriving (Eq, Ord, Generic, Hashable, NFData, Serialise, Show, FromJSON, ToJSON)