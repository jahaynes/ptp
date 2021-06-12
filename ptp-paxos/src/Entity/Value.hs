{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Value where

import Codec.Serialise (Serialise)
import RIO

newtype Value =
    Value String
        deriving (Eq, Generic, Serialise, NFData, Show)