{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Value where

import Codec.Serialise     (Serialise)
import RIO                 (Generic, NFData)
import RIO.ByteString.Lazy (ByteString)

newtype Value =
    Value ByteString
        deriving (Eq, Generic, Serialise, NFData, Show)