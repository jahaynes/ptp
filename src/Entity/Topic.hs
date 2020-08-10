{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Topic where

import Codec.Serialise (Serialise)
import GHC.Generics    (Generic)

newtype Topic =
    Topic String
        deriving (Eq, Ord, Generic, Serialise)
