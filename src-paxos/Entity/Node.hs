{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Node where

import Entity.Id
import Entity.Port

import Codec.Serialise (Serialise)
import GHC.Generics    (Generic)

data Node =
    Node !Id !Port
        deriving (Eq, Read, Show, Ord, Generic, Serialise)
