{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Node where

import Entity.Host
import Entity.Id
import Entity.Port

import Codec.Serialise (Serialise)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

data Node =
    Node { getId   :: !Id
         , getHost :: !Host
         , getPort :: !Port
         } deriving (Eq, Read, Show, Ord, Generic, Serialise, Hashable)
