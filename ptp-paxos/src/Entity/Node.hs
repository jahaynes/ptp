{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Node where

import Entity.Host
import Entity.Id
import Entity.Port

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO

data Node =
    Node { getId   :: !Id
         , getHost :: !Host
         , getPort :: !Port
         } deriving (Eq, Show, Ord, Generic, Serialise, Hashable, FromJSON, ToJSON)
