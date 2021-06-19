{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity.ClusterHash where

import Entity.Node

import Codec.Serialise (Serialise)
import Data.Hashable   (hash)
import RIO.Set

newtype ClusterHash =
    ClusterHash Int
        deriving (Serialise, Show)

clusterHash :: Set Node -> ClusterHash
clusterHash = ClusterHash . hash . toAscList