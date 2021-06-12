module Entity.ClusterHash where

import Entity.Node

import Data.Hashable (hash)
import RIO.Set

newtype ClusterHash =
    ClusterHash Int
        deriving (Read, Show)

clusterHash :: Set Node -> ClusterHash
clusterHash = ClusterHash . hash . toAscList