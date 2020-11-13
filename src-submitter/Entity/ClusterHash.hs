module Entity.ClusterHash where

import Entity.Node

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Set      (Set, toAscList)

newtype ClusterHash =
    ClusterHash Int
        deriving (Read, Show)

newtype HashNode =
    HashNode Node

instance Hashable HashNode where
    hashWithSalt salt (HashNode (Node i p)) =
        hashWithSalt salt (i, p)

clusterHash :: Set Node -> ClusterHash
clusterHash = ClusterHash . hash . map HashNode . toAscList