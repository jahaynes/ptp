{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Value where

import Entity.Id   (Id (..))
import Entity.Node (Node (..))

import Codec.Serialise    (Serialise, serialise)
import Control.DeepSeq    (NFData)
import Data.Hashable      (Hashable, hash)
import Data.Set           (Set)
import Data.UUID.V4       (nextRandom)
import GHC.Generics       (Generic)
import Text.Printf        (printf)

data Value = Value !ClusterHash !Uniq !Val
           | Broken
               deriving (Eq, Generic, Serialise, NFData)

newtype ClusterHash =
    ClusterHash Int
        deriving (Eq, Generic, Serialise, NFData)

instance Show ClusterHash where
    show (ClusterHash i) = printf "ch%d" i

clusterHash :: Maybe Node -> Set Node -> ClusterHash
clusterHash mLeader cluster = ClusterHash
                            . hash
                            . serialise
                            $ (mLeader, cluster)

newtype Uniq =
    Uniq String
        deriving (Eq, Hashable, Generic, Serialise, NFData)

uniq :: IO Uniq
uniq = Uniq . show <$> nextRandom

data Val = SimpleValue !String
         | CommandValue !Command
             deriving (Eq, Generic, Serialise, NFData)

instance Show Value where
    show Broken               = "<broken>"
    show (Value c (Uniq u) v) = printf "%s-%s-%s" (show c) u (show v)

instance Show Val where
    show (SimpleValue s)  = s
    show (CommandValue c) = show c

data Command = ElectLeader  !Node
             | AddNode      !Node
                deriving (Eq, Generic, Serialise, NFData)

instance Show Command where

    show (ElectLeader (Node (Id i) _)) = printf "Leader=%s" i

    show (AddNode node) = printf "AddNode=%s" (show node)

