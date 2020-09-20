{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Value where

import Node

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Text.Printf     (printf)

data Value =
    Value Node !String !Val
        deriving (Eq, Generic, Serialise, NFData)

newtype Val =
    SimpleValue String
        deriving (Eq, Ord, Generic, Serialise, NFData)

instance Show Value where
    show (Value n u v) = printf "%s-%s-%s" (show n) u (show v)

instance Show Val where
    show (SimpleValue s) = s

{-
data Command = ElectLeader !Node
          --   | SetServers !(Set Node)
          --   | SetObservers !(Set Node)
                 deriving (Eq, Generic, Serialise, NFData)

instance Show Command where

    show (ElectLeader (Node (Id i) _)) = printf "Leader=%s" i

    show (SetServers nodes) = printf "Servers=[%s]"
                            . intercalate ", "
                            . map (\(Node (Id ident) _) -> ident)
                            . toList
                            $ nodes
-}
