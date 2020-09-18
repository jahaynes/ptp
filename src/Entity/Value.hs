{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Value where

import Entity.Id
import Node

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
--import Data.List       (intercalate)
--import Data.Set        (Set, toList)
import GHC.Generics    (Generic)
import Text.Printf     (printf)

data Value =
    Value !String !Val
        deriving (Eq, Generic, Serialise, NFData)

data Val = SimpleValue String
         | ControlValue !Command
             deriving (Eq, Generic, Serialise, NFData)

instance Show Value where
    show (Value u v) = printf "%s-%s" u (show v)

instance Show Val where
    show (SimpleValue s) = s
    show (ControlValue c) = show c


data Command = ElectLeader !Node
          --   | SetServers !(Set Node)
          --   | SetObservers !(Set Node)
                 deriving (Eq, Generic, Serialise, NFData)

instance Show Command where

    show (ElectLeader (Node (Id i) _)) = printf "Leader=%s" i
{-
    show (SetServers nodes) = printf "Servers=[%s]"
                            . intercalate ", "
                            . map (\(Node (Id ident) _) -> ident)
                            . toList
                            $ nodes
-}
