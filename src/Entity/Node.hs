{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Node where

import Entity.Id
import Entity.Port

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Text.Printf     (printf)

data Node =
    Node !Id !Port
        deriving (Eq, Ord, Generic, Serialise, NFData)

instance Show Node where
    show (Node (Id i) (Port p)) = printf "{%s}@%s" i (show p)