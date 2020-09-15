{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Node where

import Entity.Id
import Port

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

data Node =
    Node !Id !Port
        deriving (Eq, Ord, Generic, Serialise, NFData, Show)
