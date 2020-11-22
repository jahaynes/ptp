{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.Decree where

import Entity.Node (Node)

import Codec.Serialise (Serialise)
import GHC.Generics    (Generic)

data Decree = ValueDecree !String
            | ElectLeader !Node
                deriving (Read, Show, Generic, Serialise)
