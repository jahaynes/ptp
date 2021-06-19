{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.Decree where

import Entity.Node (Node)

import Codec.Serialise     (Serialise)
import GHC.Generics        (Generic)
import RIO.ByteString.Lazy (ByteString)

data Decree = ValueDecree !ByteString
            | ElectLeader !Node
                deriving (Show, Generic, Serialise)
