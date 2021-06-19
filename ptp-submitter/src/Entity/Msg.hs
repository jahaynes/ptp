{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Msg where

import Entity.ClusterHash (ClusterHash)
import Entity.Decree      (Decree)
import Entity.Uniq        (Uniq)
import Entity.Value       (Value (..))

import Codec.Serialise    (Serialise, serialise, deserialise)
import RIO                (Generic)

data Msg =
    Msg !Uniq !ClusterHash !Decree
        deriving (Generic, Serialise, Show)

toValue :: Msg -> Value
toValue = Value . serialise

fromValue :: Value -> Msg
fromValue (Value s) = deserialise s