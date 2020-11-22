module Entity.Msg where

import Entity.ClusterHash (ClusterHash)
import Entity.Decree      (Decree)

import Entity.Uniq  (Uniq)
import Entity.Value (Value (..))

data Msg =
    Msg !Uniq !ClusterHash !Decree
        deriving (Read, Show)

toValue :: Msg -> Value
toValue = Value . show

fromValue :: Value -> Msg
fromValue (Value s) = read s