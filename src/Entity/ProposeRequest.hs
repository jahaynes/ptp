{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.ProposeRequest where

import Entity.Key
import Entity.Value
import Node

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data ProposeRequest =
    ProposeRequest !(Set Node) !Key !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream ProposeRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ProposeRequest where
    mimeUnrender _ = Right . deserialise