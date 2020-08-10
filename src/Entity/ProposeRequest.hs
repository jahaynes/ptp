{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.ProposeRequest where

import Entity.Key
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data ProposeRequest =
    ProposeRequest !Key !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream ProposeRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ProposeRequest where
    mimeUnrender _ = Right . deserialise