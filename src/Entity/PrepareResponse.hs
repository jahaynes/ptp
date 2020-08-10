{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.PrepareResponse where

import Entity.Nack
import Entity.Promise

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype PrepareResponse =
    PrepareResponse (Either Nack Promise)
        deriving (Generic, Serialise, NFData)

instance MimeRender OctetStream PrepareResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PrepareResponse where
    mimeUnrender _ = Right . deserialise