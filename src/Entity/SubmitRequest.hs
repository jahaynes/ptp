{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.SubmitRequest where

import Entity.Topic
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data SubmitRequest =
    SubmitRequest !Topic !Val
        deriving (Generic, Serialise, Show)

instance MimeRender OctetStream SubmitRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitRequest where
    mimeUnrender _ = Right . deserialise
