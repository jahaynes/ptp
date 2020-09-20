{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.CatchupRequest where

import Entity.Topic
import Entity.SequenceNum

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CatchupRequest =
    CatchupRequest !Topic ![SequenceNum]
        deriving (Generic, Serialise, Show)

instance MimeRender OctetStream CatchupRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CatchupRequest where
    mimeUnrender _ = Right . deserialise
