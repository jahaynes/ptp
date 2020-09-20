{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.PeerRequest where

import Entity.Topic
import Entity.SequenceNum

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data PeerRequest =
    PeerRequest !Topic ![SequenceNum]
        deriving (Generic, Serialise, Show)

instance MimeRender OctetStream PeerRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PeerRequest where
    mimeUnrender _ = Right . deserialise
