{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Sync where

import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype SyncRequest =
    SyncRequest Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream SyncRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SyncRequest where
    mimeUnrender _ = Right . deserialise

data SyncResponse =
    SyncResponse !SequenceNum !SequenceNum
        deriving (Generic, Serialise)

instance MimeRender OctetStream SyncResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SyncResponse where
    mimeUnrender _ = Right . deserialise
