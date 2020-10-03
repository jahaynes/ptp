{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.SequenceNum where

import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype SequenceNumRequest =
    SequenceNumRequest Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream SequenceNumRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SequenceNumRequest where
    mimeUnrender _ = Right . deserialise

newtype SequenceNumResponse =
    SequenceNumResponse (Maybe SequenceNum)
        deriving (Generic, Serialise)

instance MimeRender OctetStream SequenceNumResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SequenceNumResponse where
    mimeUnrender _ = Right . deserialise
