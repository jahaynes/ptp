{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Catchup where

import Entity.Node  (Node)
import Entity.Topic (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CatchupRequest =
    CatchupRequest !Node !Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream CatchupRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CatchupRequest where
    mimeUnrender _ = Right . deserialise

data CatchupResponse =
    CatchupResponse
        deriving (Generic, Serialise)

instance MimeRender OctetStream CatchupResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CatchupResponse where
    mimeUnrender _ = Right . deserialise
