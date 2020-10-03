{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Ping where

import Entity.Node (Node)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data Ping = Ping
          | PingOther !Node
              deriving (Generic, Serialise)

instance MimeRender OctetStream Ping where
    mimeRender _ = serialise

instance MimeUnrender OctetStream Ping where
    mimeUnrender _ = Right . deserialise

data Pong = Pong
    deriving (Generic, Serialise)

instance MimeRender OctetStream Pong where
    mimeRender _ = serialise

instance MimeUnrender OctetStream Pong where
    mimeUnrender _ = Right . deserialise
