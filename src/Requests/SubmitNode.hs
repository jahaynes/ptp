{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.SubmitNode where

import Entity.Node  (Node)
import Entity.Topic (Topic)
import Entity.Value (Uniq (..), Val (..))

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data SubmitNodeRequest =
    SubmitNodeRequest !Topic !Uniq !Val
        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitNodeRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitNodeRequest where
    mimeUnrender _ = Right . deserialise

data SubmitNodeResponse = SubmitDone
                        | SubmitRetry
                        | SubmitElsewhere !Node
                            deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitNodeResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitNodeResponse where
    mimeUnrender _ = Right . deserialise
