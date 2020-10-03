{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Submit where

import Entity.Node  (Node)
import Entity.Topic (Topic)
import Entity.Value (Uniq (..), Val (..))

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data SubmitRequest =
    SubmitRequest !Topic !Uniq !Val
        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitRequest where
    mimeUnrender _ = Right . deserialise

data SubmitResponse = SubmitDone
                    | SubmitRetry
                    | SubmitElsewhere !Node
                        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitResponse where
    mimeUnrender _ = Right . deserialise
