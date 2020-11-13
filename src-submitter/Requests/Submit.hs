{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Submit where

import Entity.Decree      (Decree)

import Entity.Node        (Node)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data SubmitRequest =
    SubmitRequest !Topic !Decree
        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitRequest where
    mimeUnrender _ = Right . deserialise

data SubmitResponse = Submitted
                    | RetryRequested
                    | OtherLeader !Node
                    | SubmitError !String
                        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitResponse where
    mimeUnrender _ = Right . deserialise
