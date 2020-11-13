{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.CreateTopic where

import Entity.Node  (Node)
import Entity.Topic (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CreateTopicRequest =
    CreateTopicRequest ![Node] !Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream CreateTopicRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicRequest where
    mimeUnrender _ = Right . deserialise

data CreateTopicResponse =
    CreateTopicResponse
        deriving (Generic, Serialise)

instance MimeRender OctetStream CreateTopicResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicResponse where
    mimeUnrender _ = Right . deserialise
