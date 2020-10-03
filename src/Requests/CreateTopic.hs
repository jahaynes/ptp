{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.CreateTopic where

import Entity.Node  (Node)
import Entity.Topic (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CreateTopicRequest =
    CreateTopicRequest !Topic !(Set Node)
        deriving (Generic, Serialise)

instance MimeRender OctetStream CreateTopicRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicRequest where
    mimeUnrender _ = Right . deserialise

newtype CreateTopicResponse =
    CreateTopicResponse (Either String ())
        deriving (Generic, Serialise)

instance MimeRender OctetStream CreateTopicResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicResponse where
    mimeUnrender _ = Right . deserialise
