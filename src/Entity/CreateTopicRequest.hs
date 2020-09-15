{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.CreateTopicRequest where

import Entity.Topic
import Node

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CreateTopicRequest =
    CreateTopicRequest !(Set Node) !Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream CreateTopicRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicRequest where
    mimeUnrender _ = Right . deserialise