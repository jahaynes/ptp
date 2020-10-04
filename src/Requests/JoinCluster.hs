{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.JoinCluster where

import Entity.Node        (Node)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data JoinClusterRequest =
    JoinClusterRequest !Node !Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream JoinClusterRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream JoinClusterRequest where
    mimeUnrender _ = Right . deserialise

data JoinClusterResponse =
    JoinClusterResponse
        deriving (Generic, Serialise)

instance MimeRender OctetStream JoinClusterResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream JoinClusterResponse where
    mimeUnrender _ = Right . deserialise
