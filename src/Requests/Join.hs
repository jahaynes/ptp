{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Join where

import Entity.Node        (Node)
import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data JoinRequest =
    JoinPrepare { joinTopic :: !Topic
                , joiner    :: !Node
                } deriving (Generic, Serialise)

instance MimeRender OctetStream JoinRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream JoinRequest where
    mimeUnrender _ = Right . deserialise

data JoinResponse = JoinNoSuchTopic
                  | JoinClusterNotFullyResponsive
                  | JoinerNotResponsive
                  | JoinedAt !SequenceNum
        deriving (Generic, Serialise)

instance MimeRender OctetStream JoinResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream JoinResponse where
    mimeUnrender _ = Right . deserialise
