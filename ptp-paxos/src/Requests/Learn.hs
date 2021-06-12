{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Learn where

import Entity.Node        (Node)
import Entity.Id          (Id)
import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)
import Entity.Value       (Value)

import Codec.Serialise   (Serialise, serialise, deserialise)
import RIO
import Servant.API       (OctetStream, MimeRender (..), MimeUnrender (..))

data LearnRequest =
    LearnRequest !(Set Node) !Topic !SequenceNum !Id !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream LearnRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream LearnRequest where
    mimeUnrender _ = Right . deserialise

data LearnResponse = LearnResponse !(Maybe Value)
                   | LearnResponseError !String
                       deriving (Generic, Serialise)

instance MimeRender OctetStream LearnResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream LearnResponse where
    mimeUnrender _ = Right . deserialise
