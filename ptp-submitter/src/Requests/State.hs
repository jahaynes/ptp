{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.State where

import Entity.Node        (Node)
import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Aeson      (ToJSON, FromJSON)
import RIO
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data StateResponse =
    StateResponse [TopicState]
        deriving (Generic, Serialise, FromJSON, ToJSON)

instance MimeRender OctetStream StateResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream StateResponse where
    mimeUnrender _ = Right . deserialise

data TopicState =
    TopicState { topic        :: !Topic
               , highestKnown :: !SequenceNum
               , cluster      :: !(Set Node)
               , leader       :: !(Maybe Node)
               } deriving (Generic, Serialise, FromJSON, ToJSON)
