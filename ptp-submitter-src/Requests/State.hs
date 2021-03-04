{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.State where

import Entity.Host        (Host)
import Entity.Id          (Id)
import Entity.Node        (Node)
import Entity.SequenceNum (SequenceNum)
import Entity.Port        (Port)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Aeson      (ToJSON, FromJSON)
import Data.Set        (Set)
import GHC.Generics    (Generic)
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

-- TODO orphan instances
instance FromJSON Topic
instance FromJSON SequenceNum
instance FromJSON Node
instance FromJSON Host
instance FromJSON Id
instance FromJSON Port

instance ToJSON Topic
instance ToJSON SequenceNum
instance ToJSON Node
instance ToJSON Host
instance ToJSON Id
instance ToJSON Port