{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Propose where

import Entity.Node        (Node)
import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)
import Entity.Value       (Value)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data ProposeRequest =
    ProposeRequest !(Set Node) !Topic !SequenceNum !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream ProposeRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ProposeRequest where
    mimeUnrender _ = Right . deserialise

data ProposeResponse = NoHighestNackRoundNo
                     | Accepted !Value
                     | NotAccepted String
                        deriving (Generic, Serialise, Show)

instance MimeRender OctetStream ProposeResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ProposeResponse where
    mimeUnrender _ = Right . deserialise