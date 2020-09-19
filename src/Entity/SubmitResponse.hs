{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.SubmitResponse where

import Entity.SequenceNum
import Entity.Topic
import Entity.Value
import Node

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data Reason = NotDefined !Topic
            | NotAcceptedR
            | SubmitElsewhere !Node
    deriving (Generic, Serialise, NFData, Show)

newtype SubmitResponse =
    SubmitResponse (Either Reason (SequenceNum, Val))
        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream SubmitResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitResponse where
    mimeUnrender _ = Right . deserialise
