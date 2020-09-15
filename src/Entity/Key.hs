{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.Key where

import Entity.SequenceNum
import Entity.Topic

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data Key = Key !Topic
               !SequenceNum
                   deriving (Eq, Ord, Generic, Serialise, Show)

instance MimeRender OctetStream Key where
    mimeRender _ = serialise

instance MimeUnrender OctetStream Key where
    mimeUnrender _ = Right . deserialise
