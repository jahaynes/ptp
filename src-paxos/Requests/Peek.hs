{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Peek where

import Entity.SequenceNum    (SequenceNum)
import Entity.Topic          (Topic)
import Entity.Value          (Value)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Map        (Map)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data PeekRequest =
    PeekRequest !Topic ![SequenceNum]
        deriving (Generic, Serialise)

instance MimeRender OctetStream PeekRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PeekRequest where
    mimeUnrender _ = Right . deserialise

data PeekResponse = PeekResponse !(Map SequenceNum Value)
                  | PeekResponseError !String
                      deriving (Generic, Serialise)

instance MimeRender OctetStream PeekResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PeekResponse where
    mimeUnrender _ = Right . deserialise
