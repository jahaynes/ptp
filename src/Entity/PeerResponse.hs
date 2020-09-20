{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.PeerResponse where

import Entity.SequenceNum
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype PeerResponse =
    PeerResponse [(SequenceNum, Val)]
        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream PeerResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PeerResponse where
    mimeUnrender _ = Right . deserialise
