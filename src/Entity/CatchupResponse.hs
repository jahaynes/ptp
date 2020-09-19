{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.CatchupResponse where

import Entity.SequenceNum
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype CatchupResponse =
    CatchupResponse [(SequenceNum, Val)]
        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream CatchupResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CatchupResponse where
    mimeUnrender _ = Right . deserialise
