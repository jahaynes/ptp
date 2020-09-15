{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.SubmitResponse where

import Entity.SequenceNum
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype SubmitResponse =
    SubmitResponse (Either String (SequenceNum, Value))
        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream SubmitResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitResponse where
    mimeUnrender _ = Right . deserialise
