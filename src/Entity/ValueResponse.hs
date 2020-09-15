{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.ValueResponse where

import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype ValueResponseE =
    ValueResponseE (Either String Value)
        deriving (Generic, Serialise, Show)

instance MimeRender OctetStream ValueResponseE where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ValueResponseE where
    mimeUnrender _ = Right . deserialise

newtype ValueResponseM =
    ValueResponseM (Maybe Value)
        deriving (Generic, NFData, Serialise, Show)

instance MimeRender OctetStream ValueResponseM where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ValueResponseM where
    mimeUnrender _ = Right . deserialise
