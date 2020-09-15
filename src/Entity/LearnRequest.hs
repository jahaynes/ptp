{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.LearnRequest where

import Entity.Key
import Entity.Id
import Entity.Value
import Node

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data LearnRequest =
    LearnRequest !(Set Node) !Key !Id !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream LearnRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream LearnRequest where
    mimeUnrender _ = Right . deserialise
