{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.ForgetLeader where

import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

newtype ForgetLeaderRequest =
    ForgetLeaderRequest Topic
        deriving (Generic, Serialise)

instance MimeRender OctetStream ForgetLeaderRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ForgetLeaderRequest where
    mimeUnrender _ = Right . deserialise

data ForgetLeaderResponse =
    ForgetLeaderResponse
        deriving (Generic, Serialise)

instance MimeRender OctetStream ForgetLeaderResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ForgetLeaderResponse where
    mimeUnrender _ = Right . deserialise
