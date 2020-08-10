{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.PrepareRequest where

import Entity.Key
import Entity.ProposalNumber

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data PrepareRequest =
    PrepareRequest !Key !ProposalNumber
        deriving (Generic, Serialise)

instance MimeRender OctetStream PrepareRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PrepareRequest where
    mimeUnrender _ = Right . deserialise