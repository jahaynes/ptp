{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.AcceptRequest where

import Entity.Key
import Entity.ProposalNumber
import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data AcceptRequest =
    AcceptRequest !Key !ProposalNumber !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream AcceptRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream AcceptRequest where
    mimeUnrender _ = Right . deserialise