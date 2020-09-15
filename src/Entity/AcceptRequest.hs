{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.AcceptRequest where

import Entity.Key
import Entity.ProposalNumber
import Entity.Value
import Node

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data AcceptRequest =
    AcceptRequest !(Set Node) !Key !ProposalNumber !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream AcceptRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream AcceptRequest where
    mimeUnrender _ = Right . deserialise
