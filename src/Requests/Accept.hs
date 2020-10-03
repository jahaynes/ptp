{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Accept where

import Entity.Node           (Node)
import Entity.ProposalNumber (ProposalNumber)
import Entity.SequenceNum    (SequenceNum)
import Entity.Topic          (Topic)
import Entity.Value          (Value)

import Codec.Serialise (Serialise, serialise, deserialise)
import Data.Set        (Set)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data AcceptRequest =
    AcceptRequest !(Set Node) !Topic !SequenceNum !ProposalNumber !Value
        deriving (Generic, Serialise)

instance MimeRender OctetStream AcceptRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream AcceptRequest where
    mimeUnrender _ = Right . deserialise

newtype AcceptResponse =
    AcceptResponse (Either String Value)
        deriving (Generic, Serialise)

instance MimeRender OctetStream AcceptResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream AcceptResponse where
    mimeUnrender _ = Right . deserialise
