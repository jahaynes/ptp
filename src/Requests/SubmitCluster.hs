{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.SubmitCluster where

import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)
import Entity.Value       (Val (..))

import Codec.Serialise (Serialise, serialise, deserialise)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data SubmitClusterRequest =
    SubmitClusterRequest !Topic !Val
        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitClusterRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitClusterRequest where
    mimeUnrender _ = Right . deserialise

-- too much Error?
newtype SubmitClusterResponse =
    SubmitClusterResponse (Either String SequenceNum)
        deriving (Generic, Serialise)

instance MimeRender OctetStream SubmitClusterResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitClusterResponse where
    mimeUnrender _ = Right . deserialise
