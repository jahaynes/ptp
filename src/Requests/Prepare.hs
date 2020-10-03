{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.Prepare where

import Entity.Proposal       (Proposal)
import Entity.ProposalNumber (ProposalNumber)
import Entity.SequenceNum    (SequenceNum)
import Entity.Topic          (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data PrepareRequest =
    PrepareRequest !Topic !SequenceNum !ProposalNumber
        deriving (Generic, Serialise)

instance MimeRender OctetStream PrepareRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PrepareRequest where
    mimeUnrender _ = Right . deserialise

newtype PrepareResponse =
    PrepareResponse (Either Nack Promise)
        deriving (Generic, Serialise)

instance MimeRender OctetStream PrepareResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream PrepareResponse where
    mimeUnrender _ = Right . deserialise

data Promise = Promise { prom_notLessThan     :: !ProposalNumber
                       , prom_highestProposal :: !(Maybe Proposal)
                       } deriving (Generic, Serialise, NFData)

newtype Nack =
    Nack ProposalNumber
        deriving (Generic, Serialise, NFData)
