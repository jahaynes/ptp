{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Requests.ReadJournal where

import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)
import Entity.Value       (Value)

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data ReadJournalRequest =
    ReadJournalRequest !Topic ![SequenceNum]
        deriving (Generic, Serialise)

instance MimeRender OctetStream ReadJournalRequest where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ReadJournalRequest where
    mimeUnrender _ = Right . deserialise

newtype ReadJournalResponse =
    ReadJournalResponse [(SequenceNum, Value)]
        deriving (Generic, Serialise, NFData)

instance MimeRender OctetStream ReadJournalResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ReadJournalResponse where
    mimeUnrender _ = Right . deserialise