{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.CatchupResponse where

import Entity.SequenceNum (SequenceNum)
import Entity.Topic       (Topic)

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CatchupResponse = CaughtUp
                     | StillMissing ![SequenceNum]
                     | ErrNoSuchTopic !Topic
                        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream CatchupResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CatchupResponse where
    mimeUnrender _ = Right . deserialise
