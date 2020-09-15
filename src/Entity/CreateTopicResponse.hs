{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.CreateTopicResponse where

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data CreateTopicResponse =
    CreateTopicResponse
        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream CreateTopicResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream CreateTopicResponse where
    mimeUnrender _ = Right . deserialise
