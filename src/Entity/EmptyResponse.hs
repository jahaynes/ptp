{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.EmptyResponse where

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data EmptyResponse =
    EmptyResponse
        deriving (Eq, Ord, Generic, NFData, Serialise)

instance MimeRender OctetStream EmptyResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream EmptyResponse where
    mimeUnrender _ = Right . deserialise
