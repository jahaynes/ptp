{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.ProposeResponse where

import Entity.Value

import Codec.Serialise (Serialise, serialise, deserialise)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Servant.API     (OctetStream, MimeRender (..), MimeUnrender (..))

data ProposeResponse = NoHighestNackRoundNo
                     | Accepted !Value
                     | NotAccepted String
                        deriving (Generic, Serialise, NFData, Show)

instance MimeRender OctetStream ProposeResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream ProposeResponse where
    mimeUnrender _ = Right . deserialise
