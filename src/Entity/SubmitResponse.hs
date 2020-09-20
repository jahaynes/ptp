{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             MultiParamTypeClasses #-}

module Entity.SubmitResponse where

import Entity.SequenceNum
import Entity.Topic
import Entity.Value
import Node

import Codec.Serialise      (Serialise, serialise, deserialise)
import Control.DeepSeq      (NFData)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics         (Generic)
import Servant              (ServerError (..))
import Servant.API          (OctetStream, MimeRender (..), MimeUnrender (..))

data Reason = NotDefined !Topic
            | SubmitElsewhere !Node
            | ServerErrorReason !ServerErrorResponse
    deriving (Generic, Serialise, NFData, Show)

newtype SubmitResponse =
    SubmitResponse (Either Reason (SequenceNum, Val))
        deriving (Generic, Serialise, NFData, Show)

data ServerErrorResponse =
    ServerErrorResponse { code   :: !Int
                        , phrase :: !String
                        , body   :: !ByteString
                        } deriving (Generic, Serialise, NFData, Show)

fromServerError :: ServerError -> Reason
fromServerError se =
    ServerErrorReason $
        ServerErrorResponse { code   = errHTTPCode se
                            , phrase = errReasonPhrase se
                            , body   = errBody se
                            }

instance MimeRender OctetStream SubmitResponse where
    mimeRender _ = serialise

instance MimeUnrender OctetStream SubmitResponse where
    mimeUnrender _ = Right . deserialise
