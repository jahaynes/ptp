{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}

module Entity.Host ( Host
                   , getHostString
                   , host
                   , localHost
                   ) where

import Codec.Serialise (Serialise)
import Data.Aeson      (FromJSON, ToJSON)
import RIO
import RIO.Text        (pack, unpack)

newtype Host =
    Host Text
        deriving (Eq, Show, Ord, Generic, Serialise, Hashable, FromJSON, ToJSON)

host :: String -> Host
host "127.0.0.1" = error "Don't use localhost"
host "localhost" = error "Don't use localhost"
host "::1"       = error "Don't use localhost"
host x           = Host $ pack x

localHost :: Host
localHost = Host "localhost"

getHostString :: Host -> String
getHostString (Host h) = unpack h