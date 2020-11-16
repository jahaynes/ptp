{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Host ( Host
                   , getHostSafe
                   , host
                   , localHost
                   ) where

import Codec.Serialise (Serialise)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)

newtype Host =
    Host String
        deriving (Eq, Read, Show, Ord, Generic, Serialise, Hashable)

host :: String -> Host
host "127.0.0.1" = error "Don't use localhost"
host "localhost" = error "Don't use localhost"
host "::1"       = error "Don't use localhost"
host x           = Host x

localHost :: Host
localHost = Host "localhost"

getHostSafe :: Host -> String
getHostSafe (Host h) = h