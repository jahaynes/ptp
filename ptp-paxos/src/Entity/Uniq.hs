{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Uniq where

import Codec.Serialise (Serialise)
import Data.UUID.V4    (nextRandom)
import RIO

newtype Uniq =
    Uniq String
        deriving (Eq, Ord, Read, Show, Generic, Serialise, NFData)

uniq :: IO Uniq
uniq = Uniq . show <$> nextRandom