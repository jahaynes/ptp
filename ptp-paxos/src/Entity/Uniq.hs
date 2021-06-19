{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving #-}

module Entity.Uniq where

import Codec.Serialise (Serialise)
import Data.UUID       (toText)
import Data.UUID.V4    (nextRandom)
import RIO

newtype Uniq =
    Uniq Text
        deriving (Eq, Ord, Show, Generic, Serialise, NFData)

uniq :: IO Uniq
uniq = Uniq . toText <$> nextRandom