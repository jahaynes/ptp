{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.Uniq where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.UUID.V4    (nextRandom)
import GHC.Generics    (Generic)

newtype Uniq =
    Uniq String
        deriving (Eq, Ord, Read, Show, Generic, Serialise, NFData)

uniq :: IO Uniq
uniq = Uniq . show <$> nextRandom