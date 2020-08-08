{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Topic where

import Data.Aeson    (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics  (Generic)

newtype Topic =
    Topic String
        deriving (Eq, Generic, Hashable, FromJSON, ToJSON)
