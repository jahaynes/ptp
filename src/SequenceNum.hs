{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}
module SequenceNum where

import Data.Aeson    (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Word     (Word64)
import GHC.Generics  (Generic)

newtype SequenceNum =
    SequenceNum Word64
        deriving (Eq, Generic, Hashable, FromJSON, ToJSON)
