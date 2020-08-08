{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module ProposalNumber where

import Codec.Serialise
import Control.DeepSeq        (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON, ToJSON)
import Data.UUID              (toString)
import Data.UUID.V4           (nextRandom)
import Data.Word              (Word64)
import GHC.Generics           (Generic)

data ProposalNumber = ProposalNumber
                    { getRoundNo :: !Word64
                    , getUniq    :: !Uniq
                    } deriving (Generic, Serialise, FromJSON, ToJSON, Eq, Ord, NFData, Show)

newtype Uniq =
    Uniq String
        deriving (Generic, Serialise, FromJSON, ToJSON, Eq, Ord, NFData, Show)

newProposalNumber :: MonadIO m => m ProposalNumber
newProposalNumber = ProposalNumber 1 <$> randomUniq

randomUniq :: MonadIO m => m Uniq
randomUniq = Uniq . toString <$> liftIO nextRandom
