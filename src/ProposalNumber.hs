{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module ProposalNumber where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON, ToJSON)
import Data.UUID              (toString)
import Data.UUID.V4           (nextRandom)
import GHC.Generics           (Generic)

data ProposalNumber = ProposalNumber
                    { getRoundNo :: !Int
                    , getUniq    :: !Uniq
                    } deriving (Generic, FromJSON, ToJSON, Eq, Ord, Show)

newtype Uniq = Uniq String
                   deriving (Generic, FromJSON, ToJSON, Eq, Ord, Show)

newProposalNumber :: MonadIO m => m ProposalNumber
newProposalNumber = ProposalNumber 1 <$> randomUniq

randomUniq :: MonadIO m => m Uniq
randomUniq = Uniq . toString <$> liftIO nextRandom
