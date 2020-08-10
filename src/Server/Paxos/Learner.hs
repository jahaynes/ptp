{-# LANGUAGE LambdaCase,
             ScopedTypeVariables #-}

module Server.Paxos.Learner ( Learner (..)
                            , create
                            ) where

import Entity.EmptyResponse
import Entity.Id
import Entity.Key
import Entity.LearnRequest
import Entity.ValueResponse
import Quorum                    (threshold, majority)
import Server.Files
import Server.Keylocks
import Server.Paxos.LearnerState

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Map.Strict as M

data Learner m = Learner { learn       :: !(LearnRequest -> m ValueResponseM)
                         , check       :: !(Key -> m ValueResponseM)
                         , dbgPurgeKey :: !(Key -> m EmptyResponse)
                         }

create :: MonadIO m => Id -> Int -> IO (Learner m)
create myId numAcceptors = do

    keyLocks <- newLocks

    let getLearnerState :: Locked Key -> IO LearnerState
            = \key -> readState myId key "ls" >>= \case
                            Just f  -> pure f
                            Nothing -> pure $ LearnerState M.empty Nothing

    let putLearnerState :: Locked Key -> LearnerState -> IO ()
            = \key learnerState -> writeState myId key "ls" learnerState

    let purge key =
            withLockedKey keyLocks key $ \lockedKey -> do
                deleteState myId lockedKey "ls"
                pure EmptyResponse

    pure $ Learner { learn       = liftIO . learnService keyLocks getLearnerState putLearnerState numAcceptors
                   , check       = liftIO . checkImpl keyLocks getLearnerState
                   , dbgPurgeKey = liftIO . purge
                   }

learnService :: Locks Key
             -> (Locked Key -> IO LearnerState)
             -> (Locked Key -> LearnerState -> IO ())
             -> Int
             -> LearnRequest
             -> IO ValueResponseM
learnService keyLocks
             getLearnerState
             putLearnerState
             numAcceptors
             (LearnRequest key acceptorId value) =

    withLockedKey keyLocks key $ \lockedKey -> do 

        LearnerState as mc <- getLearnerState lockedKey

        let as' = M.insert acceptorId value as
        putLearnerState lockedKey $! LearnerState as' mc

        case mc of
            Just c  -> pure . ValueResponseM $ Just c
            Nothing -> do
                let values = map snd . M.toList $ as'
                case majority values (threshold numAcceptors) of
                    Nothing  -> pure $ ValueResponseM Nothing
                    Just maj -> do
                        putLearnerState lockedKey $! LearnerState as' (Just maj)
                        pure . ValueResponseM $ Just maj

checkImpl :: Locks Key -> (Locked Key -> IO LearnerState) -> Key -> IO ValueResponseM
checkImpl keyLocks getLearnerState key =
    withLockedKey keyLocks key $ \lockedKey ->
        ValueResponseM . lrn_consensus <$> getLearnerState lockedKey
