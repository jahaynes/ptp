{-# LANGUAGE LambdaCase,
             ScopedTypeVariables #-}

module Server.Paxos.Learner ( Learner (..)
                            , create
                            ) where

import Quorum                    (threshold, majority)
import Server.Files
import Server.Keylocks
import Server.Paxos.LearnerState
import Types

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Map.Strict as M

data Learner m = Learner { learn       :: !(LearnRequest -> m (Maybe Value))
                         , check       :: !(Key -> m (Maybe Value))
                         , dbgPurgeKey :: !(Key -> m ())
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
            withLockedKey keyLocks key $ \lockedKey ->
                deleteState myId lockedKey "ls"

    pure $ Learner { learn       = liftIO . learnService keyLocks getLearnerState putLearnerState numAcceptors
                   , check       = liftIO . checkImpl keyLocks getLearnerState
                   , dbgPurgeKey = liftIO . purge
                   }

learnService :: Locks Key
             -> (Locked Key -> IO LearnerState)
             -> (Locked Key -> LearnerState -> IO ())
             -> Int
             -> LearnRequest
             -> IO (Maybe Value)
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
            Just c  -> pure $ Just c
            Nothing -> do
                let values = map snd . M.toList $ as'
                case majority values (threshold numAcceptors) of
                    Nothing  -> pure Nothing
                    Just maj -> do                        
                        putLearnerState lockedKey $! LearnerState as' (Just maj)
                        pure $ Just maj

checkImpl :: Locks Key -> (Locked Key -> IO LearnerState) -> Key -> IO (Maybe Value)
checkImpl keyLocks getLearnerState key =
    withLockedKey keyLocks key $ \lockedKey ->
        lrn_consensus <$> getLearnerState lockedKey
