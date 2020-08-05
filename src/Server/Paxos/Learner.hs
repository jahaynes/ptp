module Server.Paxos.Learner ( Learner (..)
                            , create
                            ) where

import Quorum (threshold, majority)
import Types

import           Control.Concurrent.STM   (TVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import           Data.Map.Strict          (Map)

data LearnerState = LearnerState { _lrn_acceptedProposals :: !(Map Id Value)
                                 , lrn_consensus         :: !(Maybe Value) }

data Learner m = Learner { learn :: !(LearnRequest -> m (Maybe Value))
                         , check :: !(m (Maybe Value))
                         }

create :: MonadIO m => Int -> IO (Learner m)
create numAcceptors = do 
    state <- newTVarIO (LearnerState M.empty Nothing)
    pure $ Learner { learn = liftIO . learnService state numAcceptors
                   , check = liftIO $ checkImpl state
                   }

learnService :: TVar LearnerState
             -> Int
             -> LearnRequest
             -> IO (Maybe Value)
learnService learnerState numAcceptors (LearnRequest acceptorId value) =
    liftIO . atomically $ do

        LearnerState as mc <- readTVar learnerState

        let as' = M.insert acceptorId value as
        writeTVar learnerState $! LearnerState as' mc

        case mc of
            Just c  -> pure $ Just c
            Nothing -> do
                let values = map snd . M.toList $ as'
                case majority values (threshold numAcceptors) of
                    Nothing  -> pure Nothing
                    Just maj -> do
                        writeTVar learnerState (LearnerState as' (Just maj))
                        pure $ Just maj

checkImpl :: TVar LearnerState -> IO (Maybe Value)
checkImpl learnerState = atomically $ do
    lrn_consensus <$> readTVar learnerState