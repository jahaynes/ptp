module Server.Paxos.Acceptor ( Acceptor (..)
                             , create
                             ) where

import Client.WebNodeClient (NodeClient, getLearnClient)
import ProposalNumber       (ProposalNumber)
import Types

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM   (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Either              (rights)
import Data.Maybe               (catMaybes, fromJust)

data AcceptorState = AcceptorState { acc_notLessThan :: !(Maybe ProposalNumber)
                                   , acc_proposal    :: !(Maybe Proposal) }

data Acceptor m =
    Acceptor { prepare :: !(PrepareRequest -> m (Either Nack Promise))
             , accept  :: !(AcceptRequest -> m (Either String Value))
             }

create :: MonadIO m => Id -> [NodeClient e] -> IO (Acceptor m)
create myId clients = do
    state <- newTVarIO (AcceptorState Nothing Nothing)
    pure $ Acceptor { prepare = liftIO . prepareService state
                    , accept  = liftIO . acceptService myId clients state
                    }

prepareService :: TVar AcceptorState
               -> PrepareRequest
               -> IO (Either Nack Promise)
prepareService acceptorState (PrepareRequest n) = atomically $ do
    state <- readTVar acceptorState
    if Just n > acc_notLessThan state
        then do
            writeTVar acceptorState $ state {acc_notLessThan = Just n}
            pure . Right $ Promise n (acc_proposal state)
        else pure . Left . Nack . fromJust . acc_notLessThan $ state

acceptService :: Id
              -> [NodeClient e]
              -> TVar AcceptorState
              -> AcceptRequest
              -> IO (Either String Value)
acceptService myId clients acceptorState (AcceptRequest n v) = do

    accepted <- atomically $ do
        state <- readTVar acceptorState
        if Just n >= acc_notLessThan state
            then do
                writeTVar acceptorState $ state { acc_notLessThan = Just n
                                                , acc_proposal    = Just (Proposal n v) }
                pure True
            else pure False

    if accepted

        then do

            -- Inform every (responsive) learner
            aLearnerResponses <- mapM ((\c -> async $ c (LearnRequest myId v)) . getLearnClient) clients
            learnerResponses <- rights <$> mapM wait aLearnerResponses

            -- Consider every consensus claimed by a learner
            pure $ case catMaybes learnerResponses of

                        []     -> Left "No consensus (yet)"

                            -- Sanity check
                        (c:cs) | all (==c) cs -> Right c

                               | otherwise -> Left "Fatal: Inconsistent consensus"

        else pure $ Left "Not accepted"
