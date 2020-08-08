{-# LANGUAGE ScopedTypeVariables,
             LambdaCase #-}

module Server.Paxos.Acceptor ( Acceptor (..)
                             , create
                             ) where

import Client.WebNodeClient       (NodeClient, getLearnClient)
import Server.Files
import Server.Keylocks
import Server.Paxos.AcceptorState
import Types

import Control.Concurrent.Async (async, wait)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Either              (rights)
import Data.Maybe               (catMaybes, fromJust)

data Acceptor m =
    Acceptor { prepare     :: !(PrepareRequest -> m (Either Nack Promise))
             , accept      :: !(AcceptRequest -> m (Either String Value))
             , dbgPurgeKey :: !(Key -> m ())
             }

create :: MonadIO m => Id -> [NodeClient e] -> IO (Acceptor m)
create myId clients = do

    keyLocks <- newLocks

    let getAcceptorState :: Locked Key -> IO AcceptorState
            = \key -> readState myId key "as" >>= \case
                          Just f  -> pure f
                          Nothing -> pure $ AcceptorState Nothing Nothing

    let putAcceptorState :: Locked Key -> AcceptorState -> IO ()
            = \key acceptorState -> writeState myId key "as" acceptorState

    let purge key =
            withLockedKey keyLocks key $ \lockedKey ->
                deleteState myId lockedKey "as"

    pure $ Acceptor { prepare     = liftIO . prepareService keyLocks getAcceptorState putAcceptorState
                    , accept      = liftIO . acceptService myId clients keyLocks getAcceptorState putAcceptorState
                    , dbgPurgeKey = liftIO . purge
                    }

prepareService :: Locks Key
               -> (Locked Key -> IO AcceptorState)
               -> (Locked Key -> AcceptorState -> IO ())
               -> PrepareRequest
               -> IO (Either Nack Promise)
prepareService keyLocks getAcceptorState putAcceptorState (PrepareRequest key n) = do   
    withLockedKey keyLocks key $ \lockedKey -> do
        state <- getAcceptorState lockedKey
        if Just n > acc_notLessThan state
            then do
                putAcceptorState lockedKey $! state {acc_notLessThan = Just n}
                pure . Right $ Promise n (acc_proposal state)
            else pure . Left . Nack . fromJust . acc_notLessThan $ state

acceptService :: Id
              -> [NodeClient e]
              -> Locks Key
              -> (Locked Key -> IO AcceptorState)
              -> (Locked Key -> AcceptorState -> IO ())
              -> AcceptRequest
              -> IO (Either String Value)
acceptService myId clients keyLocks getAcceptorState putAcceptorState (AcceptRequest key n v) = do

    accepted <- withLockedKey keyLocks key $ \lockedKey -> do
        state <- getAcceptorState lockedKey
        if Just n >= acc_notLessThan state
            then do
                putAcceptorState lockedKey $ state { acc_notLessThan = Just n
                                                     , acc_proposal    = Just (Proposal n v) }
                pure True
            else pure False

    if accepted

        then do

            -- Inform every (responsive) learner
            aLearnerResponses <- mapM ((\c -> async $ c (LearnRequest key myId v)) . getLearnClient) clients
            learnerResponses <- rights <$> mapM wait aLearnerResponses

            -- Consider every consensus claimed by a learner
            pure $ case catMaybes learnerResponses of

                        []     -> Left "No consensus (yet)"

                            -- Sanity check
                        (c:cs) | all (==c) cs -> Right c

                               | otherwise -> Left "Fatal: Inconsistent consensus"

        else pure $ Left "Not accepted"
