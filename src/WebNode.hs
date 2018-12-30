{-# LANGUAGE ScopedTypeVariables #-}

module WebNode where

import NodeApi                  (NodeApi)
import Proposal                 (NodeId(NodeId), doProposal)
import ProposalNumber           (ProposalNumber, newProposalNumber)
import Quorum                   (threshold, majority)
import Types
import WebNodeClient            (NodeClient, getLearnClient)

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Either              (rights)
import qualified Data.Map as M
import Data.Map                 (Map)
import Data.Maybe               (fromJust, catMaybes)
import Data.Proxy               (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Servant                  (serve)
import Servant.API

newtype ProposerState = ProposerState { getPropNum :: ProposalNumber }

data AcceptorState = AcceptorState { acc_notLessThan :: Maybe ProposalNumber
                                   , acc_proposal    :: Maybe Proposal }

data LearnerState = LearnerState { lrn_acceptedProposals :: Map String Value
                                 , lrn_consensus         :: Maybe Value }

runNode :: Int -> String -> [NodeClient] -> Int -> IO ()
runNode numAcceptors myId clients port = do

    proposerState <- ProposerState <$> newProposalNumber

    acceptorState <- newTVarIO $ AcceptorState { acc_notLessThan = Nothing
                                               , acc_proposal    = Nothing }

    learnerState <- newTVarIO $ LearnerState { lrn_acceptedProposals = M.empty
                                             , lrn_consensus         = Nothing}

    run port $ serve (Proxy :: Proxy NodeApi) $ proposeImpl proposerState
                                           :<|> prepareImpl acceptorState
                                           :<|> acceptImpl acceptorState
                                           :<|> learnImpl learnerState

    where
    proposeImpl :: MonadIO m => ProposerState -> ProposeRequest -> m (Either String Value)
    proposeImpl proposerState (ProposeRequest value) = liftIO $ do

        let clientMap = M.fromList $ zip (map NodeId [1..]) clients
        doProposal (getPropNum proposerState) value clientMap

    prepareImpl :: MonadIO m => TVar AcceptorState -> PrepareRequest -> m (Either Nack Promise)
    prepareImpl acceptorState (PrepareRequest n) = liftIO . atomically $ do
        state <- readTVar acceptorState
        if Just n > acc_notLessThan state
            then do
                writeTVar acceptorState $ state {acc_notLessThan = Just n}
                pure . Right $ Promise n (acc_proposal state)
            else pure . Left . Nack . fromJust . acc_notLessThan $ state

    acceptImpl :: MonadIO m => TVar AcceptorState -> AcceptRequest -> m (Either String Value)
    acceptImpl acceptorState (AcceptRequest n v) = liftIO $ do

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
                case catMaybes learnerResponses of
                    []     -> pure $ Left "No consensus (yet)"
                    (c:cs) ->
                        -- Sanity check
                        if all (==c) cs
                            then pure $ Right c
                            else pure $ Left "Fatal: Inconsistent consensus"

                else pure $ Left "Not accepted"

    learnImpl :: MonadIO m => TVar LearnerState -> LearnRequest -> m (Maybe Value)
    learnImpl learnerState (LearnRequest acceptorId value) = liftIO $ do

        (a,newLearnt) <- atomically $ do

            LearnerState as mc <- readTVar learnerState

            let as' = M.insert acceptorId value as
            writeTVar learnerState (LearnerState as' mc)

            case mc of
                Just c  -> pure (Just c, Nothing)
                Nothing -> do
                    let values = map snd . M.toList $ as'
                    case majority values (threshold numAcceptors) of
                        Nothing  -> pure (Nothing, Nothing)
                        Just maj -> do
                            writeTVar learnerState (LearnerState as' (Just maj))
                            pure (Just maj, Just maj)

        case newLearnt of
             Just l -> print ("learned", l)
             Nothing -> return ()

        return a
