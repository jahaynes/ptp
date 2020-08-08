{-# LANGUAGE LambdaCase #-}

module Server.Paxos.Proposer ( Proposer (..)
                             , create
                             ) where

import Client.WebNodeClient (NodeClient (..))
import ProposalNumber       (ProposalNumber (..), newProposalNumber)
import Quorum
import Types

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Either              (rights)
import           Data.List                (maximumBy)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe               (fromJust, fromMaybe, mapMaybe)
import           Data.Ord                 (comparing)
import           Data.Word                (Word64)
import           Safe                     (headMay)

newtype ProposerState = ProposerState { getPropNum :: ProposalNumber }

newtype Proposer m = Proposer { propose :: ProposeRequest -> m (Either String Value) }

newtype NodeId = NodeId Int deriving (Eq, Ord)

create :: (MonadIO m, Show e) => [NodeClient e] -> IO (Proposer m)
create clients = do
    state <- ProposerState <$> newProposalNumber
    pure $ Proposer { propose = liftIO . proposeService clients state }

proposeService :: Show e => [NodeClient e] 
                         -> ProposerState
                         -> ProposeRequest
                         -> IO (Either String Value)
proposeService clients proposerState (ProposeRequest key value) = liftIO $ do
    let clientMap = M.fromList $ zip (map NodeId [1..]) clients
    doProposal key (getPropNum proposerState) value clientMap

doProposal :: Show e => Key
                     -> ProposalNumber
                     -> Value
                     -> Map NodeId (NodeClient e)
                     -> IO (Either String Value)
doProposal key pn defaultVal clients =

    doPrepares key pn clients >>= \case

        Left errors -> case highestNackRoundNo errors of
                           Nothing  -> pure . Left $ "No highestNackRoundNo"
                           Just hrn -> doProposal key pn {getRoundNo = succ hrn} defaultVal clients

        Right quorum -> do

            let valueToUse = fromMaybe defaultVal
                           . highestNumberedValue 
                           . map getPromise
                           $ quorum

            let responsiveClients = map (\k -> fromJust $ M.lookup k clients)
                                  . map getNodeId
                                  $ quorum

            doAccepts (AcceptRequest key pn valueToUse) responsiveClients

    where
    highestNackRoundNo :: [PrepareFail e] -> Maybe Word64
    highestNackRoundNo = go Nothing
        where
        go acc                    [] = getRoundNo <$> acc
        go acc            (Bad _:xs) = go acc xs
        go acc (Nacked (Nack na):xs) = go (max (Just na) acc) xs

    highestNumberedValue :: [Promise] -> Maybe Value
    highestNumberedValue promises = f (mapMaybe prom_highestProposal promises)
        where
        f [] = Nothing
        f rs = Just . getProposalValue
                    . maximumBy (comparing getProposalNumber)
                    $ rs

data PrepareFail e = Nacked !Nack
                   | Bad !e

data NodePromise = NodePromise { getNodeId  :: !NodeId,
                                 getPromise :: !Promise }

doPrepares :: Show e => Key
                     -> ProposalNumber
                     -> Map NodeId (NodeClient e)
                     -> IO (Either [PrepareFail String] [NodePromise])
doPrepares key n = asyncMajority . map doPrepare . M.toList

    where
    doPrepare :: Show e => (NodeId, NodeClient e) -> IO (Either (PrepareFail String) NodePromise)
    doPrepare (i, c) = handle <$> getPrepareClient c (PrepareRequest key n)
        where
        handle (Left  servantError) = Left  $ Bad (show servantError)
        handle (Right  (Left nack)) = Left  $ Nacked nack
        handle (Right (Right prom)) = Right $ NodePromise i prom

doAccepts :: AcceptRequest -> [NodeClient e] -> IO (Either String Value)
doAccepts acceptRequest responsiveClients = do

    let acceptClients = map getAcceptClient responsiveClients

    headMay . rights . rights <$> mapConcurrently (\c -> c acceptRequest) acceptClients >>= \case

        Just v  -> pure $ Right v

        Nothing -> pure $ Left "Not accepted"
