{-# LANGUAGE LambdaCase #-}

module Proposal where

import ProposalNumber
import Quorum
import Types
import WebNodeClient

import Control.Concurrent.Async (mapConcurrently)
import Data.Either              (rights)
import Data.List                (maximumBy)
import Data.Map                 (Map)
import Data.Maybe               (fromJust, fromMaybe, mapMaybe)
import Data.Ord                 (comparing)
import qualified Data.Map as M
import Safe                     (headMay)

newtype NodeId = NodeId Int deriving (Eq, Ord)

doProposal :: Show e => ProposalNumber
                     -> Value
                     -> Map NodeId (NodeClient e)
                     -> IO (Either String Value)
doProposal pn defaultVal clients =

    doPrepares pn clients >>= \case

        Left errors -> case highestNackRoundNo errors of
                           Nothing  -> pure . Left $ "No highestNackRoundNo"
                           Just hrn -> doProposal pn {getRoundNo = succ hrn} defaultVal clients

        Right quorum -> do

            let valueToUse = fromMaybe defaultVal
                           . highestNumberedValue 
                           . map getPromise
                           $ quorum

            let responsiveClients = map (\k -> fromJust $ M.lookup k clients)
                                  . map getNodeId
                                  $ quorum

            doAccepts (AcceptRequest pn valueToUse) responsiveClients

    where
    highestNackRoundNo :: [PrepareFail e] -> Maybe Int
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

doPrepares :: Show e => ProposalNumber
                     -> Map NodeId (NodeClient e)
                     -> IO (Either [PrepareFail String] [NodePromise])
doPrepares n = asyncMajority . map doPrepare . M.toList

    where
    doPrepare :: Show e => (NodeId, NodeClient e) -> IO (Either (PrepareFail String) NodePromise)
    doPrepare (i, c) = handle <$> getPrepareClient c (PrepareRequest n)
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
