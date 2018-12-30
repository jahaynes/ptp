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

doProposal :: ProposalNumber -> Value -> Map NodeId NodeClient -> IO (Either String Value)
doProposal pn defaultVal clients = do

    eQuorum <- doPrepares pn clients

    case eQuorum of

        Left errors -> handle errors

        Right quorum -> doAccepts (AcceptRequest pn (valueToUse quorum)) (getResponsiveClients quorum)

    where
    getResponsiveClients = map ((\k -> fromJust $ M.lookup k clients) . getNodeId)

    valueToUse = fromMaybe defaultVal
               . highestNumberedValue 
               . map getPromise

        where
        highestNumberedValue :: [Promise] -> Maybe Value
        highestNumberedValue promises = f (mapMaybe prom_highestProposal promises)
            where
            f [] = Nothing
            f rs = Just . getProposalValue
                        . maximumBy (comparing getProposalNumber)
                        $ rs

    handle :: [PrepareFail e] -> IO (Either String Value)
    handle errors =
        case highestNackRoundNo errors of
            Nothing -> pure . Left $ "No highestNackRoundNo"
            Just hrn -> doProposal pn {getRoundNo = succ hrn} defaultVal clients

        where
        highestNackRoundNo :: [PrepareFail e] -> Maybe Int
        highestNackRoundNo = go Nothing
            where
            go acc                    [] = getRoundNo <$> acc
            go acc            (Bad _:xs) = go acc xs
            go acc (Nacked (Nack na):xs) = go (max (Just na) acc) xs

data PrepareFail e = Nacked Nack
                   | Bad e

data NodePromise = NodePromise { getNodeId  :: !NodeId,
                                 getPromise :: !Promise }

doPrepares :: ProposalNumber -> Map NodeId NodeClient -> IO (Either [PrepareFail String] [NodePromise])
doPrepares n = asyncMajority . map doPrepare . M.toList

    where
    doPrepare :: (NodeId, NodeClient) -> IO (Either (PrepareFail String) NodePromise)
    doPrepare (i, c) = handle <$> getPrepareClient c (PrepareRequest n)
        where
        handle (Left  servantError) = Left  $ Bad (show servantError)
        handle (Right  (Left nack)) = Left  $ Nacked nack
        handle (Right (Right prom)) = Right $ NodePromise i prom

doAccepts :: AcceptRequest -> [NodeClient] -> IO (Either String Value)
doAccepts acceptRequest responsiveClients = do
    successes <- rights <$> mapConcurrently ((\c -> c acceptRequest) . getAcceptClient) responsiveClients
    pure $ case headMay $ rights successes of
               Just v  -> Right v
               Nothing -> Left "Not accepted"
