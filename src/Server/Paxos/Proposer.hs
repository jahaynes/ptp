{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Server.Paxos.Proposer ( Proposer (..)
                             , create
                             ) where

import Client.WebNodeClient  (AcceptClient, PrepareClient)
import Entity.AcceptRequest
import Entity.Key
import Entity.Nack
import Entity.PrepareRequest
import Entity.PrepareResponse
import Entity.Promise
import Entity.Proposal
import Entity.ProposalNumber (ProposalNumber (..), newProposalNumber)
import Entity.ProposeRequest
import Entity.ProposeResponse
import Entity.Value
import Entity.ValueResponse
import Node
import Quorum

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Either              (rights)
import           Data.List                (maximumBy)
import           Data.Maybe               (fromMaybe, mapMaybe)
import           Data.Ord                 (comparing)
import           Data.Set                 (Set)
import qualified Data.Set as S
import           Data.Word                (Word64)
import           Network.HTTP.Client
import           Safe                     (headMay)

newtype Proposer m =
    Proposer { propose :: ProposeRequest -> m ProposeResponse
             }

create :: (MonadIO m, Show e) => (Manager -> Node -> PrepareClient e)
                              -> (Manager -> Node -> AcceptClient e)
                              -> IO (Proposer m)
create prepareBuilder acceptBuilder = do

    http <- newManager $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutMicro 3000000 }

    pure $ Proposer { propose = liftIO . proposeService (prepareBuilder http) (acceptBuilder http)
                    }

proposeService :: Show e => (Node -> PrepareClient e)
                         -> (Node -> AcceptClient e)
                         -> ProposeRequest
                         -> IO ProposeResponse
proposeService prepareBuilder acceptBuilder (ProposeRequest nodes key value) = do

    initialProposalNum <- newProposalNumber
    doProposal initialProposalNum value

    where
    doProposal :: ProposalNumber -> Value -> IO ProposeResponse
    doProposal pn defaultVal =

        doPrepares nodes key pn prepareBuilder >>= \case

            Left errors ->
                case highestNackRoundNo errors of
                            Nothing  -> pure NoHighestNackRoundNo
                            Just hrn -> doProposal (pn {getRoundNo = succ hrn}) defaultVal

            Right quorum ->

                let valueToUse = fromMaybe defaultVal
                               . highestNumberedValue
                               . map getPromise
                               $ quorum

                    responsiveNodes = map (\(NodePromise node _) -> node) quorum

                -- TODO.  sending all nodes here, or just responsive nodes?
                -- in either case should the two instances match?
                in doAccepts (AcceptRequest nodes key pn valueToUse) responsiveNodes acceptBuilder

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

data NodePromise = NodePromise { getNodeId  :: !Node,
                                 getPromise :: !Promise } deriving Show

doPrepares :: Show e => Set Node
                     -> Key
                     -> ProposalNumber
                     -> (Node -> PrepareClient e)
                     -> IO (Either [PrepareFail String] [NodePromise])
doPrepares nodes key n prepareBuilder =

    asyncMajority (doPrepare <$> S.toList nodes)

    where
    doPrepare :: Node -> IO (Either (PrepareFail String) NodePromise)
    doPrepare node = handle . fmap (\(PrepareResponse r) -> r) <$> prepareBuilder node (PrepareRequest key n)

        where
        handle ((Left  servantError)) = Left  $ Bad (show servantError)
        handle ((Right  (Left nack))) = Left  $ Nacked nack
        handle ((Right (Right prom))) = Right $ NodePromise node prom

doAccepts :: Show e => AcceptRequest
                    -> [Node]
                    -> (Node -> AcceptClient e)
                    -> IO ProposeResponse
doAccepts acceptRequest responsiveNodes acceptBuilder = do

    let acceptClients = map acceptBuilder responsiveNodes

    headMay . rights . rights . map (fmap (\(ValueResponseE r) -> r)) <$> mapConcurrently (\c -> c acceptRequest) acceptClients >>= \case

        Just v  -> pure $ Accepted v

        Nothing -> pure $ NotAccepted "No Acceptor accepted"
