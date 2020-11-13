{-# LANGUAGE LambdaCase #-}

module Submitter where

import           Entity.ClusterHash
import           Entity.Decree
import           Entity.Msg
import           Requests.CreateTopic
import           Requests.Submit
import           Requests.Sync

import           Client.PaxosClient
import           Entity.Node
import           Entity.SequenceNum
import           Entity.Topic
import           Entity.Uniq
import           Entity.Value
import           Requests.Peek
import           Requests.Propose

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Functor            ((<&>))
import qualified Data.Map as Map
import           Data.Maybe               (mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set as S
import           Network.HTTP.Client     (Manager)
import           Servant.Client          (ClientError)
import           StmContainers.Map       (Map)
import qualified StmContainers.Map as M
import           System.Random           (randomRIO)
import           Text.Printf             (printf)

data Submitter m =
    Submitter { createTopic :: !(CreateTopicRequest -> m CreateTopicResponse)
              , sync        :: !(SyncRequest        -> m SyncResponse)
              , submit      :: !(SubmitRequest      -> m SubmitResponse)
              }

newtype StateMachines =
    StateMachines (Map Topic StateMachine)

data StateMachine =
    StateMachine { getHighestKnown :: !SequenceNum
                 , getCluster      :: !(Set Node)
                 , getLeader       :: !(Maybe Node)
                 }

createTopicImpl :: StateMachines
                -> CreateTopicRequest
                -> IO CreateTopicResponse
createTopicImpl (StateMachines m) (CreateTopicRequest nodes topic) = do
    let sm = StateMachine { getHighestKnown = SequenceNum 0
                          , getCluster      = S.fromList nodes
                          , getLeader       = Nothing
                          }
    atomically $ M.insert sm topic m
    pure CreateTopicResponse

create :: MonadIO m => Node -> Manager -> IO (Submitter m)
create me http = do
    stateMachines <- StateMachines <$> M.newIO
    pure $ Submitter { createTopic = liftIO . createTopicImpl stateMachines
                     , sync        = liftIO . syncImpl http stateMachines
                     , submit      = liftIO . submitImpl me http stateMachines
                     }

syncImpl :: Manager
         -> StateMachines
         -> SyncRequest
         -> IO SyncResponse
syncImpl http stateMachines@(StateMachines sms) (SyncRequest topic) = do

    -- Get the state machine state
    Just (StateMachine _ cluster _) <- atomically $ M.lookup topic sms

    let peekClients = map (peekBuilder http) (S.toList cluster)

    let start = SequenceNum 1
    end <- go cluster peekClients start

    pure $ SyncResponse start end

    where
    go cluster peekClients (SequenceNum n) = do

        let hi = n + 9
            range = SequenceNum <$> [n .. hi]

        msvs <- mapMaybe handlePeek <$> mapM (\pc -> pc (PeekRequest topic range)) peekClients

        let numNodes = S.size cluster
            reachableNodes = length msvs

        if (reachableNodes < numNodes)

            then do
                -- Could not reach all nodes.  'Safe side' is loop till the node comes back up
                printf "Could not sync... Only (%d/%d) nodes reachable\n" reachableNodes numNodes
                threadDelay 1000000
                go cluster peekClients (SequenceNum n)

            else

                case same msvs of

                    Nothing -> do
                        putStrLn "Missing records detected: "
                        forM_ msvs $ \msv -> do
                            mapM_ print $ Map.toList msv
                            putStrLn ""
                        putStrLn "Repairing..."

                        -- repair
                        repair (S.size cluster) msvs

                        -- run again
                        go cluster peekClients (SequenceNum n)

                    Just msvs' ->

                        let svs = Map.toAscList msvs'

                        in if null svs

                                -- No results, so we're done
                                then pure (SequenceNum hi)

                                else do

                                    forM_ svs $ \(seqNum, val) ->

                                        observe stateMachines topic seqNum (fromValue val)

                                    go cluster peekClients (SequenceNum $! n+10)

        where
        handlePeek :: Either l PeekResponse -> Maybe (Map.Map SequenceNum Value)
        handlePeek (Right (PeekResponse msv))    = Just msv
        handlePeek (Right (PeekResponseError _)) = Nothing
        handlePeek (Left _)                      = Nothing

        repair :: Int -> [Map.Map SequenceNum Value] -> IO ()
        repair numNodes msvs = do

            let superMap = Map.unionsWith (\a b -> if a == b then a else error "irreparable mismatch") msvs

            let missingKeys = map fst
                            . filter (\(_, v) -> v < numNodes)
                            . Map.toList
                            . Map.unionsWith (+)
                            . fmap (const 1 <$>)
                            $ msvs

            forM_ missingKeys $ \key -> do

                let Just val = Map.lookup key superMap

                printf "Resubmitting: %s -> %s\n" (show key) (show val)

                -- _ <- submitImpl me http stateMachines topic decree
                proposeClient <- chooseProposeClient http cluster

                -- Don't 'handle' in repair.  Let 'go' restart this batch and handle it there
                _ <- proposeClient (ProposeRequest cluster topic key val)

                pure ()

submitImpl :: Node
           -> Manager
           -> StateMachines
           -> SubmitRequest
           -> IO SubmitResponse
submitImpl me http stateMachines@(StateMachines sms) (SubmitRequest topic decree) = do

    -- Get the state machine state
    Just (StateMachine highestKnown cluster mLeader) <- atomically $ M.lookup topic sms

    case mLeader of

        Nothing -> do

            proposeClient <- chooseProposeClient http cluster

            u <- uniq
            let seqNum = next highestKnown
                ch     = clusterHash cluster
                elect  = ElectLeader me
                value  = toValue (Msg u ch elect)

            -- TODO Warn - original decree was not sent
            proposeClient (ProposeRequest cluster topic seqNum value) >>= handle seqNum value

        Just leader

            | leader == me -> do

                u <- uniq
                let seqNum = next highestKnown
                    ch     = clusterHash cluster
                    value  = toValue (Msg u ch decree)

                -- Pick an arbitrary node in the cluster
                proposeClient <- chooseProposeClient http cluster

                -- Propose to the cluster
                proposeClient (ProposeRequest cluster topic seqNum value) >>= handle seqNum value

            | otherwise -> pure $ OtherLeader leader

    where
    handle :: SequenceNum
           -> Value
           -> Either ClientError ProposeResponse
           -> IO SubmitResponse

    handle _ _ (Left e) = pure . SubmitError $ show e

    handle seqNum val (Right r) =

        case r of

            Accepted val' -> do

                observe stateMachines topic seqNum (fromValue val')

                if val' == val
                    then pure Submitted
                    else pure RetryRequested

            NotAccepted x ->
                pure . SubmitError $ printf "Paxos cluster didn't accept %s -> %s. %s.  Not enough nodes alive?" (show seqNum) (show val) (show x)

            NoHighestNackRoundNo ->
                pure $ SubmitError "Paxos cluster could not find a Nack among responses.  Not enough nodes alive?"

chooseProposeClient :: Manager -> Set Node -> IO (ProposeClient ClientError)
chooseProposeClient http cluster = proposeBuilder http <$> choice (S.toList cluster)
    where
    choice :: [a] -> IO a
    choice [] = error "No choice"
    choice xs = randomRIO (0, length xs - 1) <&> \r -> xs !! r

observe :: StateMachines
        -> Topic
        -> SequenceNum
        -> Msg
        -> IO ()
observe (StateMachines sms) topic seqNum (Msg u ch decree) =

    -- check u  ?
    -- check ch ?

    atomically $ do

        M.lookup topic sms >>= \case

            Nothing ->

                error "No topic state B"

            Just topicState ->

                let topicState' =

                        let leader' = case decree of
                                          ElectLeader leader -> Just leader
                                          _                  -> getLeader topicState

                            highest' = max seqNum (getHighestKnown topicState)

                        in topicState { getHighestKnown = highest'
                                      , getLeader       = leader'
                                      }

                in M.insert topicState' topic sms

same :: Eq a => [a] -> Maybe a
same     [] = error "nothing same"
same (x:xs) | all (==x) xs = Just x
            | otherwise    = Nothing
