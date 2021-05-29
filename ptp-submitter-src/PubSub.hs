{-# LANGUAGE LambdaCase,
             OverloadedLists #-}

module PubSub where

import Entity.SequenceNum (SequenceNum (..), next)
import Entity.Topic       (Topic)

import           Control.Concurrent.STM
import           Data.Set               (Set, (\\))
import qualified Data.Set as S
import           Data.Maybe             (fromJust, fromMaybe)
import           StmContainers.Map      (Map)
import qualified StmContainers.Map as M

data PubSub =
    PubSub { notify :: !(Topic -> SequenceNum -> IO ())
           , poll   :: !(Topic -> IO (Maybe SequenceNum))
           }

data TopicState =
    TopicState { getLatest :: !SequenceNum
               , getAlso   :: !(Set SequenceNum)
               } deriving Show

newtype TopicStates =
    TopicStates (Map Topic TopicState)

create :: IO PubSub
create = do

    topicStates <- TopicStates <$> M.newIO

    pure PubSub { notify = notifyImpl topicStates
                , poll   = pollImpl topicStates
                }

notifyImpl :: TopicStates
           -> Topic
           -> SequenceNum
           -> IO ()
notifyImpl (TopicStates ts) topic seqNum =

    atomically $ do

        -- Get or create topic state
        topicState <- fromMaybe (TopicState (SequenceNum 1) S.empty)
                   <$> M.lookup topic ts

        -- Logic!
        let topicState' = step seqNum topicState

        -- Write
        M.insert topicState' topic ts

    where
    step :: SequenceNum
         -> TopicState
         -> TopicState
    step n s =

        -- Ignore too small
        if n <= getLatest s

            then s

            else

                if n == next (getLatest s)

                    -- Good fit
                    then

                        -- Try again?
                        if S.size (getAlso s \\ [n]) > 0

                            -- Big enough
                            then step (setMin (getAlso s \\ [n]))
                                      (TopicState n (getAlso s \\ [n]))

                            -- Too small
                            else TopicState n (getAlso s \\ [n])

                    -- Slot it in later
                    else s { getAlso = S.insert n (getAlso s) }

        where
        setMin :: Set c -> c
        setMin = fst . fromJust . S.minView

pollImpl :: TopicStates
         -> Topic
         -> IO (Maybe SequenceNum)
pollImpl (TopicStates ts) topic =
    atomically (fmap getLatest <$> M.lookup topic ts)