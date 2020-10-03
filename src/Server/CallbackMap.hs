{-# LANGUAGE LambdaCase #-}

module Server.CallbackMap ( CallbackMap (..)
                          , Notify (..)
                          , create
                          ) where

import           Entity.SequenceNum (SequenceNum)
import           Entity.Value       (Uniq)

import           Control.Concurrent.STM  (atomically, retry)
import           Data.Hashable           (Hashable)
import           StmContainers.Map       (Map)
import qualified StmContainers.Map as SM

-- TODO expiry

data Notify = NotifyDone !SequenceNum
            | NotifyRetry

data CallbackMap =
    CallbackMap { putCallback  :: !(Uniq -> Notify -> IO ())
                , waitCallback :: !(Uniq -> IO Notify)
                }

create :: IO CallbackMap
create = do

    callbackMap <- SM.newIO

    pure $ CallbackMap { putCallback  = putCallbackImpl  callbackMap
                       , waitCallback = waitCallbackImpl callbackMap
                       }

putCallbackImpl :: (Eq k, Hashable k) => Map k v
                                      -> k
                                      -> v
                                      -> IO ()
putCallbackImpl callbackMap uniq notify = atomically $
    SM.insert notify uniq callbackMap

waitCallbackImpl :: (Eq k, Hashable k) => Map k v
                                       -> k
                                       -> IO v
waitCallbackImpl callbackMap uniq = atomically $
    SM.lookup uniq callbackMap >>= \case
        Nothing     -> retry
        Just notify -> pure notify
