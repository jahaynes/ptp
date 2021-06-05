{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             MultiParamTypeClasses #-}

-- Mostly for testing
module Server.InMemStorage where

import Entity.SequenceNum
import Entity.Topic
import Server.Locks
import Server.Storage

import Control.Exception          (SomeException)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT)

import           Data.IORef
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M

newtype InMemStorage a =
    InMemStorage (IORef (Map (Topic, SequenceNum) a))

create :: IO (InMemStorage a)
create = InMemStorage <$> newIORef M.empty

instance Storage (InMemStorage a) a where
    
    readTopicSequence :: InMemStorage a
                      -> Locked (Topic, SequenceNum)
                      -> ExceptT SomeException IO (Maybe a)
    readTopicSequence (InMemStorage m) (Locked (topic, seqNum)) =
        M.lookup (topic, seqNum) <$> liftIO (readIORef m)

    writeTopicSequence :: InMemStorage a
                       -> Locked (Topic, SequenceNum)
                       -> a
                       -> ExceptT SomeException IO ()
    writeTopicSequence (InMemStorage m) (Locked (topic, seqNum)) x =
        liftIO $ atomicModifyIORef m $ \m' -> (M.insert (topic, seqNum) x m', ())