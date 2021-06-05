{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Storage where

import Entity.SequenceNum
import Entity.Topic
import Server.Locks

import Control.Exception
import Control.Monad.Trans.Except

class Storage s a where

    readTopicSequence  :: s -> Locked (Topic, SequenceNum) -> ExceptT SomeException IO (Maybe a)

    writeTopicSequence :: s -> Locked (Topic, SequenceNum) -> a -> ExceptT SomeException IO ()
