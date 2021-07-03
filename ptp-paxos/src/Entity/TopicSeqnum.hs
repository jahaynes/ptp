{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Entity.TopicSeqnum where

import Entity.SequenceNum
import Entity.Topic
import Storage

import Codec.Serialise     (Serialise, serialise)
import RIO.ByteString.Lazy (toStrict)
import RIO                 (Generic, Hashable)

data TopicSeqnum =
    TopicSeqnum !Topic !SequenceNum
        deriving (Eq, Generic, Hashable, Serialise)

instance Key TopicSeqnum where
    toKeyBytes = toStrict . serialise