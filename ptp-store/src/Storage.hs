module Storage where

import RIO

class Key k where
    toKeyBytes :: k -> ByteString

class StoreValue v where
    toValBytes   :: v -> ByteString
    fromValBytes :: ByteString -> v

class Storage s where
    readStore  :: (MonadIO m, MonadThrow m, Key k, StoreValue v, NFData v) => s -> k -> m (Maybe v)
    writeStore :: (MonadIO m, Key k, StoreValue v) => s -> k -> v -> m ()

class Lock l

class LockedStorage s where
    readStoreL  :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v) => l -> s -> k -> m (Maybe v)
    writeStoreL :: (MonadIO m, Lock l, Key k, StoreValue v) => l -> s -> k -> v -> m ()
