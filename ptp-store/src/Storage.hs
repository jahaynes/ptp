module Storage where

import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.IO.Unlift    (MonadUnliftIO)
import RIO.ByteString             (ByteString)
import RIO.Prelude.Types          (MonadThrow, NFData, Text)

class Key k where
    toKeyBytes :: k -> ByteString

class StoreValue v where
    toValBytes   :: v -> ByteString
    fromValBytes :: ByteString -> v

class Storage s where
    readStore  :: (MonadThrow m, MonadUnliftIO m, Key k, StoreValue v, NFData v) => s -> k -> m (Maybe v)
    writeStore :: (MonadUnliftIO m, Key k, StoreValue v) => s -> k -> v -> m ()

class ShowIO s where
    showIO :: s -> IO ByteString

class Lock l

class LockedStorage s where
    readStoreL  :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v) => l -> s -> k -> m (Maybe v)
    writeStoreL :: (MonadIO m, Lock l, Key k, StoreValue v) => l -> s -> k -> v -> m ()