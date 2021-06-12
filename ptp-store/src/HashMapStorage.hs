{-# LANGUAGE OverloadedStrings #-}

module HashMapStorage ( HashMapStorage
                      , create
                      ) where

import Storage

import           RIO
import qualified RIO.HashMap as HM

newtype HashMapStorage =
    HashMapStorage (IORef (HashMap ByteString ByteString))

create :: MonadIO m => m HashMapStorage
create = HashMapStorage <$> newIORef HM.empty

instance Storage HashMapStorage where
    readStore  = readStoreImpl
    writeStore = writeStoreImpl

readStoreImpl :: (MonadIO m, MonadThrow m, Key k, StoreValue v, NFData v) => HashMapStorage -> k -> m (Maybe v)
readStoreImpl (HashMapStorage ioref) key =
    readIORef ioref <&> \hm ->
        case HM.lookup (toKeyBytes key) hm of
            Nothing    -> Nothing
            Just bytes -> force . Just $ fromValBytes bytes

writeStoreImpl :: (MonadIO m, Key k, StoreValue v) => HashMapStorage -> k -> v -> m ()
writeStoreImpl (HashMapStorage ioref) key val =
    modifyIORef' ioref $ HM.insert (toKeyBytes key) (toValBytes val)

instance LockedStorage HashMapStorage where
    readStoreL  = readStoreLImpl
    writeStoreL = writeStoreLImpl

readStoreLImpl :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v)
               => l -> HashMapStorage -> k -> m (Maybe v)
readStoreLImpl _locked = readStoreImpl

writeStoreLImpl :: (MonadIO m, Lock l, Key k, StoreValue v)
                => l -> HashMapStorage -> k -> v -> m ()
writeStoreLImpl _locked = writeStoreImpl