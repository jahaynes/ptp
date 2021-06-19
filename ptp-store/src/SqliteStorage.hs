{-# LANGUAGE BangPatterns,
             OverloadedStrings #-}

module SqliteStorage ( SqliteStorage
                     , create
                     ) where

import Storage

import Control.Concurrent.STM
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import RIO hiding (atomically, newTVarIO)

data SqliteStorage =
    SqliteStorage { conn  :: !Connection
                  , state :: !(TVar SqliteState)
                  }

data SqliteState =
    SqliteState { locked    :: !Bool
                , remaining :: !Int
                }

newtype SqlVal =
    SqlVal ByteString

instance FromRow SqlVal where
    fromRow = SqlVal <$> field

newtype SqlKey =
    SqlKey ByteString

instance ToField SqlKey where
    toField (SqlKey k) = toField k

instance ToField SqlVal where
    toField (SqlVal v) = toField v

insertionsPerTransaction :: Int
insertionsPerTransaction = 10

create :: String -> IO SqliteStorage
create name = do
    conn      <- open $ name <> ".db"
    execute_ conn "CREATE TABLE IF NOT EXISTS store (key BLOB PRIMARY KEY, val BLOB)"
    execute_ conn "BEGIN TRANSACTION"
    state <- newTVarIO $
        SqliteState { locked    = False
                    , remaining = 0
                    }
    pure $ SqliteStorage conn state

instance Storage SqliteStorage where
    readStore  = readStoreImpl
    writeStore = writeStoreImpl

readStoreImpl :: (MonadIO m, MonadThrow m, Key k, StoreValue v, NFData v)
              => SqliteStorage -> k -> m (Maybe v)
readStoreImpl sqliteStorage key = do
    vs <- liftIO $ query (conn sqliteStorage)
        " SELECT val from store \
        \ WHERE key = ?;        "
          (Only . SqlKey . toKeyBytes $ key)

    pure $ case vs of
        [SqlVal v] -> Just $ fromValBytes v
        []         -> Nothing

writeStoreImpl :: (MonadIO m, Key k, StoreValue v)
               => SqliteStorage -> k -> v -> m ()
writeStoreImpl sqliteStorage key val = do

    let !k = toKeyBytes key
        !v = toValBytes val

    liftIO $ bracket (atomically acquire) (atomically . release) $ \r -> do

        when (r == 0) $ do
            execute_ (conn sqliteStorage) "COMMIT TRANSACTION"
            execute_ (conn sqliteStorage) "BEGIN TRANSACTION"

        execute (conn sqliteStorage)
            " INSERT OR REPLACE INTO store (key, val) \
            \ VALUES                       (  ?,   ?) "
                (SqlKey k, SqlVal v)

    where
    acquire = do
        SqliteState l r <- readTVar (state sqliteStorage)
        if l
            then retry
            else do
                let r' = if r < 1 then insertionsPerTransaction else r - 1
                writeTVar (state sqliteStorage) (SqliteState True r')
                pure r

    release r =
        writeTVar (state sqliteStorage) (SqliteState False r)

instance LockedStorage SqliteStorage where
    readStoreL  = readStoreLImpl
    writeStoreL = writeStoreLImpl

readStoreLImpl :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v)
               => l -> SqliteStorage -> k -> m (Maybe v)
readStoreLImpl _locked = readStoreImpl

writeStoreLImpl :: (MonadIO m, Lock l, Key k, StoreValue v)
                => l -> SqliteStorage -> k -> v -> m ()
writeStoreLImpl _locked = writeStoreImpl