{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, InstanceSigs #-}

module SqliteStorage ( SqliteStorage
                     , create
                     ) where

import Storage

import           Control.Monad.IO.Class (liftIO)
import           Database.SQLite.Simple     
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField


import           RIO
import           RIO.ByteString             (intercalate)
import qualified RIO.HashMap as HM
import           RIO.List                   (sort)

newtype SqliteStorage =
    SqliteStorage Connection

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

create :: String -> IO SqliteStorage
create name = do
    conn <- open $ name <> ".db"
    execute_ conn "CREATE TABLE IF NOT EXISTS store (key BLOB PRIMARY KEY, val BLOB)"
    pure $ SqliteStorage conn

instance Storage SqliteStorage where
    readStore  = readStoreImpl
    writeStore = writeStoreImpl

readStoreImpl :: (MonadIO m, MonadThrow m, Key k, StoreValue v, NFData v)
              => SqliteStorage -> k -> m (Maybe v)
readStoreImpl (SqliteStorage conn) key = do

    vs <- liftIO $ query conn
        " SELECT val from store \
        \ WHERE key = ?;        "
          (Only . SqlKey . toKeyBytes $ key)

    pure $ case vs of
        [SqlVal v] -> Just $ fromValBytes v
        []         -> Nothing

writeStoreImpl :: (MonadIO m, Key k, StoreValue v)
               => SqliteStorage -> k -> v -> m ()
writeStoreImpl (SqliteStorage conn) key val = do
    let k = toKeyBytes key
        v = toValBytes val
    liftIO $ execute conn " INSERT OR REPLACE INTO store (key, val) \
                          \ VALUES                       (  ?,   ?) "
        (SqlKey k, SqlVal v)

instance LockedStorage SqliteStorage where
    readStoreL  = readStoreLImpl
    writeStoreL = writeStoreLImpl

readStoreLImpl :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v)
               => l -> SqliteStorage -> k -> m (Maybe v)
readStoreLImpl _locked = readStoreImpl

writeStoreLImpl :: (MonadIO m, Lock l, Key k, StoreValue v)
                => l -> SqliteStorage -> k -> v -> m ()
writeStoreLImpl _locked = writeStoreImpl