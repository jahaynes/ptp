{-# LANGUAGE BangPatterns,
             OverloadedStrings #-}

module SqliteStorage ( SqliteStorage
                     , create
                     , shutdown
                     ) where

import Storage

import Control.Concurrent.STM
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import RIO hiding (atomically, newTVarIO, readTVarIO)

data SqliteStorage =
    SqliteStorage { conn  :: !Connection
                  , state :: !(TVar SqliteState)
                  }

data TransactionState = NoTransaction
                      | RemainingInserts !Int

data SqliteState =
    SqliteState { locked           :: !Bool
                , transactionState :: !TransactionState
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
    c <- open $ name <> ".db"
    execute_ c "CREATE TABLE IF NOT EXISTS store (key BLOB PRIMARY KEY, val BLOB)"
    ss <- newTVarIO $
        SqliteState { locked           = False
                    , transactionState = NoTransaction
                    }
    pure $ SqliteStorage c ss

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

    let c  = conn sqliteStorage
        !k = toKeyBytes key
        !v = toValBytes val

    liftIO . bracket_ (acquireLock sqliteStorage) (releaseLock sqliteStorage) $ do

        s <- readTVarIO (state sqliteStorage)

        -- TODO check done each 10
        let (begin, commit, ts') =
                case transactionState s of
                    NoTransaction   -> (True,  False, RemainingInserts (insertionsPerTransaction - 1))
                    RemainingInserts ri
                        | ri < 1    -> (False,  True, NoTransaction)
                        | otherwise -> (False, False, RemainingInserts (ri - 1))

        when begin $
            execute_ c "BEGIN TRANSACTION"

        -- TODO see how good the ctrl-c handling is by sleeping here while holding the lock

        -- TODO check result?
        _ <- execute c
                 "INSERT OR REPLACE INTO store (key, val) VALUES (?, ?)"
                 (SqlKey k, SqlVal v)

        when commit $
            execute_ c "COMMIT TRANSACTION"

        atomically $ modifyTVar' (state sqliteStorage) $ \s' -> s' { transactionState = ts' }

acquireLock :: SqliteStorage -> IO ()
acquireLock sqliteStorage = atomically $ do
    s <- readTVar (state sqliteStorage)
    if locked s
        then retry
        else modifyTVar' (state sqliteStorage) $ \s' -> s' { locked = True }

releaseLock :: SqliteStorage -> IO ()
releaseLock sqliteStorage = atomically $
    modifyTVar' (state sqliteStorage) $ \s -> s { locked = False }

instance LockedStorage SqliteStorage where
    readStoreL  = readStoreLImpl
    writeStoreL = writeStoreLImpl

readStoreLImpl :: (MonadIO m, MonadThrow m, Lock l, Key k, StoreValue v, NFData v)
               => l -> SqliteStorage -> k -> m (Maybe v)
readStoreLImpl _locked = readStoreImpl

writeStoreLImpl :: (MonadIO m, Lock l, Key k, StoreValue v)
                => l -> SqliteStorage -> k -> v -> m ()
writeStoreLImpl _locked = writeStoreImpl

shutdown :: MonadIO m => SqliteStorage -> m ()
shutdown sqliteStorage = do

    let c = conn sqliteStorage

    liftIO . bracket_ (acquireLock sqliteStorage) (releaseLock sqliteStorage) $ do

        s <- readTVarIO (state sqliteStorage)

        let commit =
                case transactionState s of
                    NoTransaction      -> False
                    RemainingInserts{} -> True

        when commit $
            execute_ c "COMMIT TRANSACTION"

        execute_ c "VACUUM"

        atomically $ modifyTVar' (state sqliteStorage) $ \s' -> s' { transactionState = NoTransaction }
