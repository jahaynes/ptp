{-# LANGUAGE BangPatterns,
             NumericUnderscores,
             OverloadedStrings #-}

module SqliteStorage ( SqliteStorage
                     , create
                     , vacuumAndClose
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
    SqliteState { closing          :: !Bool -- probably don't need
                , locked           :: !Bool
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
        SqliteState { closing          = False
                    , locked           = False
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
acquireLock sqliteStorage = do
    abort <- atomically $ do
        s <- readTVar (state sqliteStorage)
        if closing s
            then pure True
            else if locked s
                then retry
                else do
                    modifyTVar' (state sqliteStorage) $ \s' -> s' { locked = True }
                    pure False
    when abort (error "Tried to write after close")

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

vacuumAndClose :: MonadIO m => SqliteStorage -> m ()
vacuumAndClose sqliteStorage = do

    let c = conn sqliteStorage

    liftIO . bracket_ (acquireLock sqliteStorage) (releaseLock sqliteStorage) $ do

        s <- readTVarIO (state sqliteStorage)

        let commit =
                case transactionState s of
                    NoTransaction      -> False
                    RemainingInserts{} -> True

        when commit $ do
            putStrLn "About to commit"
            execute_ c "COMMIT TRANSACTION"
            putStrLn "Committed"

        putStrLn "About to vacuum"
        execute_ c "VACUUM"
        putStrLn "Vacuumed"

        threadDelay 100_000

        putStrLn "About to close"
        close c -- So that further writes will fail
        putStrLn "Closed"

        atomically $ modifyTVar' (state sqliteStorage) $ \s' -> s' { closing = True
                                                                   , transactionState = NoTransaction }
