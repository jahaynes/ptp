{-# LANGUAGE LambdaCase #-}

module Server.Locks where

import           Control.Concurrent.STM
import           Control.DeepSeq            (NFData, deepseq)
import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Hashable              (Hashable)
import           StmContainers.Set      (Set)
import qualified StmContainers.Set as S

newtype Locked a =
    Locked a

newtype Locks a =
    Locks (Set a)

withLocked :: (NFData a, Eq k, Hashable k) => Locks k
                                           -> k
                                           -> (Locked k -> ExceptT SomeException IO a)
                                           -> ExceptT SomeException IO a
withLocked locks k f = do
    lock <- liftIO . atomically $ takeLock locks k
    y <- f lock
    y `deepseq` liftIO (atomically (releaseLock locks lock))
    pure y

takeLock :: (Eq k, Hashable k) => Locks k -> k -> STM (Locked k)
takeLock (Locks ks) k =
    S.lookup k ks >>= \case
        True  -> retry
        False -> do S.insert k ks
                    pure (Locked k)

releaseLock :: (Eq k, Hashable k) => Locks k -> Locked k -> STM ()
releaseLock (Locks ks) (Locked k) = S.delete k ks

newLocks :: IO (Locks k)
newLocks = Locks <$> S.newIO
