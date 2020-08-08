{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase #-}

module Server.Keylocks where

import           Control.Concurrent.STM (atomically, retry)
import           Data.Hashable          (Hashable)
import           GHC.Generics           (Generic)
import           StmContainers.Set      (Set)
import qualified StmContainers.Set as S

newtype Locked a =
    Locked a
        deriving (Eq, Generic, Hashable)

newtype Locks a =
    Locks (Set a)
    {- TODO can possibly replace this with
        Data.Map k (TVar Bool)
        removing last depency on stm-containers
    -}

-- TODO catch deep
withLockedKey :: (Eq k, Hashable k) => Locks k
                                    -> k
                                    -> (Locked k -> IO a)
                                    -> IO a
withLockedKey (Locks ks) k f = do
    takeLock
    y <- f (Locked k)
    releaseLock
    pure y

    where
    takeLock :: IO ()
    takeLock = atomically $
        S.lookup k ks >>= \case
            True  -> retry
            False -> S.insert k ks

    releaseLock :: IO ()
    releaseLock = atomically $ S.delete k ks

newLocks :: IO (Locks k)
newLocks = Locks <$> S.newIO
