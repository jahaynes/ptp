module Server.Locks where

import           Control.Concurrent.STM
import           Control.DeepSeq        (NFData, deepseq)
import           Data.Set               (Set)
import qualified Data.Set as S

newtype Locked a =
    Locked a

newtype Locks a =
    Locks (TVar (Set a))
    {- TODO can possibly replace this with
        Data.Map k (TVar Bool)
        removing last depency on stm-containers
    -}

-- TODO catch deep
withLocked :: (NFData a, Ord k) => Locks k
                                -> k
                                -> (Locked k -> IO a)
                                -> IO a
withLocked locks k f = do
    lock <- atomically $ takeLock locks k
    y <- f lock
    y `deepseq` atomically (releaseLock locks lock)
    pure y

takeLock :: Ord k => Locks k -> k -> STM (Locked k)
takeLock (Locks tks) k = do
    ks <- readTVar tks
    if S.member k ks
        then retry
        else do writeTVar tks $! S.insert k ks
                pure (Locked k)

releaseLock :: Ord k => Locks k -> Locked k -> STM ()
releaseLock (Locks tks) (Locked k) =
    modifyTVar' tks $ \ks -> S.delete k ks

newLocks :: IO (Locks k)
newLocks = Locks <$> newTVarIO S.empty
