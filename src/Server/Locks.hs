module Server.Locks where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
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
withLocked (Locks tks) k f = do
    takeLock
    y <- f (Locked k)
    y `deepseq` releaseLock
    pure y

    where
    takeLock :: IO ()
    takeLock = atomically $ do
        ks <- readTVar tks
        if S.member k ks
            then retry
            else writeTVar tks $! S.insert k ks

    releaseLock :: IO ()
    releaseLock = atomically $
        modifyTVar' tks $ \ks -> S.delete k ks

newLocks :: IO (Locks k)
newLocks = Locks <$> newTVarIO S.empty
