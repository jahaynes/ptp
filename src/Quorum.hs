module Quorum where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM   (atomically, modifyTVar', newTVarIO, readTVar, retry)
import Control.Monad            (forM_)

newtype Threshold = Threshold Int

asyncMajority :: [IO (Either l r)] -> IO (Either [l] [r])
asyncMajority ioJobs = do

    let numJobs = length ioJobs
        (Threshold t) = threshold numJobs 

    tRemaining <- newTVarIO numJobs
    tDone <- newTVarIO []
    tFail <- newTVarIO []

    forM_ ioJobs $ \ioj -> async $ do
        er <- ioj
        atomically $ do
            modifyTVar' tRemaining (\x -> x - 1)
            case er of
                Right r -> modifyTVar' tDone (r:)
                Left  l -> modifyTVar' tFail (l:)

    atomically $ do
        done <- readTVar tDone
        failed <- readTVar tFail
        remaining <- readTVar tRemaining

        let tieOutcome  = remaining == 0 && length done == length failed 
            failOutcome = length failed >= t
            passOutcome = length done >= t

        if tieOutcome || failOutcome
            then pure $ Left failed
            else if passOutcome
                     then pure $ Right done
                     else retry

threshold :: Int -> Threshold
threshold num = Threshold (1 + div num 2)

{- Boyer-Moore majority vote -}
majority :: Eq a => [a] -> Threshold -> Maybe a
majority ls (Threshold mn) =
    let c = candidate ls
    in if isMajority ls c then Just c else Nothing

    where
    isMajority :: Eq a => [a] -> a -> Bool
    isMajority xs y = (length . filter (==y) $ xs) >= mn

    candidate :: Eq a => [a] -> a
    candidate = go undefined (0 :: Int)
        where
        go m _     [] = m
        go m i (x:xs) | i == 0    = go x     1 xs
                      | m == x    = go m (i+1) xs
                      | otherwise = go m (i-1) xs
