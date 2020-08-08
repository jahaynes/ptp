module Server.Files where

import SequenceNum
import Server.Keylocks
import Topic
import Types

import Codec.Serialise        (Serialise, serialise, deserialise)
import Control.DeepSeq        (NFData, deepseq)
import Control.Exception.Safe (catchIO)
import Data.ByteString.Lazy   (hGetContents, hPut)
import GHC.IO.IOMode          (IOMode (ReadMode, WriteMode))
import RIO.File               (withBinaryFileDurableAtomic)
import System.Directory       (removeFile)
import System.IO              (withBinaryFile)
import Text.Printf            (printf)

-- todo exceptions
writeState :: Serialise a => Id
                          -> Locked Key
                          -> String
                          -> a
                          -> IO ()
writeState ident key filetype x =
    withBinaryFileDurableAtomic filePath WriteMode $ \h ->
        hPut h $ serialise x
    where
    filePath = getFile ident key filetype

-- TODO don't treat every exception as fnf
readState :: (NFData a, Serialise a) => Id
                                     -> Locked Key
                                     -> String
                                     -> IO (Maybe a)
readState ident key filetype =
    catchIO (withBinaryFile filePath ReadMode $ \h -> do
                content <- hGetContents h
                content `deepseq` (pure . Just . deserialise $ content))
            (\_ -> pure Nothing)
    where
    filePath = getFile ident key filetype

-- todo exceptions
deleteState :: Id
            -> Locked Key
            -> String
            -> IO ()
deleteState ident key filetype =
    removeFile filePath
    where
    filePath = getFile ident key filetype

getFile :: Id -> Locked Key -> String -> FilePath
getFile (Id ident) (Locked (Key (Topic topic) (SequenceNum seqNum))) filetype =
    printf "%s_%s_%d.%s" ident topic seqNum filetype

