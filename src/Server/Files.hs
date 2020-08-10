{-# LANGUAGE LambdaCase #-}

module Server.Files where


import Entity.Id
import Entity.Key
import Entity.SequenceNum
import Entity.Topic
import Server.Keylocks

import Codec.Serialise        (Serialise, readFileDeserialise, writeFileSerialise)
import Control.Exception.Safe (catchIO)
import System.Directory       (doesFileExist, removeFile)
import Text.Printf            (printf)

-- todo exceptions
writeState :: Serialise a => Id
                          -> Locked Key
                          -> String
                          -> a
                          -> IO ()
writeState ident key filetype =
    writeFileSerialise filePath
    where
    filePath = getFile ident key filetype

readState :: Serialise a => Id
                         -> Locked Key
                         -> String
                         -> IO (Maybe a)
readState ident key filetype =
    catchIO readStateImpl
            (\ex -> do writeFile ("fatal_" <> show ident) (show ex)
                       error "FATAL")

    where
    -- TODO don't treat every exception as fnf
    readStateImpl :: Serialise a => IO (Maybe a)
    readStateImpl =
        doesFileExist filePath >>= \case
            False -> pure Nothing
            True  -> Just <$> readFileDeserialise filePath
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

