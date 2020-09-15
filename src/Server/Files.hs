{-# LANGUAGE BangPatterns,
             LambdaCase #-}

module Server.Files where

import Entity.Id
import Entity.SequenceNum
import Entity.Topic
import Server.Locks

import Codec.Serialise        (Serialise, readFileDeserialise, writeFileSerialise)
import Control.Exception.Safe (catchIO)
import System.Directory       (createDirectoryIfMissing, doesFileExist, removeFile)
import Text.Printf            (printf)

-- write meta
-- read meta
writeTopic :: Serialise a => Id
                          -> Locked Topic
                          -> String
                          -> a
                          -> IO ()
writeTopic ident topic filetype x = do
    createDirectoryIfMissing True (getTopicDir ident topic)
    writeFileSerialise filePath x
    where
    filePath = getTopicFile ident topic filetype

readTopic :: Serialise a => Id
                         -> Locked Topic
                         -> String
                         -> IO (Maybe a)
readTopic ident topic filetype =
    catchIO readTopicImpl
            (\ex -> do writeFile ("fatal_" <> show ident) (show ex)
                       error "FATAL: " $ show ex)

    where
    -- TODO don't treat every exception as fnf
    readTopicImpl :: Serialise a => IO (Maybe a)
    readTopicImpl =
        doesFileExist filePath >>= \case
            False -> pure Nothing
            True  -> do !x <- readFileDeserialise filePath
                        pure $ Just x
        where
        filePath = getTopicFile ident topic filetype

-- todo exceptions
writeState :: Serialise a => Id
                          -> Locked Topic
                          -> SequenceNum
                          -> String
                          -> a
                          -> IO ()
writeState ident topic seqNum filetype x = do
    createDirectoryIfMissing True (getTopicDir ident topic)
    writeFileSerialise filePath x
    where
    filePath = getTopicSeqNoFile ident topic seqNum filetype

readState :: Serialise a => Id
                         -> Locked Topic
                         -> SequenceNum
                         -> String
                         -> IO (Maybe a)
readState ident topic seqNum filetype =
    catchIO readStateImpl
            (\ex -> do writeFile ("fatal_" <> show ident) (show ex)
                       error "FATAL: " $ show ex)

    where
    -- TODO don't treat every exception as fnf
    readStateImpl :: Serialise a => IO (Maybe a)
    readStateImpl =
        doesFileExist filePath >>= \case
            False -> pure Nothing
            True  -> do !x <- readFileDeserialise filePath
                        pure $ Just x
        where
        filePath = getTopicSeqNoFile ident topic seqNum filetype

-- todo exceptions
deleteState :: Id
            -> Locked Topic
            -> SequenceNum
            -> String
            -> IO Bool
deleteState ident topic seqNum filetype =
    catchIO (removeFile filePath >> pure True)
            (\_ -> pure False)
    where
    filePath = getTopicSeqNoFile ident topic seqNum filetype

getTopicSeqNoFile :: Id -> Locked Topic -> SequenceNum -> String -> FilePath
getTopicSeqNoFile ident topic (SequenceNum seqNum) =
    printf "%s/%d.%s" (getTopicDir ident topic) seqNum

getTopicFile :: Id -> Locked Topic -> String -> FilePath
getTopicFile ident topic =
    printf "%s/%s" (getTopicDir ident topic)

getTopicDir :: Id -> Locked Topic -> FilePath
getTopicDir (Id ident) (Locked (Topic t)) =
    printf "nodes/%s/topics/%s" ident t
