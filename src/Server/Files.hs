{-# LANGUAGE LambdaCase #-}

module Server.Files ( readState
                    , readTopic
                    , writeState
                    , writeTopic
                    ) where

import Entity.Id
import Entity.SequenceNum
import Entity.Topic
import Server.Locks

import Codec.Serialise        (Serialise, readFileDeserialise, writeFileSerialise)
import Control.DeepSeq        (NFData, deepseq)
import Control.Exception.Safe (catchIO)
import Data.Functor           ((<&>))
import System.Directory       (createDirectoryIfMissing, doesFileExist)
import Text.Printf            (printf)

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

readTopic :: (NFData a, Serialise a) => Id
                                     -> Locked Topic
                                     -> String
                                     -> IO (Maybe a)
readTopic ident topic filetype =
    catchIO readTopicImpl
            (\ex -> do writeFile ("fatal_" <> show ident) (show ex)
                       error "FATAL: " $ show ex)

    where
    -- TODO don't treat every exception as fnf
    readTopicImpl :: (NFData a, Serialise a) => IO (Maybe a)
    readTopicImpl =
        doesFileExist filePath >>= \case
            False -> pure Nothing
            True  -> readFileDeserialise filePath <&> \x ->
                        x `deepseq` Just x
        where
        filePath = getTopicFile ident topic filetype

-- todo exceptions
writeState :: (NFData a, Serialise a) => Id
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

readState :: (NFData a, Serialise a) => Id
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
    readStateImpl :: (NFData a, Serialise a) => IO (Maybe a)
    readStateImpl =
        doesFileExist filePath >>= \case
            False -> pure Nothing
            True  -> readFileDeserialise filePath <&> \x ->
                        x `deepseq` Just x
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
