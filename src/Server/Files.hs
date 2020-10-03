{-# LANGUAGE LambdaCase #-}

-- TODO does this need RIO ?

module Server.Files ( Machine (..)
                    , readSubState
                    , readTopicDetail
                    , writeSubState
                    , writeTopicDetail
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

-- Declare elsewhere
data Machine = Machine deriving (Eq, Ord)


writeTopicDetail :: Serialise a => Id
                                -> Locked (Topic, b)
                                -> String
                                -> a
                                -> IO ()
writeTopicDetail ident (Locked (topic, _)) filetype x = do
    createDirectoryIfMissing True (getTopicDir ident topic)
    writeFileSerialise filePath x
    where
    filePath = getTopicFile ident topic filetype

readTopicDetail :: (NFData a, Serialise a) => Id
                                           -> Locked (Topic, b)
                                           -> String
                                           -> IO (Maybe a)
readTopicDetail ident (Locked (topic, _)) filetype =
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
writeSubState :: (NFData a, Serialise a) => Id
                                         -> Locked (Topic, b)
                                         -> SequenceNum
                                         -> String
                                         -> a
                                         -> IO ()
writeSubState ident (Locked (topic, _)) seqNum filetype x = do
    createDirectoryIfMissing True (getTopicDir ident topic)
    writeFileSerialise filePath x
    where
    filePath = getTopicSeqNoFile ident topic seqNum filetype

readSubState :: (NFData a, Serialise a) => Id
                                        -> Locked (Topic, b)
                                        -> SequenceNum
                                        -> String
                                        -> IO (Maybe a)
readSubState ident (Locked (topic, _)) seqNum filetype =
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

getTopicSeqNoFile :: Id -> Topic -> SequenceNum -> String -> FilePath
getTopicSeqNoFile ident topic (SequenceNum seqNum) =
    printf "%s/%d.%s" (getTopicDir ident topic) seqNum

getTopicFile :: Id -> Topic -> String -> FilePath
getTopicFile ident topic =
    printf "%s/%s" (getTopicDir ident topic)

getTopicDir :: Id -> Topic -> FilePath
getTopicDir (Id ident) (Topic t) =
    printf "nodes/%s/topics/%s" ident t
