{-# LANGUAGE FlexibleInstances, 
             InstanceSigs,
             LambdaCase,
             MultiParamTypeClasses #-}

module Server.LocalFileStorage (LocalFileStorage, create) where

import Entity.Id
import Entity.SequenceNum
import Entity.Topic
import Server.Locks
import Server.Storage

import Codec.Serialise            (Serialise, readFileDeserialise, writeFileSerialise)
import Control.DeepSeq            (NFData, deepseq)
import Control.Exception          (SomeException)
import Control.Exception.Safe     (catchAnyDeep)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Functor               ((<&>))
import System.Directory           (createDirectoryIfMissing, doesFileExist)
import Text.Printf                (printf)

data LocalFileStorage a =
    LocalFileStorage { _myId    :: !Id
                     , _fileExt :: !String
                     }

instance (Serialise a, NFData a) => Storage (LocalFileStorage a) a where

    readTopicSequence :: LocalFileStorage a
                      -> Locked (Topic, SequenceNum)
                      -> ExceptT SomeException IO (Maybe a)
    readTopicSequence (LocalFileStorage ident fileExt) (Locked (topic, seqNum)) =
        catchAnyDeep (liftIO action) handler
        where
        action = do
            let path = getTopicSeqNoFile ident topic seqNum fileExt
            doesFileExist path >>= \case
                False -> pure Nothing
                True  -> readFileDeserialise path <&> \x -> x `deepseq` Just x
        handler ex = do
            liftIO $ printf "failed to readTopicSequenceImpl: %s %s %s: %s\nYou may want to manually delete this file\n" (show topic) (show seqNum) fileExt (show ex)
            throwE ex

    writeTopicSequence :: LocalFileStorage a
                       -> Locked (Topic, SequenceNum)
                       -> a
                       -> ExceptT SomeException IO ()
    writeTopicSequence (LocalFileStorage ident fileExt) (Locked (topic, seqNum)) x =
        catchAnyDeep (liftIO action) handler
        where
        action = do
            createDirectoryIfMissing True (getTopicDir ident topic)
            -- TODO: write to temp directory and then move into place?
            writeFileSerialise (getTopicSeqNoFile ident topic seqNum fileExt) x
        handler ex = do
            liftIO $ printf "failed to writeTopicSequenceImpl: %s %s %s: %s\n" (show topic) (show seqNum) fileExt (show ex)
            throwE ex

create :: Id -> String -> IO (LocalFileStorage a)
create myId ext = pure $ LocalFileStorage myId ext

getTopicSeqNoFile :: Id -> Topic -> SequenceNum -> String -> FilePath
getTopicSeqNoFile ident topic (SequenceNum seqNum) =
    printf "%s/%d.%s" (getTopicDir ident topic) seqNum

getTopicDir :: Id -> Topic -> FilePath
getTopicDir (Id ident) (Topic t) =
    printf "nodes/%s/topics/%s" ident t
