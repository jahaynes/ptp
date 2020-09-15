{-# LANGUAGE BangPatterns,
             LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Journal ( Journal (..)
               , create
               ) where

import Entity.Id
import Entity.SequenceNum
import Entity.Topic
import Entity.Value
import Server.Locks

import           Codec.Serialise             (deserialise, serialise)
import           Control.Monad               (forM, forM_, when, unless)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe                  (catMaybes)
import           Data.Word                   (Word32, Word64)
import           RIO.File                    (withBinaryFileDurable)
import           System.Directory            (createDirectoryIfMissing, doesFileExist)
import           System.IO                   (Handle, SeekMode (..), IOMode(..), hSeek, hFileSize, hSetFileSize, withBinaryFile)
import           Text.Printf                 (printf)

-- TODO it may be worth trying to move the serial/deserial code outside of the locks

-- TODO try-catch on everything here

data Journal =
    Journal { readEntries    :: !(Locked Topic -> [SequenceNum] -> IO [(SequenceNum, Val)])
            , writeEntries   :: !(Locked Topic -> [(SequenceNum, Val)] -> IO ())
            , dumpJournal    :: !(Locked Topic -> ((SequenceNum, Val) -> IO ()) -> IO ())
            }

data Index = -- Can't use Serial here - need fixed width for O(1) access
    Index { _start :: !Word64
          , _end   :: !Word32
          }

create :: Id -> IO Journal
create ident =
    pure $ Journal { readEntries    = readEntriesImpl    ident
                   , writeEntries   = writeEntriesImpl   ident
                   , dumpJournal    = dumpJournalImpl ident
                   }

readEntriesImpl :: Id
                -> Locked Topic
                -> [SequenceNum]
                -> IO [(SequenceNum, Val)]
readEntriesImpl ident topic seqNums = do

    let idxPath = getIndexFile ident topic
    let jrnPath = getJournalFile ident topic

    doesFileExist idxPath >>= \case

        False -> pure []

        True ->
            catMaybes <$> forM seqNums (\seqNum ->
                withBinaryFile idxPath ReadMode (`readIndex` seqNum) >>= \case
                    Nothing -> pure Nothing
                    Just index ->
                        withBinaryFile jrnPath ReadMode $ \h -> do
                            val <- readEntry h index
                            pure $ Just (seqNum, val))

    where
    readEntry :: Handle -> Index -> IO Val
    readEntry h (Index pos sz) = do
        hSeek h AbsoluteSeek (fromIntegral pos)
        !v <- deserialise <$> LBS.hGet h (fromIntegral sz)
        pure v

    readIndex :: Handle -> SequenceNum -> IO (Maybe Index)
    readIndex h (SequenceNum n) = do
        idxSzBytes <- hFileSize h
        let (idxSz, 0) = idxSzBytes `divMod` 12
        let n' = fromIntegral n
        if n' >= idxSz
            then pure Nothing
            else do
                hSeek h AbsoluteSeek (12 * n')
                !pos <- runGet getWord64le <$> LBS.hGet h 8
                !sz  <- runGet getWord32le <$> LBS.hGet h 4
                case pos of
                    0 -> pure Nothing
                    _ -> pure . Just $ Index pos sz

    -- To check - make sure to reset handles between invocations?
writeEntriesImpl :: Id
                 -> Locked Topic
                 -> [(SequenceNum, Val)]
                 -> IO ()
writeEntriesImpl ident topic seqVals = do
    createDirectoryIfMissing True (getJournalDir ident topic)
    withBinaryFileDurable (getJournalFile ident topic) AppendMode $ \jrnH ->
        withBinaryFileDurable (getIndexFile ident topic) ReadWriteMode $ \idxH ->
            forM_ seqVals $ \(seqNum, val) -> writeEntryImpl jrnH idxH seqNum val

    where
    writeEntryImpl :: Handle
                   -> Handle
                   -> SequenceNum
                   -> Val
                   -> IO ()
    writeEntryImpl jrnH idxH (SequenceNum n) v = do

        let sv = serialise v
        let valueSz = LBS.length sv

        -- Append the value
        valuePos <- do
            !preSize <- hFileSize jrnH
            if preSize == 0
                then do
                    LBS.hPut jrnH "\0"
                    LBS.hPut jrnH sv
                    pure (preSize + 1)
                else do
                    LBS.hPut jrnH sv
                    pure preSize

        -- write index

        -- Resize if necessary
        !idxSz <- hFileSize idxH
        let n' = 12 * fromIntegral n
        when (n' >= idxSz) $ hSetFileSize idxH (2 * n' + 12)

        -- Write the index entry
        hSeek idxH AbsoluteSeek n'
        LBS.hPut idxH . runPut $ do putWord64le (fromIntegral valuePos)
                                    putWord32le (fromIntegral valueSz)

dumpJournalImpl :: Id
                -> Locked Topic
                -> ((SequenceNum, Val) -> IO ())
                -> IO ()
dumpJournalImpl ident topic f = do

    let journalPath = getJournalFile ident topic

    let idxPath = getIndexFile ident topic

    exist <- (&&) <$> doesFileExist journalPath
                  <*> doesFileExist idxPath

    when exist $
        withBinaryFile journalPath ReadMode $ \jrnH ->
            withBinaryFile idxPath ReadMode $ \idxH -> do
                idxSz <- fromIntegral . (`div` 12) <$> hFileSize idxH
                forM_ [0..idxSz-1] $ \sn -> do
                    pos <- fromIntegral . runGet getWord64le <$> LBS.hGet idxH 8
                    sz  <- fromIntegral . runGet getWord32le <$> LBS.hGet idxH 4
                    unless (pos * sz == 0) $ do
                        hSeek jrnH AbsoluteSeek pos
                        v <- deserialise <$> LBS.hGet jrnH (fromIntegral sz)
                        f (SequenceNum sn, v)

getIndexFile :: Id -> Locked Topic -> FilePath
getIndexFile ident topic =
    printf "%s/index" (getJournalDir ident topic)

getJournalFile :: Id -> Locked Topic -> FilePath
getJournalFile ident topic =
    printf "%s/journal" (getJournalDir ident topic)

getJournalDir :: Id -> Locked Topic -> FilePath
getJournalDir (Id ident) (Locked (Topic t)) =
    printf "nodes/%s/topics/%s" ident t
