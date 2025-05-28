{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Codec.Serialise
import qualified Control.Monad as Monad
import Control.Monad.Except
import qualified Control.Monad.Trans as Trans (lift)
import Control.ResourceRegistry (ResourceRegistry)
import qualified Control.ResourceRegistry as RR
import Control.Tracer (nullTracer)
import DBAnalyser.Parsers
import Data.Bifunctor
import qualified Data.ByteString.Builder as BS
import qualified Data.SOP.Dict as Dict
import Main.Utf8
import Options.Applicative
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Lock as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as V2
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.API.Lazy
import System.FS.CRC
import System.FS.IO
import System.FilePath (splitFileName)
import System.IO.Temp

data Format
  = Legacy
  | Mem
  | LMDB
  deriving (Show, Read)

data Config = Config
  { from :: Format
  -- ^ Which format the input snapshot is in
  , inpath :: FilePath
  -- ^ Path to the input snapshot
  , to :: Format
  -- ^ Which format the output snapshot must be in
  , outpath :: FilePath
  -- ^ Path to the output snapshot
  }

getCommandLineConfig :: IO (Config, BlockType)
getCommandLineConfig =
  execParser $
    info
      ((,) <$> parseConfig <*> blockTypeParser <**> helper)
      (fullDesc <> progDesc "Utility for converting snapshots to and from UTxO-HD")

parseConfig :: Parser Config
parseConfig =
  Config
    <$> argument
      auto
      ( mconcat
          [ help "From format (Legacy, Mem or LMDB)"
          , metavar "FORMAT-IN"
          ]
      )
    <*> strArgument
      ( mconcat
          [ help "Input dir/file. Use relative paths like ./100007913"
          , metavar "PATH-IN"
          ]
      )
    <*> argument
      auto
      ( mconcat
          [ help "To format (Legacy, Mem or LMDB)"
          , metavar "FORMAT-OUT"
          ]
      )
    <*> strArgument
      ( mconcat
          [ help "Output dir/file Use relative paths like ./100007913"
          , metavar "PATH-OUT"
          ]
      )

-- Helpers

pathToDiskSnapshot :: FilePath -> Maybe (SomeHasFS IO, FsPath, DiskSnapshot)
pathToDiskSnapshot path = (SomeHasFS $ ioHasFS $ MountPoint dir,mkFsPath [file],) <$> snapshotFromPath file
 where
  (dir, file) = splitFileName path

defaultLMDBLimits :: V1.LMDBLimits
defaultLMDBLimits =
  V1.LMDBLimits
    { V1.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , V1.lmdbMaxDatabases = 10
    , V1.lmdbMaxReaders = 16
    }

data Error blk
  = SnapshotError (SnapshotFailure blk)
  | TablesCantDeserializeError DeserialiseFailure
  | TablesTrailingBytes
  | SnapshotFormatMismatch Format String
  | ReadSnapshotCRCError FsPath CRCError
  deriving Exception

instance StandardHash blk => Show (Error blk) where
  show (SnapshotError err) =
    "Couldn't deserialize the snapshot. Are you running the same node version that created the snapshot? "
      <> show err
  show (TablesCantDeserializeError err) = "Couldn't deserialize the tables: " <> show err
  show TablesTrailingBytes = "Malformed tables, there are trailing bytes!"
  show (SnapshotFormatMismatch expected err) =
    "The input snapshot does not seem to correspond to the input format:\n\t"
      <> show expected
      <> "\n\tThe provided path "
      <> err
  show (ReadSnapshotCRCError fp err) = "An error occurred while reading the snapshot checksum at " <> show fp <> ": \n\t" <> show err

checkSnapshotFileStructure :: Format -> FsPath -> SomeHasFS IO -> ExceptT (Error blk) IO ()
checkSnapshotFileStructure m p (SomeHasFS fs) = case m of
  Legacy -> want (doesFileExist fs) p "is NOT a file"
  Mem -> newFormatCheck "tvar"
  LMDB -> newFormatCheck "data.mdb"
 where
  want :: (FsPath -> IO Bool) -> FsPath -> String -> ExceptT (Error blk) IO ()
  want fileType path err = do
    exists <- Trans.lift $ fileType path
    Monad.unless exists $ throwError $ SnapshotFormatMismatch m err

  isDir = (doesDirectoryExist, [], "is NOT a directory")
  hasTablesDir = (doesDirectoryExist, ["tables"], "DOES NOT contain a \"tables\" directory")
  hasState = (doesFileExist, ["state"], "DOES NOT contain a \"state\" file")
  hasTables tb = (doesFileExist, ["tables", tb], "DOES NOT contain a \"tables/" <> tb <> "\" file")

  newFormatCheck tb =
    mapM_
      (\(doCheck, extra, err) -> want (doCheck fs) (p </> mkFsPath extra) err)
      [ isDir
      , hasTablesDir
      , hasState
      , hasTables tb
      ]

load ::
  forall blk.
  ( LedgerDbSerialiseConstraints blk
  , CanStowLedgerTables (LedgerState blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Config ->
  ResourceRegistry IO ->
  CodecConfig blk ->
  FilePath ->
  ExceptT (Error blk) IO (ExtLedgerState blk EmptyMK, LedgerTables (ExtLedgerState blk) ValuesMK)
load config@Config{inpath = pathToDiskSnapshot -> Just (fs@(SomeHasFS hasFS), path, ds)} rr ccfg tempFP =
  case from config of
    Legacy -> do
      checkSnapshotFileStructure Legacy path fs
      (st, checksumAsRead) <-
        first unstowLedgerTables
          <$> withExceptT
            (SnapshotError . InitFailureRead . ReadSnapshotFailed)
            (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode path)
      let crcPath = path <.> "checksum"
      crcFileExists <- Trans.lift $ doesFileExist hasFS crcPath
      Monad.when crcFileExists $ do
        snapshotCRC <-
          withExceptT (ReadSnapshotCRCError crcPath) $
            readCRC hasFS crcPath
        Monad.when (checksumAsRead /= snapshotCRC) $
          throwError $
            SnapshotError $
              InitFailureRead ReadSnapshotDataCorruption
      pure (forgetLedgerTables st, projectLedgerTables st)
    Mem -> do
      checkSnapshotFileStructure Mem path fs
      (ls, _) <- withExceptT SnapshotError $ V2.loadSnapshot nullTracer rr ccfg fs ds
      let h = V2.currentHandle ls
      (V2.state h,) <$> Trans.lift (V2.readAll (V2.tables h))
    LMDB -> do
      checkSnapshotFileStructure LMDB path fs
      ((dbch, bstore), _) <-
        withExceptT SnapshotError $
          V1.loadSnapshot
            nullTracer
            (V1.LMDBBackingStoreArgs tempFP defaultLMDBLimits Dict.Dict)
            ccfg
            (V1.SnapshotsFS fs)
            ds
      (V1.current dbch,) <$> Trans.lift (V1.bsReadAll bstore (V1.changelogLastFlushedState dbch))
load _ _ _ _ = error "Malformed input path!"

store ::
  ( LedgerDbSerialiseConstraints blk
  , CanStowLedgerTables (LedgerState blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Config ->
  CodecConfig blk ->
  (ExtLedgerState blk EmptyMK, LedgerTables (ExtLedgerState blk) ValuesMK) ->
  SomeHasFS IO ->
  IO ()
store config@Config{outpath = pathToDiskSnapshot -> Just (fs@(SomeHasFS hasFS), path, DiskSnapshot _ suffix)} ccfg (state, tbs) tempFS =
  case to config of
    Legacy -> do
      crc <-
        writeExtLedgerState
          fs
          (encodeDiskExtLedgerState ccfg)
          path
          (stowLedgerTables $ state `withLedgerTables` tbs)
      withFile hasFS (path <.> "checksum") (WriteMode MustBeNew) $ \h ->
        Monad.void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc
    Mem -> do
      lseq <- V2.empty state tbs $ V2.newInMemoryLedgerTablesHandle nullTracer fs
      let h = V2.currentHandle lseq
      Monad.void $ V2.takeSnapshot ccfg nullTracer fs suffix h
    LMDB -> do
      chlog <- newTVarIO (V1.empty state)
      lock <- V1.mkLedgerDBLock
      bs <-
        V1.newLMDBBackingStore
          nullTracer
          defaultLMDBLimits
          (V1.LiveLMDBFS tempFS)
          (V1.SnapshotsFS fs)
          (V1.InitFromValues (pointSlot $ getTip state) state tbs)
      Monad.void $ V1.withReadLock lock $ do
        V1.takeSnapshot chlog ccfg nullTracer (V1.SnapshotsFS fs) bs suffix
store _ _ _ _ = error "Malformed output path!"

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (conf, blocktype) <- getCommandLineConfig
  case blocktype of
    ByronBlock args -> run conf args
    ShelleyBlock args -> run conf args
    CardanoBlock args -> run conf args
 where
  run conf args = do
    ccfg <- configCodec . pInfoConfig <$> mkProtocolInfo args
    withSystemTempDirectory "lmdb" $ \dir -> do
      let tempFS = SomeHasFS $ ioHasFS $ MountPoint dir
      RR.withRegistry $ \rr -> do
        putStrLn "Loading snapshot..."
        state <- either throwIO pure =<< runExceptT (load conf rr ccfg dir)
        putStrLn "Loaded snapshot"
        putStrLn "Writing snapshot..."
        store conf ccfg state tempFS
        putStrLn "Written snapshot"
