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
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as V2
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import qualified System.Directory as Directory
import System.FS.API
import System.FS.API.Lazy
import System.FS.CRC
import System.FS.IO
import System.FilePath (splitFileName)
import System.IO (hFlush, stdout)
import System.IO.Temp

data Format
  = Legacy FilePath
  | Mem FilePath
  | LMDB FilePath
  | LSM FilePath FilePath
  deriving (Show, Read)

data Config = Config
  { from :: Format
  -- ^ Which format the input snapshot is in
  , to :: Format
  -- ^ Which format the output snapshot must be in
  }

getCommandLineConfig :: IO (Config, BlockType)
getCommandLineConfig =
  execParser $
    info
      ((,) <$> (Config <$> parseConfig In <*> parseConfig Out) <*> blockTypeParser <**> helper)
      (fullDesc <> progDesc "Utility for converting snapshots to and from UTxO-HD")

data InOut = In | Out

inoutForGroup :: InOut -> String
inoutForGroup In = "Input arguments:"
inoutForGroup Out = "Output arguments:"

inoutForHelp :: InOut -> String -> String
inoutForHelp In = ("Input " ++)
inoutForHelp Out = ("Output " ++)

inoutForCommand :: InOut -> String -> String
inoutForCommand In = (++ "-in")
inoutForCommand Out = (++ "-out")

parseConfig :: InOut -> Parser Format
parseConfig io =
  ( Legacy
      <$> parserOptionGroup
        (inoutForGroup io)
        (parsePath (inoutForCommand io "legacy") (inoutForHelp io "snapshot file"))
  )
    <|> ( Mem
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "mem") (inoutForHelp io "snapshot dir"))
        )
    <|> ( LMDB
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lmdb") (inoutForHelp io "snapshot dir"))
        )
    <|> ( LSM
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-snapshot") (inoutForHelp io "snapshot dir"))
            <*> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-database") (inoutForHelp io "LSM database"))
        )

parsePath :: String -> String -> Parser FilePath
parsePath optName strHelp =
  strOption
    ( mconcat
        [ long optName
        , help strHelp
        , metavar "PATH"
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

load ::
  forall blk.
  ( CanStowLedgerTables (LedgerState blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Config ->
  ResourceRegistry IO ->
  CodecConfig blk ->
  FilePath ->
  ExceptT (Error blk) IO (ExtLedgerState blk EmptyMK, LedgerTables (ExtLedgerState blk) ValuesMK)
load config rr ccfg tempFP =
  case from config of
    Legacy (pathToDiskSnapshot -> Just (fs@(SomeHasFS hasFS), path, _)) -> do
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
    Mem (pathToDiskSnapshot -> Just (fs, _, ds)) -> do
      (ls, _) <- withExceptT SnapshotError $ V2.loadSnapshot nullTracer rr ccfg fs ds
      let h = V2.currentHandle ls
      (V2.state h,) <$> Trans.lift (V2.readAll (V2.tables h) (V2.state h))
    LMDB (pathToDiskSnapshot -> Just (fs, _, ds)) -> do
      ((dbch, k, bstore), _) <-
        withExceptT SnapshotError $
          V1.loadSnapshot
            nullTracer
            (V1.LMDBBackingStoreArgs tempFP defaultLMDBLimits Dict.Dict)
            ccfg
            (V1.SnapshotsFS fs)
            rr
            ds
      values <- Trans.lift (V1.bsReadAll bstore (V1.changelogLastFlushedState dbch))
      _ <- Trans.lift $ RR.release k
      pure (V1.current dbch, values)
    LSM _ _ -> error "unimplemented"
    _ -> error "Malformed input path!"

store ::
  ( CanStowLedgerTables (LedgerState blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Config ->
  CodecConfig blk ->
  (ExtLedgerState blk EmptyMK, LedgerTables (ExtLedgerState blk) ValuesMK) ->
  SomeHasFS IO ->
  IO ()
store config ccfg (state, tbs) tempFS =
  case to config of
    Legacy (p@(pathToDiskSnapshot -> Just (fs@(SomeHasFS hasFS), path, _))) -> do
      crc <-
        writeExtLedgerState
          fs
          (encodeDiskExtLedgerState ccfg)
          path
          (stowLedgerTables $ state `withLedgerTables` tbs)
      withFile hasFS (path <.> "checksum") (WriteMode MustBeNew) $ \h ->
        Monad.void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc
      putStrLn "DONE"
      putStrLn $
        unlines $
          [ "You can now copy the file "
              ++ p
              ++ " to your `ledger` directory in your ChainDB storage."
          , "Note this snapshot can only be used by cardano-node <10.4."
          ]
    Mem (p@(pathToDiskSnapshot -> Just (fs, _, DiskSnapshot _ suffix))) -> do
      lseq <- V2.empty state tbs $ V2.newInMemoryLedgerTablesHandle nullTracer fs
      let h = V2.currentHandle lseq
      Monad.void $ InMemory.implTakeSnapshot ccfg nullTracer fs suffix h
      putStrLn "DONE"
      putStrLn $
        unlines $
          [ "You can now copy the directory "
              ++ p
              ++ " to your `ledger` directory in your ChainDB storage."
          , "Note this snapshot can only be used by cardano-node >=10.4 configured to use the InMemory backend (set the \"LedgerDB\".\"Backend\" key in your config file to \"V2InMemory\" or leave it undefined)."
          ]
    LMDB (p@(pathToDiskSnapshot -> Just (fs, _, DiskSnapshot _ suffix))) -> do
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
        V1.implTakeSnapshot chlog ccfg nullTracer (V1.SnapshotsFS fs) bs suffix
      putStrLn "DONE"
      putStrLn $
        unlines $
          [ "You can now copy the directory "
              ++ p
              ++ " to your `ledger` directory in your ChainDB storage."
          , "Note this snapshot can only be used by cardano-node >=10.4 configured to use the LMDB backend (set the \"LedgerDB\".\"Backend\" key in your config file to \"V1LMDB\")."
          ]
    LSM (p@(pathToDiskSnapshot -> Just (fs, _, DiskSnapshot _ suffix))) dbPath -> do
      exists <- Directory.doesDirectoryExist dbPath
      Monad.when (not exists) $ Directory.createDirectory dbPath
      RR.withRegistry $ \reg -> do
        (_, SomeHasFSAndBlockIO hasFS blockIO) <- LSM.stdMkBlockIOFS dbPath reg
        salt <- LSM.stdGenSalt
        LSM.withNewSession nullTracer hasFS blockIO salt (mkFsPath [""]) $ \session -> do
          lsmTable <- LSM.tableFromValuesMK reg session state tbs
          lsmHandle <- LSM.newLSMLedgerTablesHandle nullTracer reg lsmTable
          Monad.void $
            LSM.implTakeSnapshot
              ccfg
              nullTracer
              fs
              suffix
              (V2.StateRef state lsmHandle)
      putStrLn "DONE"
      putStrLn $
        unlines $
          [ "You can now:"
          , "- copy the directory "
              ++ p
              ++ " to your `ledger` directory in your ChainDB storage."
          , "- copy the directory "
              ++ dbPath
              ++ " to your fast storage device and point to it in your config file."
          , "Note this snapshot can only be used by cardano-node >=10.7 configured to use the LSM backend (set the \"LedgerDB\".\"Backend\" key in your config file to \"V2LSM\")."
          ]
    _ -> error "Malformed output path!"

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
        putStr "Loading snapshot..."
        hFlush stdout
        state <- either throwIO pure =<< runExceptT (load conf rr ccfg dir)
        putStrLn "DONE"
        putStr "Writing snapshot..."
        hFlush stdout
        store conf ccfg state tempFS
