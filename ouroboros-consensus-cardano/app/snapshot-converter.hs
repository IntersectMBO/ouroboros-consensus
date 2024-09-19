{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Cursor as LMDB.Cursor
import           DBAnalyser.Parsers
import           Main.Utf8
import           Options.Applicative
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as Disk
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Bridge as LMDB.Bridge
import           Ouroboros.Consensus.Util.CBOR
import           Ouroboros.Consensus.Util.IOLike
import           System.FilePath (isRelative)
import           System.FS.API
import           System.FS.API.Lazy
import           System.FS.IO

data Format
    = Legacy
    | Mem
    | LMDB
    deriving (Show, Read)

data Config = Config
    { from    :: Format
    -- ^ Which format the input snapshot is in
    , inpath  :: FsPath
    -- ^ Path to the input snapshot
    , to      :: Format
    -- ^ Which format the output snapshot must be in
    , outpath :: FsPath
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
        <*> argument
               (eitherReader (\x -> if isRelative x then Right (mkFsPath [x]) else Left $ "Non-relative path in input path argument: " <> show x))
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
        <*> argument
             (eitherReader (\x -> if isRelative x then Right (mkFsPath [x]) else Left $ "Non-relative path in output path argument: " <> show x))
                    ( mconcat
                        [ help "Output dir/file Use relative paths like ./100007913"
                        , metavar "PATH-OUT"
                        ]
                    )


-- Helpers

defaultLMDBLimits :: LMDB.Limits
defaultLMDBLimits =
    LMDB.Limits
        { LMDB.mapSize = 16 * 1024 * 1024 * 1024
        , LMDB.maxDatabases = 10
        , LMDB.maxReaders = 16
        }

data Error
    = SnapshotError ReadIncrementalErr
    | TablesCantDeserializeError DeserialiseFailure
    | TablesTrailingBytes
    | SnapshotFormatMismatch Format String
    deriving Exception

instance Show Error where
    show (SnapshotError err) = "Couldn't deserialize the snapshot. Are you running the same node version that created the snapshot? " <> show err
    show (TablesCantDeserializeError err) = "Couldn't deserialize the tables: " <> show err
    show TablesTrailingBytes = "Malformed tables, there are trailing bytes!"
    show (SnapshotFormatMismatch expected err) = "The input snapshot does not seem to correspond to the input format:\n\t" <> show expected <> "\n\tThe provided path " <> err

checkSnapshot :: Format -> FsPath -> SomeHasFS IO -> IO ()
checkSnapshot m p (SomeHasFS fs) = case m of
    Legacy ->
        want (doesFileExist fs) p "is NOT a file"
    Mem -> newFormatCheck "tvar"
    LMDB -> newFormatCheck "data.mdb"
  where
    want :: (FsPath -> IO Bool) -> FsPath -> String -> IO ()
    want fileType path err = do
        exists <- fileType path
        unless exists $ throwIO $ SnapshotFormatMismatch m err

    isDir        = (doesDirectoryExist, [],             "is NOT a directory")
    hasTablesDir = (doesDirectoryExist, ["tables"],     "DOES NOT contain a \"tables\" directory")
    hasState     = (doesFileExist,      ["state"],      "DOES NOT contain a \"state\" file")
    hasTables tb = (doesFileExist,      ["tables", tb], "DOES NOT contain a \"tables/" <> tb <> "\" file")

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
       , HasLedgerTables (LedgerState blk)
       )
    => Config
    -> SomeHasFS IO
    -> CodecConfig blk
    -> IO (ExtLedgerState blk ValuesMK)
load Config{from = Legacy, inpath} fs ccfg = do
    checkSnapshot Legacy inpath fs
    eSt <- fmap unstowLedgerTables
        <$> runExceptT (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode inpath)
    case eSt of
      Left err -> throwIO $ SnapshotError err
      Right st -> pure st
load Config{from = Mem, inpath} fs@(SomeHasFS hasFS) ccfg = do
    checkSnapshot Mem inpath fs
    eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (inpath </> mkFsPath ["state"])
    case eExtLedgerSt of
      Left err -> throwIO $ SnapshotError err
      Right extLedgerSt -> do
        values <- withFile hasFS (inpath </> mkFsPath ["tables", "tvar"]) ReadMode $ \h -> do
            bs <- hGetAll hasFS h
            case CBOR.deserialiseFromBytes valuesMKDecoder bs of
                Left err -> throwIO $ TablesCantDeserializeError err
                Right (extra, x) ->
                        if BSL.null extra
                            then pure x
                            else throwIO TablesTrailingBytes
        pure (extLedgerSt `withLedgerTables` values)
load Config{from = LMDB, inpath} fs ccfg = do
    checkSnapshot LMDB inpath fs
    eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (inpath </> mkFsPath ["state"])
    case eExtLedgerSt of
      Left err -> throwIO $ SnapshotError err
      Right extLedgerSt -> do
        values <- do
            dbEnv <- LMDB.openEnvironment (fsToFilePath (MountPoint ".") (inpath </> mkFsPath ["tables"])) defaultLMDBLimits
            Disk.LMDBMK _ dbBackingTables <- LMDB.readWriteTransaction dbEnv (Disk.getDb (K2 "utxo"))
            catch (LMDB.readOnlyTransaction dbEnv $
                      LMDB.Cursor.runCursorAsTransaction'
                          LMDB.Cursor.cgetAll
                          dbBackingTables
                          (LMDB.Bridge.fromCodecMK $ getLedgerTables $ codecLedgerTables @(LedgerState blk))
                  )
                  (\(err :: DeserialiseFailure) -> throwIO $ TablesCantDeserializeError err)
        pure (extLedgerSt `withLedgerTables` LedgerTables (ValuesMK values))

store ::
       ( LedgerDbSerialiseConstraints blk
       , CanStowLedgerTables (LedgerState blk)
       , HasLedgerTables (LedgerState blk)
       , IsLedger (LedgerState blk)
       )
    => Config
    -> SomeHasFS IO
    -> CodecConfig blk
    -> ExtLedgerState blk ValuesMK
    -> IO ()
store Config{to = Legacy, outpath} fs ccfg state =
    writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) outpath (stowLedgerTables state)
store Config{to = Mem, outpath} fs@(SomeHasFS hasFS) ccfg state = do
    -- write state
    createDirectoryIfMissing hasFS True outpath
    writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) (outpath </> mkFsPath ["state"]) (forgetLedgerTables state)
    -- write tables
    createDirectoryIfMissing hasFS True $ outpath </> mkFsPath ["tables"]
    withFile hasFS (outpath </> mkFsPath ["tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
        void $
            hPutAll hasFS hf $
                CBOR.toLazyByteString $
                    valuesMKEncoder (projectLedgerTables state)
store Config{to = LMDB, outpath} fs@(SomeHasFS hasFS) ccfg state = do
    -- write state
    createDirectoryIfMissing hasFS True outpath
    writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) (outpath </> mkFsPath ["state"]) (forgetLedgerTables state)
    -- write tables
    createDirectoryIfMissing hasFS True $ outpath </> mkFsPath ["tables"]
    dbEnv <- LMDB.openEnvironment (fsToFilePath (MountPoint ".") $ outpath </> mkFsPath ["tables"]) defaultLMDBLimits
    dbState <- LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")
    dbBackingTables <-
        LMDB.readWriteTransaction dbEnv $
            lttraverse Disk.getDb (ltpure $ K2 "utxo")
    LMDB.readWriteTransaction dbEnv $
        Disk.withDbStateRWMaybeNull dbState $ \case
            Nothing ->
                ltzipWith3A Disk.initLMDBTable dbBackingTables codecLedgerTables (projectLedgerTables state)
                    $> ((), Disk.DbState{Disk.dbsSeq = pointSlot $ getTip state})
            Just _ -> liftIO $ throwIO $ Disk.LMDBErrInitialisingAlreadyHasState

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    (conf, blocktype) <- getCommandLineConfig
    case blocktype of
        ByronBlock args   -> run conf args
        ShelleyBlock args -> run conf args
        CardanoBlock args -> run conf args
  where
    run conf args = do
        ccfg <- configCodec . pInfoConfig <$> mkProtocolInfo args
        let fs = SomeHasFS $ ioHasFS $ MountPoint "."
        putStrLn "Loading snapshot..."
        state <- load conf fs ccfg
        putStrLn "Loaded snapshot"
        putStrLn "Writing snapshot..."
        store conf fs ccfg state
        putStrLn "Written snapshot"
