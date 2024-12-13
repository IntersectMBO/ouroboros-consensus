{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise
import qualified Control.Monad as Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.Bits
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (ord)
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
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as Disk
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Bridge as LMDB.Bridge
import           Ouroboros.Consensus.Util.IOLike
import           System.FilePath (splitFileName)
import           System.FS.API
import           System.FS.API.Lazy
import           System.FS.CRC
import           System.FS.IO

data Format
    = Legacy
    | Mem
    | LMDB
    deriving (Show, Read)

data Config = Config
    { from       :: Format
    -- ^ Which format the input snapshot is in
    , inpath     :: FilePath
    -- ^ Path to the input snapshot
    , to         :: Format
    -- ^ Which format the output snapshot must be in
    , outpath    :: FilePath
    -- ^ Path to the output snapshot
    , doChecksum :: Flag "DoDiskSnapshotChecksum"
    -- ^ Write and check checksums
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
        <*> flag DoDiskSnapshotChecksum NoDoDiskSnapshotChecksum
            ( mconcat
                [ long "no-checksum"
                , help "Disable checking and writing checksums"
                ]
            )

-- Helpers

pathToFS :: FilePath -> (SomeHasFS IO, FsPath)
pathToFS path = (SomeHasFS $ ioHasFS $ MountPoint dir, mkFsPath [file])
  where
    (dir, file) = splitFileName path

defaultLMDBLimits :: LMDB.Limits
defaultLMDBLimits =
    LMDB.Limits
        { LMDB.mapSize = 16 * 1024 * 1024 * 1024
        , LMDB.maxDatabases = 10
        , LMDB.maxReaders = 16
        }

data Error
    = SnapshotError ReadSnapshotErr
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
        Monad.unless exists $ throwIO $ SnapshotFormatMismatch m err

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
    -> CodecConfig blk
    -> IO (ExtLedgerState blk ValuesMK)
load Config{from = Legacy, inpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg = do
    checkSnapshot Legacy path fs
    eSt <- fmap (first unstowLedgerTables)
        <$> runExceptT (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode doChecksum path)
    case eSt of
      Left err -> throwIO $ SnapshotError $ ReadSnapshotFailed $ err
      Right (st, mbChecksumAsRead) ->
        if getFlag doChecksum then do
          -- the checksum path is wrong here
          snapshotCRC <- runExceptT $ readCRC hasFS (path <.> "checksum")
          case snapshotCRC of
            Left err -> throwIO $ SnapshotError err
            Right storedCrc ->
              if mbChecksumAsRead /= Just storedCrc then
                throwIO $ SnapshotError ReadSnapshotDataCorruption
              else pure st
        else pure st
load Config{from = Mem, inpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg = do
    checkSnapshot Mem path fs
    eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode doChecksum (path </> mkFsPath ["state"])
    case eExtLedgerSt of
      Left err -> throwIO $ SnapshotError $ ReadSnapshotFailed err
      Right (extLedgerSt, mbChecksumAsRead) ->
       let cont = do
             values <- withFile hasFS (path </> mkFsPath ["tables", "tvar"]) ReadMode $ \h -> do
                 bs <- hGetAll hasFS h
                 case CBOR.deserialiseFromBytes valuesMKDecoder bs of
                     Left err -> throwIO $ TablesCantDeserializeError err
                     Right (extra, x) ->
                             if BSL.null extra
                                 then pure x
                                 else throwIO TablesTrailingBytes
             pure (extLedgerSt `withLedgerTables` values)
       in if getFlag doChecksum then do
          !snapshotCRC <- runExceptT $ readCRC hasFS (path </> mkFsPath ["checksum"])
          case snapshotCRC of
            Left err -> throwIO $ SnapshotError err
            Right storedCrc ->
              if mbChecksumAsRead /= Just storedCrc then
                throwIO $ SnapshotError ReadSnapshotDataCorruption
              else cont
       else cont
load Config{from = LMDB, inpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg = do
    checkSnapshot LMDB path fs
    eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode doChecksum (path </> mkFsPath ["state"])
    case eExtLedgerSt of
      Left err -> throwIO $ SnapshotError $ ReadSnapshotFailed err
      Right (extLedgerSt, mbChecksumAsRead) ->
        let cont = do
              values <- do
                  dbEnv <- LMDB.openEnvironment (fsToFilePath (MountPoint ".") (path </> mkFsPath ["tables"])) defaultLMDBLimits
                  Disk.LMDBMK _ dbBackingTables <- LMDB.readWriteTransaction dbEnv (Disk.getDb (K2 "utxo"))
                  catch (LMDB.readOnlyTransaction dbEnv $
                            LMDB.Cursor.runCursorAsTransaction'
                                LMDB.Cursor.cgetAll
                                dbBackingTables
                                (LMDB.Bridge.fromCodecMK $ getLedgerTables $ codecLedgerTables @(LedgerState blk))
                        )
                        (\(err :: DeserialiseFailure) -> throwIO $ TablesCantDeserializeError err)
              pure (extLedgerSt `withLedgerTables` LedgerTables (ValuesMK values))
        in if getFlag doChecksum then do
          !snapshotCRC <- runExceptT $ readCRC hasFS (path </> mkFsPath ["checksum"])
          case snapshotCRC of
            Left err -> throwIO $ SnapshotError  err
            Right storedCrc ->
              if mbChecksumAsRead /= Just storedCrc then
                throwIO $ SnapshotError $ ReadSnapshotDataCorruption
              else cont
       else cont

store ::
       ( LedgerDbSerialiseConstraints blk
       , CanStowLedgerTables (LedgerState blk)
       , HasLedgerTables (LedgerState blk)
       , IsLedger (LedgerState blk)
       )
    => Config
    -> CodecConfig blk
    -> ExtLedgerState blk ValuesMK
    -> IO ()
store Config{to = Legacy, outpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg state = do
    crc <- writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) path (stowLedgerTables state)
    Monad.when (getFlag doChecksum) $
      withFile hasFS (path <.> "checksum") (WriteMode MustBeNew) $ \h ->
        Monad.void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc

store Config{to = Mem, outpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg state = do
    -- write state
    createDirectoryIfMissing hasFS True path
    crc <- writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) (path </> mkFsPath ["state"]) (forgetLedgerTables state)
    Monad.when (getFlag doChecksum) $
      withFile hasFS (path </> mkFsPath ["checksum"]) (WriteMode MustBeNew) $ \h ->
        Monad.void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc
    -- write tables
    createDirectoryIfMissing hasFS True $ path </> mkFsPath ["tables"]
    withFile hasFS (path </> mkFsPath ["tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
        void $
            hPutAll hasFS hf $
                CBOR.toLazyByteString $
                    valuesMKEncoder (projectLedgerTables state)
store Config{to = LMDB, outpath = pathToFS -> (fs@(SomeHasFS hasFS), path), doChecksum} ccfg state = do
    -- write state
    createDirectoryIfMissing hasFS True path
    crc <- writeExtLedgerState fs (encodeDiskExtLedgerState ccfg) (path </> mkFsPath ["state"]) (forgetLedgerTables state)
    Monad.when (getFlag doChecksum) $
      withFile hasFS (path </> mkFsPath ["checksum"]) (WriteMode MustBeNew) $ \h ->
        Monad.void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc
    -- write tables
    createDirectoryIfMissing hasFS True $ path </> mkFsPath ["tables"]
    dbEnv <- LMDB.openEnvironment (fsToFilePath (MountPoint ".") $ path </> mkFsPath ["tables"]) defaultLMDBLimits
    dbState <- LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")
    dbBackingTables <-
        LMDB.readWriteTransaction dbEnv $
            lttraverse Disk.getDb (ltpure $ K2 "utxo")
    LMDB.readWriteTransaction dbEnv $
        Disk.withDbSeqNoRWMaybeNull dbState $ \case
            Nothing ->
                ltzipWith3A Disk.initLMDBTable dbBackingTables codecLedgerTables (projectLedgerTables state)
                    $> ((), Disk.DbSeqNo{Disk.dbsSeq = pointSlot $ getTip state})
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
        putStrLn "Loading snapshot..."
        state <- load conf ccfg
        putStrLn "Loaded snapshot"
        putStrLn "Writing snapshot..."
        store conf ccfg state
        putStrLn "Written snapshot"


readCRC ::
     MonadThrow m
  => HasFS m h
  -> FsPath
  -> ExceptT ReadSnapshotErr m CRC
readCRC hasFS crcPath = ExceptT $ do
  crcExists <- doesFileExist hasFS crcPath
  if not crcExists
    then pure (Left $ ReadSnapshotNoChecksumFile crcPath)
    else do
      withFile hasFS crcPath ReadMode $ \h -> do
        str' <- BSL.toStrict <$> hGetAll hasFS h
        if not (BSC.length str' == 8 && BSC.all isHexDigit str')
          then pure (Left $ ReadSnapshotInvalidChecksumFile crcPath)
          else pure . Right . CRC $ fromIntegral (hexdigitsToInt str')
          -- TODO: remove the functions in the where clause when we start depending on lsm-tree
  where
    isHexDigit :: Char -> Bool
    isHexDigit c = (c >= '0' && c <= '9')
                || (c >= 'a' && c <= 'f') --lower case only

    -- Precondition: BSC.all isHexDigit
    hexdigitsToInt :: BSC.ByteString -> Word
    hexdigitsToInt =
        BSC.foldl' accumdigit 0
      where
        accumdigit :: Word -> Char -> Word
        accumdigit !a !c =
          (a `shiftL` 4) .|. hexdigitToWord c


    -- Precondition: isHexDigit
    hexdigitToWord :: Char -> Word
    hexdigitToWord c
      | let !dec = fromIntegral (ord c - ord '0')
      , dec <= 9  = dec
      | let !hex = fromIntegral (ord c - ord 'a' + 10)
      , otherwise = hex
