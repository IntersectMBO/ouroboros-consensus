{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Codec.Serialise
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.ResourceRegistry
import DBAnalyser.Parsers
import Data.Bifunctor
import Data.Char (toLower)
import qualified Data.Text.Lazy as T
import Main.Utf8
import Options.Applicative
import Options.Applicative.Help (Doc, line)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.StreamingLedgerTables
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as V1
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.StreamingLedgerTables
import System.Console.ANSI
import qualified System.Directory as D
import System.Exit
import System.FS.API
import System.FS.CRC
import System.FS.IO
import System.FilePath (splitDirectories)
import qualified System.FilePath as F
import System.IO
import System.ProgressBar

data Format
  = Mem FilePath
  | LMDB FilePath
  deriving (Show, Read)

data Config = Config
  { from :: Format
  -- ^ Which format the input snapshot is in
  , to :: Format
  -- ^ Which format the output snapshot must be in
  }

getCommandLineConfig :: IO (Config, CardanoBlockArgs)
getCommandLineConfig =
  execParser $
    info
      ((,) <$> (Config <$> parseConfig In <*> parseConfig Out) <*> parseCardanoArgs <**> helper)
      ( fullDesc
          <> header "Utility for converting snapshots among the different snapshot formats used by cardano-node."
          <> progDescDoc programDescription
      )

programDescription :: Maybe Doc
programDescription =
  Just $
    mconcat
      [ "The input snapshot must correspond to a snapshot that was produced by "
      , "a cardano-node, and thus follows the naming convention used in the node."
      , line
      , "This means in particular that the filepath to the snapshot must have as "
      , "the last fragment a directory named after the slot number of the ledger "
      , "state snapshotted, plus an optional suffix, joined by an underscore."
      , line
      , line
      , "For the output, the same convention is enforced, so that the produced "
      , "snapshot can be loaded right away by a cardano-node."
      , line
      , line
      , "Note that snapshots that have a suffix will be preserved by the node. "
      , "If you produce a snapshot with a suffix and you start a node with it, "
      , "the node will take as many more snapshots as it is configured to take, "
      , "but it will never delete your snapshot, because it has a suffix on the name."
      , line
      , "Therefore, for the most common use case it is advisable to create a "
      , "snapshot without a suffix, as in:"
      , line
      , line
      , "```"
      , line
      , "$ mkdir out"
      , line
      , "$ snapshot-converter --<fmt>-in <some-path>/<slot> --<fmt>-out out/<slot> --config <path-to-config.json>"
      , line
      , "```"
      ]

data InOut = In | Out

inoutForGroup :: InOut -> String
inoutForGroup In = "Input arguments:"
inoutForGroup Out = "Output arguments:"

inoutForHelp :: InOut -> String -> Bool -> String
inoutForHelp In s b =
  mconcat $
    ("Input " <> s)
      : if b
        then
          [ ". Must be a filepath where the last fragment is named after the "
          , "slot of the snapshotted state plus an optional suffix. Example: `1645330287_suffix`."
          ]
        else []
inoutForHelp Out s b =
  mconcat $
    ("Output " <> s)
      : if b
        then
          [ ". Must be a filepath where the last fragment is named after the "
          , "slot of the snapshotted state plus an optional suffix. Example: `1645330287_suffix`."
          ]
        else []

inoutForCommand :: InOut -> String -> String
inoutForCommand In = (++ "-in")
inoutForCommand Out = (++ "-out")

parseConfig :: InOut -> Parser Format
parseConfig io =
  ( Mem
      <$> parserOptionGroup
        (inoutForGroup io)
        (parsePath (inoutForCommand io "mem") (inoutForHelp io "snapshot dir" True))
  )
    <|> ( LMDB
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lmdb") (inoutForHelp io "snapshot dir" True))
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

data Error blk
  = SnapshotError (SnapshotFailure blk)
  | BadDirectoryName FilePath
  | WrongSlotDirectoryName FilePath SlotNo
  | InvalidMetadata String
  | BackendMismatch SnapshotBackend SnapshotBackend
  | CRCMismatch CRC CRC
  | ReadTablesError DeserialiseFailure
  | Cancelled
  deriving Exception

instance StandardHash blk => Show (Error blk) where
  show (SnapshotError err) =
    "Couldn't deserialize the snapshot. Are you running the same node version that created the snapshot? "
      <> show err
  show (BadDirectoryName fp) =
    mconcat
      [ "Filepath "
      , fp
      , " is not an snapshot. The last fragment on the path should be"
      , " named after the slot number of the state it contains and an"
      , " optional suffix, such as `163470034` or `163470034_my-suffix`."
      ]
  show (InvalidMetadata s) = "Metadata is invalid: " <> s
  show (BackendMismatch b1 b2) =
    mconcat
      [ "Mismatched backend in snapshot. Reading as "
      , show b1
      , " but snapshot is "
      , show b2
      ]
  show (WrongSlotDirectoryName fp sl) =
    mconcat
      [ "The name of the snapshot (\""
      , fp
      , "\") does not correspond to the slot number of the state ("
      , (show . unSlotNo $ sl)
      , ")."
      ]
  show (CRCMismatch c1 c2) =
    mconcat
      [ "The input snapshot seems corrupted. Metadata has CRC "
      , show c1
      , " but reading it gives CRC "
      , show c2
      ]
  show (ReadTablesError df) =
    mconcat
      ["Error when reading entries in the UTxO tables: ", show df]
  show Cancelled = "Cancelled"

data InEnv = InEnv
  { inState :: LedgerState (CardanoBlock StandardCrypto) EmptyMK
  , inFilePath :: FilePath
  , inStream ::
      LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (YieldArgs (LedgerState (CardanoBlock StandardCrypto)) IO)
  , inProgressMsg :: String
  , inCRC :: CRC
  , inSnapReadCRC :: Maybe CRC
  }

data OutEnv = OutEnv
  { outFilePath :: FilePath
  , outStream ::
      LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (SinkArgs (LedgerState (CardanoBlock StandardCrypto)) IO)
  , outCreateExtra :: Maybe FilePath
  , outDeleteExtra :: Maybe FilePath
  , outProgressMsg :: String
  , outBackend :: SnapshotBackend
  }

main :: IO ()
main = withStdTerminalHandles $ do
  eRes <- runExceptT main'
  case eRes of
    Left err -> do
      putStrLn $ show err
      exitFailure
    Right () -> exitSuccess
 where
  main' = do
    lift $ cryptoInit
    (conf, args) <- lift $ getCommandLineConfig
    ccfg <- lift $ configCodec . pInfoConfig <$> mkProtocolInfo args

    InEnv{..} <- getInEnv ccfg (from conf)

    o@OutEnv{..} <- getOutEnv inState (to conf)

    wipeOutputPaths o

    lift $ putStr "Copying state file..." >> hFlush stdout
    lift $ D.copyFile (inFilePath F.</> "state") (outFilePath F.</> "state")
    lift $ putColored Green True "Done"

    lift $ putStr "Streaming ledger tables..." >> hFlush stdout >> saveCursor

    tid <- lift $ niceAnimatedProgressBar inProgressMsg outProgressMsg

    eRes <- lift $ runExceptT (stream inState inStream outStream)

    case eRes of
      Left err -> throwError $ ReadTablesError err
      Right (mCRCIn, mCRCOut) -> do
        lift $ maybe (pure ()) cancel tid
        lift $ clearLine >> restoreCursor >> cursorUp 1 >> putColored Green True "Done"
        let crcIn = maybe inCRC (crcOfConcat inCRC) mCRCIn
        maybe
          ( lift $
              putColored Yellow True "The metadata file is missing, the snapshot is not guaranteed to be correct!"
          )
          ( \cs ->
              Monad.when (cs /= crcIn) $ throwError $ CRCMismatch cs crcIn
          )
          inSnapReadCRC

        let crcOut = maybe inCRC (crcOfConcat inCRC) mCRCOut

        lift $ putStr "Generating new metadata file..." >> hFlush stdout
        putMetadata outFilePath (SnapshotMetadata outBackend crcOut)

        lift $ putColored Green True "Done"

  wipeOutputPaths OutEnv{..} = do
    wipePath outFilePath
    lift $ maybe (pure ()) (D.createDirectory . (outFilePath F.</>)) outCreateExtra
    maybe
      (pure ())
      wipePath
      outDeleteExtra

  getState ccfg fp@(pathToHasFS -> fs) = do
    eState <- lift $ do
      putStr $ "Reading ledger state from " <> (fp F.</> "state") <> "..."
      hFlush stdout
      runExceptT (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (mkFsPath ["state"]))
    case eState of
      Left err ->
        throwError . SnapshotError . InitFailureRead @(CardanoBlock StandardCrypto) . ReadSnapshotFailed $
          err
      Right st -> lift $ do
        putColored Green True " Done"
        pure . first ledgerState $ st

  getMetadata fp bknd = do
    (fs, ds) <- toDiskSnapshot fp
    mtd <-
      lift $ runExceptT $ loadSnapshotMetadata fs ds
    (,ds)
      <$> either
        ( \case
            MetadataFileDoesNotExist -> pure Nothing
            MetadataInvalid s -> throwError $ InvalidMetadata s
            MetadataBackendMismatch -> error "impossible"
        )
        ( \mtd' -> do
            if bknd /= snapshotBackend mtd'
              then throwError $ BackendMismatch bknd (snapshotBackend mtd')
              else pure $ Just $ snapshotChecksum mtd'
        )
        mtd

  putMetadata fp bknd = do
    (fs, ds) <- toDiskSnapshot fp
    lift $ writeSnapshotMetadata fs ds bknd

  getInEnv ccfg = \case
    Mem fp -> do
      (mtd, ds) <- getMetadata fp UTxOHDMemSnapshot
      (st, c) <- getState ccfg fp
      Monad.when
        ((unSlotNo <$> pointSlot (getTip st)) /= NotOrigin (dsNumber ds))
        ( throwError $
            WrongSlotDirectoryName
              (snapshotToDirName ds)
              ( withOrigin
                  ( error
                      "Impossible! the snapshot seems to be at Genesis but cardano-node would never create such an snapshot!"
                  )
                  id
                  $ pointSlot (getTip st)
              )
        )

      pure $
        InEnv
          st
          fp
          (fromInMemory (fp F.</> "tables" F.</> "tvar"))
          ("InMemory@[" <> fp <> "]")
          c
          mtd
    LMDB fp -> do
      (mtd, ds) <- getMetadata fp UTxOHDLMDBSnapshot
      (st, c) <- getState ccfg fp
      Monad.when
        ((unSlotNo <$> pointSlot (getTip st)) /= NotOrigin (dsNumber ds))
        ( throwError $
            WrongSlotDirectoryName
              (snapshotToDirName ds)
              (withOrigin undefined id $ pointSlot (getTip st))
        )

      pure $
        InEnv
          st
          fp
          (fromLMDB (fp F.</> "tables") defaultLMDBLimits)
          ("LMDB@[" <> fp <> "]")
          c
          mtd

  getOutEnv st = \case
    Mem fp -> do
      (_, ds) <- toDiskSnapshot fp
      Monad.when
        ((unSlotNo <$> pointSlot (getTip st)) /= NotOrigin (dsNumber ds))
        ( throwError $
            WrongSlotDirectoryName
              (snapshotToDirName ds)
              (withOrigin undefined id $ pointSlot (getTip st))
        )
      pure $
        OutEnv
          fp
          (toInMemory (fp F.</> "tables" F.</> "tvar"))
          (Just "tables")
          (Nothing)
          ("InMemory@[" <> fp <> "]")
          UTxOHDMemSnapshot
    LMDB fp -> do
      (_, ds) <- toDiskSnapshot fp
      Monad.when
        ((unSlotNo <$> pointSlot (getTip st)) /= NotOrigin (dsNumber ds))
        ( throwError $
            WrongSlotDirectoryName
              (snapshotToDirName ds)
              (withOrigin undefined id $ pointSlot (getTip st))
        )
      pure $
        OutEnv
          fp
          (toLMDB fp defaultLMDBLimits)
          Nothing
          Nothing
          ("LMDB@[" <> fp <> "]")
          UTxOHDLMDBSnapshot

-- Helpers

-- UI
niceAnimatedProgressBar :: String -> String -> IO (Maybe (Async IO ()))
niceAnimatedProgressBar inMsg outMsg = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      putStrLn ""
      pb <-
        newProgressBar
          defStyle{stylePrefix = msg (T.pack inMsg), stylePostfix = msg (T.pack outMsg)}
          10
          (Progress 1 100 ())

      fmap Just $
        async $
          let loop = do
                threadDelay 0.2
                updateProgress pb (\prg -> prg{progressDone = (progressDone prg + 4) `mod` 100})
           in Monad.forever loop
    else pure Nothing

putColored :: Color -> Bool -> String -> IO ()
putColored c b s = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  Monad.when stdoutSupportsANSI $ setSGR [SetColor Foreground Vivid c]
  if b
    then
      putStrLn s
    else
      putStr s
  Monad.when stdoutSupportsANSI $ setSGR [Reset]
  hFlush stdout

askForConfirmation ::
  ExceptT (Error (CardanoBlock StandardCrypto)) IO a ->
  String ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO a
askForConfirmation act infoMsg = do
  lift $ putColored Yellow False $ "I'm going to " <> infoMsg <> ". Continue? (Y/n) "
  answer <- lift $ getLine
  case map toLower answer of
    "y" -> act
    _ -> throwError Cancelled

-- | Ask before deleting
wipePath :: FilePath -> ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
wipePath fp = do
  exists <- lift $ D.doesDirectoryExist fp
  ( if exists
      then flip askForConfirmation ("wipe the path " <> fp)
      else id
    )
    (lift $ D.removePathForcibly fp >> D.createDirectoryIfMissing True fp)

toDiskSnapshot ::
  FilePath -> ExceptT (Error (CardanoBlock StandardCrypto)) IO (SomeHasFS IO, DiskSnapshot)
toDiskSnapshot fp@(F.splitFileName . maybeRemoveTrailingSlash -> (snapPath, snapName)) =
  maybe
    (throwError $ BadDirectoryName fp)
    (pure . (pathToHasFS snapPath,))
    $ snapshotFromPath snapName

-- | Given a filepath pointing to a snapshot (with or without a trailing slash), produce:
--
-- * A HasFS at the snapshot directory
pathToHasFS :: FilePath -> SomeHasFS IO
pathToHasFS (maybeRemoveTrailingSlash -> path) =
  SomeHasFS $ ioHasFS $ MountPoint path

maybeRemoveTrailingSlash :: String -> String
maybeRemoveTrailingSlash s = case last s of
  '/' -> init s
  '\\' -> init s
  _ -> s

defaultLMDBLimits :: V1.LMDBLimits
defaultLMDBLimits =
  V1.LMDBLimits
    { V1.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , V1.lmdbMaxDatabases = 10
    , V1.lmdbMaxReaders = 16
    }
