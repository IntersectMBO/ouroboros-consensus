{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Convert snapshots among different formats. This is exposed in
-- @cardano-node@ as a subcommand and also via the @snapshot-converter@
-- executable.
module Ouroboros.Consensus.Cardano.SnapshotConversion
  ( Format (..)
  , parseFormat
  , convertSnapshot
  ) where

import Codec.Serialise
import Control.Monad (when)
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.ResourceRegistry
import Data.Bifunctor
import Data.Char (toLower)
import qualified Data.Text.Lazy as T
import Options.Applicative
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Cardano.StreamingLedgerTables
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as V1
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike hiding (yield)
import System.Console.ANSI
import qualified System.Directory as D
import System.FS.API
import System.FS.CRC
import System.FS.IO
import System.FilePath (splitDirectories)
import qualified System.FilePath as F
import System.IO
import System.ProgressBar
import System.Random

data Format
  = Mem FilePath
  | LMDB FilePath
  | LSM FilePath FilePath
  deriving (Show, Read)

{-------------------------------------------------------------------------------
 Optparse
-------------------------------------------------------------------------------}

inoutForHelp :: String -> Bool -> String
inoutForHelp s b =
  mconcat $
    ("Output " <> s)
      : if b
        then
          [ ". Must be a filepath where the last fragment is named after the "
          , "slot of the snapshotted state plus an optional suffix. Example: `1645330287_suffix`."
          ]
        else []

parsePath :: String -> String -> Parser FilePath
parsePath optName strHelp =
  strOption
    ( mconcat
        [ long optName
        , help strHelp
        , metavar "PATH"
        ]
    )

parseFormat :: Parser Format
parseFormat =
  ( Mem
      <$> (parsePath "mem-out" (inoutForHelp "snapshot dir" True))
  )
    <|> ( LMDB
            <$> (parsePath "lmdb-out" (inoutForHelp "snapshot dir" True))
        )
    <|> ( LSM
            <$> (parsePath "lsm-snapshot-out" (inoutForHelp "snapshot dir" True))
            <*> (parsePath "lsm-database-out" (inoutForHelp "LSM database" False))
        )

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Environments
-------------------------------------------------------------------------------}

data InEnv backend = InEnv
  { inState :: LedgerState (CardanoBlock StandardCrypto) EmptyMK
  -- ^ Ledger state (without tables) that will be used to index the snapshot.
  , inFilePath :: FilePath
  -- ^ The file path to the LedgerDB snapshot
  , inStream ::
      LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (SomeBackend YieldArgs)
  -- ^ Yield arguments for producing a stream of TxOuts
  , inProgressMsg :: String
  -- ^ A progress message (just for displaying)
  , inCRC :: CRC
  -- ^ The CRC of the input @state@ file as read
  , inSnapReadCRC :: Maybe CRC
  -- ^ The CRC of the input snapshot from the metadata file
  }

data OutEnv backend = OutEnv
  { outFilePath :: FilePath
  -- ^ The output snapshot directory
  , outStream ::
      LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (SomeBackend SinkArgs)
  -- ^ Sink arguments for consuming a stream of TxOuts
  , outDeleteExtra :: Maybe FilePath
  -- ^ In case some other directory needs to be wiped out
  , outProgressMsg :: String
  -- ^ A progress message (just for displaying)
  , outBackend :: SnapshotBackend
  -- ^ The backend used for the output snapshot, to write it in the metadata
  }

data SomeBackend c where
  SomeBackend ::
    StreamingBackend IO backend (LedgerState (CardanoBlock StandardCrypto)) =>
    c IO backend (LedgerState (CardanoBlock StandardCrypto)) -> SomeBackend c

convertSnapshot ::
  Bool ->
  ProtocolInfo (CardanoBlock StandardCrypto) ->
  Format ->
  Format ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
convertSnapshot interactive (configCodec . pInfoConfig -> ccfg) from to = do
  InEnv{..} <- getInEnv

  o@OutEnv{..} <- getOutEnv inState

  wipeOutputPaths o

  when interactive $ lift $ putStr "Copying state file..." >> hFlush stdout
  lift $ D.copyFile (inFilePath F.</> "state") (outFilePath F.</> "state")
  when interactive $ lift $ putColored Green True "Done"

  when interactive $ lift $ putStr "Streaming ledger tables..." >> hFlush stdout >> saveCursor

  tid <-
    if interactive
      then lift $ niceAnimatedProgressBar inProgressMsg outProgressMsg
      else pure Nothing

  eRes <- lift $ runExceptT (stream inState inStream outStream)

  case eRes of
    Left err -> throwError $ ReadTablesError err
    Right (mCRCIn, mCRCOut) -> do
      lift $ maybe (pure ()) cancel tid
      when interactive $ lift $ clearLine >> restoreCursor >> cursorUp 1 >> putColored Green True "Done"
      let crcIn = maybe inCRC (crcOfConcat inCRC) mCRCIn
      when interactive $
        maybe
          ( lift $
              putColored Yellow True "The metadata file is missing, the snapshot is not guaranteed to be correct!"
          )
          ( \cs ->
              Monad.when (cs /= crcIn) $ throwError $ CRCMismatch cs crcIn
          )
          inSnapReadCRC

      let crcOut = maybe inCRC (crcOfConcat inCRC) mCRCOut

      when interactive $ lift $ putStr "Generating new metadata file..." >> hFlush stdout
      putMetadata outFilePath (SnapshotMetadata outBackend crcOut TablesCodecVersion1)

      when interactive $ lift $ putColored Green True "Done"
 where
  wipeOutputPaths OutEnv{..} = do
    wipePath interactive outFilePath
    maybe
      (pure ())
      (wipePath interactive)
      outDeleteExtra

  getState fp@(pathToHasFS -> fs) = do
    eState <- lift $ do
      when interactive $ putStr $ "Reading ledger state from " <> (fp F.</> "state") <> "..."
      when interactive $ hFlush stdout
      runExceptT (readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (mkFsPath ["state"]))
    case eState of
      Left err ->
        throwError . SnapshotError . InitFailureRead @(CardanoBlock StandardCrypto) . ReadSnapshotFailed $
          err
      Right st -> lift $ do
        when interactive $ putColored Green True " Done"
        pure . first ledgerState $ st

  -- Metadata management
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

  -- Produce an InEnv from the given arguments
  getInEnv = case from of
    Mem fp -> do
      (mtd, ds) <- getMetadata fp UTxOHDMemSnapshot
      (st, c) <- getState fp
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
          (\a b -> SomeBackend <$> mkInMemYieldArgs (fp F.</> "tables") a b)
          ("InMemory@[" <> fp <> "]")
          c
          mtd
    LMDB fp -> do
      (mtd, ds) <- getMetadata fp UTxOHDLMDBSnapshot
      (st, c) <- getState fp
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
          (\a b -> SomeBackend <$> V1.mkLMDBYieldArgs (fp F.</> "tables") defaultLMDBLimits a b)
          ("LMDB@[" <> fp <> "]")
          c
          mtd
    LSM fp lsmDbPath -> do
      (mtd, ds) <- getMetadata fp UTxOHDLSMSnapshot
      (st, c) <- getState fp
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
          ( \a b ->
              SomeBackend <$> mkLSMYieldArgs lsmDbPath (last $ splitDirectories fp) stdMkBlockIOFS newStdGen a b
          )
          ("LSM@[" <> lsmDbPath <> "]")
          c
          mtd

  -- Produce an OutEnv from the given arguments
  getOutEnv st = case to of
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
          (\a b -> SomeBackend <$> mkInMemSinkArgs (fp F.</> "tables") a b)
          Nothing
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
          (\a b -> SomeBackend <$> V1.mkLMDBSinkArgs fp defaultLMDBLimits a b)
          Nothing
          ("LMDB@[" <> fp <> "]")
          UTxOHDLMDBSnapshot
    LSM fp lsmDbPath -> do
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
          ( \a b ->
              SomeBackend <$> mkLSMSinkArgs lsmDbPath (last $ splitDirectories fp) stdMkBlockIOFS newStdGen a b
          )
          (Just lsmDbPath)
          ("LSM@[" <> lsmDbPath <> "]")
          UTxOHDLSMSnapshot

  stream ::
    LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
    ( LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (SomeBackend YieldArgs)
    ) ->
    ( LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
      ResourceRegistry IO ->
      IO (SomeBackend SinkArgs)
    ) ->
    ExceptT DeserialiseFailure IO (Maybe CRC, Maybe CRC)
  stream st mYieldArgs mSinkArgs =
    ExceptT $
      withRegistry $ \reg -> do
        (SomeBackend (yArgs :: YieldArgs IO backend1 l)) <- mYieldArgs st reg
        (SomeBackend (sArgs :: SinkArgs IO backend2 l)) <- mSinkArgs st reg
        runExceptT $ yield (Proxy @backend1) yArgs st $ sink (Proxy @backend2) sArgs st

{-------------------------------------------------------------------------------
  User interaction
-------------------------------------------------------------------------------}

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
  Bool ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO a ->
  String ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO a
askForConfirmation False act _ = act
askForConfirmation True act infoMsg = do
  lift $ putColored Yellow False $ "I'm going to " <> infoMsg <> ". Continue? (Y/n) "
  answer <- lift $ getLine
  case map toLower answer of
    "y" -> act
    _ -> throwError Cancelled

-- | Ask before deleting
wipePath :: Bool -> FilePath -> ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
wipePath interactive fp = do
  exists <- lift $ D.doesDirectoryExist fp
  ( if exists
      then flip (askForConfirmation interactive) ("wipe the path " <> fp)
      else id
    )
    (lift $ D.removePathForcibly fp >> D.createDirectoryIfMissing True fp)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}
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
