{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Convert snapshots among different formats. This is exposed in
-- @cardano-node@ as a subcommand and also via the @snapshot-converter@
-- executable.
module Ouroboros.Consensus.Cardano.SnapshotConversion
  ( SnapshotsDirectory (..)
  , ExportedSnapshotPath (..)
  , Snapshot (..)
  , SnapshotsDirectoryWithFormat (..)
  , snapshotDirectory
  , StandaloneFormat (..)
  , convertSnapshot
  ) where

import Codec.Serialise
import Control.Monad (when)
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bifunctor
import Data.Char (toLower)
import qualified Data.Text.Lazy as T
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Cardano.StreamingLedgerTables
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike hiding (yield)
import System.Console.ANSI
import qualified System.Directory as D
import System.FS.API
import System.FS.CRC
import System.FS.IO
import qualified System.FilePath as F
import System.IO
import System.ProgressBar
import System.Random

data SnapshotsDirectory = SnapshotsDirectory {getSnapshotDir :: FilePath}

-- | The directory holding a standalone (exported) LSM snapshot, i.e. the LSM
-- ledger tables exported out of a session via @lsm-tree@'s @exportSnapshot@.
--
-- This is paired with a 'SnapshotsDirectory' that holds the @state@/@meta@
-- files, just like the parts of an LSM snapshot inside a running node are split
-- between the ChainDB @ledger@ directory and the LSM session directory.
data ExportedSnapshotPath = ExportedSnapshotPath {getExportedSnapshotPath :: FilePath}

data StandaloneFormat
  = Mem

data SnapshotsDirectoryWithFormat
  = StandaloneSnapshot SnapshotsDirectory StandaloneFormat
  | -- | A standalone (exported) LSM snapshot. Conversions never operate on a
    -- live LSM database; they only ever read from or write to exported
    -- snapshots (see 'ExportedSnapshotPath').
    ExportedLSMSnapshot SnapshotsDirectory ExportedSnapshotPath

data Snapshot = Snapshot
  { snapshotSnapShotDir :: SnapshotsDirectoryWithFormat
  , snapshotDiskSnapshot :: DiskSnapshot
  }

snapshotDirectory :: SnapshotsDirectoryWithFormat -> SnapshotsDirectory
snapshotDirectory (StandaloneSnapshot fp _) = fp
snapshotDirectory (ExportedLSMSnapshot fp _) = fp

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

data Error blk
  = SnapshotError (SnapshotFailure blk)
  | BadDirectoryName FilePath
  | WrongSlotDirectoryName FilePath SlotNo
  | SnapshotAtGenesis
  | SnapshotAtByronEra
  | InvalidMetadata String
  | BackendMismatch SnapshotBackend SnapshotBackend
  | CRCMismatch CRC CRC
  | ReadTablesError DeserialiseFailure
  | Cancelled
  deriving Exception

instance StandardHash blk => Show (Error blk) where
  show SnapshotAtGenesis =
    "The provided snapshot is at Genesis. This should be impossible, the cardano-node will never create those!"
  show SnapshotAtByronEra =
    "The provided snapshot is in the Byron era, which carries no UTxO-HD ledger tables to convert."
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

-- | A backend (in-memory or LSM) packed with its 'StreamingBackend' dictionary,
-- at the single era @x@ that the snapshot's tables live in (see
-- 'withCardanoCurrentEra'). The backend itself (@Mem@\/@LSM@) is existential;
-- the era @x@ is fixed by the enclosing era scope so the input and output
-- backends agree on the entry type streamed between them.
data SomeBackend x c where
  SomeBackend ::
    StreamingBackend IO backend LedgerState x =>
    c IO backend LedgerState x -> SomeBackend x c

convertSnapshot ::
  Bool ->
  ProtocolInfo (CardanoBlock StandardCrypto) ->
  Snapshot ->
  Snapshot ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
convertSnapshot interactive (configCodec . pInfoConfig -> ccfg) from to = do
  inSnapReadCRC <- getMetadata inSnap inExpectedBackend
  (inState, inCRC) <- getState inSnap
  checkSnapSlot inState inSnap
  checkSnapSlot inState outSnap

  wipePath interactive (getSnapshotDir outSnapDir F.</> snapshotToDirName outSnap)

  when interactive $ lift $ putStr "Copying state file..." >> hFlush stdout
  inStateFile <- lift $ unsafeToFilePath inHasFS (snapshotToStatePath inSnap)
  outStateFile <- lift $ unsafeToFilePath outHasFS (snapshotToStatePath outSnap)
  lift $ D.copyFile inStateFile outStateFile
  when interactive $ lift $ putColored Green True "Done"

  when interactive $ lift $ putStr "Streaming ledger tables..." >> hFlush stdout >> saveCursor

  tid <-
    if interactive
      then lift $ niceAnimatedProgressBar inProgressMsg outProgressMsg
      else pure Nothing

  -- The snapshot's UTxO tables live in the era at its tip; stream both the
  -- input and output at that single era. A Byron tip has no UTxO-HD tables.
  (mCRCIn, mCRCOut) <-
    withCardanoCurrentEra inState (throwError SnapshotAtByronEra) $ \_idx stEra proj -> do
      eRes <- lift $ runExceptT (stream stEra (mkInStream stEra proj) (mkOutStream stEra))
      case eRes of
        Left err -> throwError $ ReadTablesError err
        Right res -> pure res

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
  lift $ putMetadata (SnapshotMetadata outBackend crcOut TablesCodecVersion2)

  when interactive $ lift $ putColored Green True "Done"
 where
  inSnap, outSnap :: DiskSnapshot
  inSnap = snapshotDiskSnapshot from
  outSnap = snapshotDiskSnapshot to

  inSnapDir, outSnapDir :: SnapshotsDirectory
  inSnapDir = snapshotDirectory $ snapshotSnapShotDir from
  outSnapDir = snapshotDirectory $ snapshotSnapShotDir to

  inHasFS, outHasFS :: HasFS IO HandleIO
  inHasFS = ioHasFS (MountPoint (getSnapshotDir inSnapDir))
  outHasFS = ioHasFS (MountPoint (getSnapshotDir outSnapDir))

  inSomeHasFS, outSomeHasFS :: SomeHasFS IO
  inSomeHasFS = SomeHasFS inHasFS
  outSomeHasFS = SomeHasFS outHasFS

  getState ::
    DiskSnapshot ->
    ExceptT
      (Error (CardanoBlock StandardCrypto))
      IO
      (LedgerState (CardanoBlock StandardCrypto), CRC)
  getState ds = do
    eState <- lift $ do
      when interactive $ putStr $ "Reading ledger state from " <> snapshotToDirName ds <> "..."
      when interactive $ hFlush stdout
      runExceptT
        (readExtLedgerState inSomeHasFS (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds))
    case eState of
      Left err ->
        throwError . SnapshotError . InitFailureRead @(CardanoBlock StandardCrypto) . ReadSnapshotFailed $
          err
      Right st -> lift $ do
        when interactive $ putColored Green True " Done"
        pure . first ledgerState $ st

  -- Get the CRC of the input snapshot if the backend matches the expected one
  getMetadata ::
    DiskSnapshot ->
    SnapshotBackend ->
    ExceptT (Error (CardanoBlock StandardCrypto)) IO (Maybe CRC)
  getMetadata ds expectedBackend = do
    mtd <-
      lift $
        runExceptT $
          loadSnapshotMetadata inSomeHasFS ds
    case mtd of
      Left MetadataFileDoesNotExist -> pure Nothing
      Left (MetadataInvalid why) -> throwError $ InvalidMetadata why
      Left MetadataBackendMismatch -> error "impossible"
      Right mtd' ->
        if expectedBackend /= snapshotBackend mtd'
          then throwError $ BackendMismatch expectedBackend (snapshotBackend mtd')
          else pure $ Just $ snapshotChecksum mtd'

  -- Write the snapshot metadata for the output snapshot
  putMetadata :: SnapshotMetadata -> IO ()
  putMetadata bknd =
    writeSnapshotMetadata outSomeHasFS outSnap bknd

  checkSnapSlot ::
    LedgerState (CardanoBlock StandardCrypto) ->
    DiskSnapshot ->
    ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
  checkSnapSlot st ds =
    withOrigin
      (throwError SnapshotAtGenesis)
      ( \t ->
          Monad.when (unSlotNo t /= dsNumber ds) $
            throwError $
              WrongSlotDirectoryName (snapshotToDirName ds) t
      )
      (pointSlot $ getTip st)

  -- The expected backend of the input snapshot (to validate its metadata).
  inExpectedBackend :: SnapshotBackend
  inExpectedBackend = case from of
    Snapshot (StandaloneSnapshot _ Mem) _ -> UTxOHDMemSnapshot
    Snapshot ExportedLSMSnapshot{} _ -> UTxOHDLSMSnapshot

  -- The backend to record for the output snapshot's metadata.
  outBackend :: SnapshotBackend
  outBackend = case to of
    Snapshot (StandaloneSnapshot _ Mem) _ -> UTxOHDMemSnapshot
    Snapshot ExportedLSMSnapshot{} _ -> UTxOHDLSMSnapshot

  inProgressMsg, outProgressMsg :: String
  inProgressMsg = case from of
    Snapshot (StandaloneSnapshot _ Mem) _ -> "InMemory@[" <> snapshotToDirName inSnap <> "]"
    Snapshot (ExportedLSMSnapshot _ (getExportedSnapshotPath -> exportDir)) _ ->
      "LSM (exported)@[" <> exportDir <> "]"
  outProgressMsg = case to of
    Snapshot (StandaloneSnapshot _ Mem) _ -> "InMemory@[" <> snapshotToDirName outSnap <> "]"
    Snapshot (ExportedLSMSnapshot _ (getExportedSnapshotPath -> exportDir)) _ ->
      "LSM (exported)@[" <> exportDir <> "]"

  -- Build the input (yield) backend at the snapshot's tip era. The LSM handle is
  -- opened at the hard-fork block and 'readRange' projects it onto the era @blk@.
  mkInStream ::
    StreamableEra proto era =>
    LedgerState (ShelleyBlock proto era) ->
    (Values (CardanoBlock StandardCrypto) -> Values (ShelleyBlock proto era)) ->
    IO (SomeBackend (ShelleyBlock proto era) YieldArgs)
  mkInStream stEra proj = case from of
    Snapshot (StandaloneSnapshot _ Mem) _ -> do
      -- Read the input snapshot's on-disk values codec version so we can read
      -- both the legacy list-wrapped (V1) and the bare-map (V2) framings. If
      -- the metadata is missing, assume the current (V2) framing.
      inVersion <-
        either (const TablesCodecVersion2) snapshotTablesCodecVersion
          <$> runExceptT (loadSnapshotMetadata inSomeHasFS inSnap)
      pure $ SomeBackend $ mkInMemYieldArgs inVersion inSomeHasFS inSnap stEra
    Snapshot (ExportedLSMSnapshot _ (getExportedSnapshotPath -> exportDir)) _ ->
      SomeBackend
        <$> mkExportedLSMYieldArgs @(CardanoBlock StandardCrypto)
          proj
          exportDir
          inSnap
          stdMkBlockIOFS
          (SomeHasFS . ioHasFS . MountPoint)
          newStdGen

  -- Build the output (sink) backend at the snapshot's tip era.
  mkOutStream ::
    StreamableEra proto era =>
    LedgerState (ShelleyBlock proto era) ->
    IO (SomeBackend (ShelleyBlock proto era) SinkArgs)
  mkOutStream stEra = case to of
    Snapshot (StandaloneSnapshot _ Mem) _ ->
      pure $ SomeBackend $ mkInMemSinkArgs outSomeHasFS outSnap stEra
    Snapshot (ExportedLSMSnapshot _ (ExportedSnapshotPath exportDir)) _ ->
      SomeBackend
        <$> mkExportedLSMSinkArgs
          exportDir
          outSnap
          outSomeHasFS
          stdMkBlockIOFS
          newStdGen

  stream ::
    forall x.
    LedgerState x ->
    IO (SomeBackend x YieldArgs) ->
    IO (SomeBackend x SinkArgs) ->
    ExceptT DeserialiseFailure IO (Maybe CRC, Maybe CRC)
  stream st mYieldArgs mSinkArgs =
    ExceptT $
      bracket
        ((,) <$> mYieldArgs <*> mSinkArgs)
        ( \(SomeBackend yArgs, SomeBackend sArgs) -> do
            releaseYieldArgs yArgs
            releaseSinkArgs sArgs
        )
        ( \( SomeBackend (yArgs :: YieldArgs IO backend1 LedgerState x)
             , SomeBackend (sArgs :: SinkArgs IO backend2 LedgerState x)
             ) ->
              runExceptT $ yield (Proxy @backend1) yArgs st $ sink (Proxy @backend2) sArgs st
        )

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
