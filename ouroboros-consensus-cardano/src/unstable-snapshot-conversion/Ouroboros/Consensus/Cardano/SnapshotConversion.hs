{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Convert snapshots among different formats. This is exposed in
-- @cardano-node@ as a subcommand and also via the @snapshot-converter@
-- executable.
--
-- The conversion is era-aware: only Shelley-based eras carry an on-disk
-- UTxO blob, so for them we dispatch to the per-era 'SnapshotYielder' /
-- 'SnapshotSinker' provided by the backend modules; Byron snapshots have
-- only a state file and no UTxO, so the streaming step is a no-op.
module Ouroboros.Consensus.Cardano.SnapshotConversion
  ( SnapshotsDirectory (..)
  , LSMDatabaseFilePath (..)
  , Snapshot (..)
  , SnapshotsDirectoryWithFormat (..)
  , snapshotDirectory
  , StandaloneFormat (..)
  , convertSnapshot
  ) where

import Cardano.Ledger.Binary.Decoding (DecShareCBOR, Interns, Share)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Codec.CBOR.Read (DeserialiseFailure)
import Codec.Serialise (decode)
import Control.Monad (when)
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bifunctor
import Data.Char (toLower)
import Data.MemPack (MemPack)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Strict (NP (..), NS, hap, hcollapse, hmap, type (-.->) (..))
import qualified Data.SOP.Telescope as Telescope
import qualified Data.Text.Lazy as T
import Ouroboros.Consensus.Backends.InMemory
  ( inMemorySnapshotSinker
  , inMemorySnapshotYielder
  )
import Ouroboros.Consensus.Backends.LSM
  ( lsmSnapshotSinker
  , lsmSnapshotYielder
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator (LedgerState (HardForkLedgerState))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  )
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, shelleyLedgerState)
import Ouroboros.Consensus.Shelley.Ledger.SnapshotStream
  ( SnapshotSinker (..)
  , SnapshotYielder (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
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

data SnapshotsDirectory = SnapshotsDirectory {getSnapshotDir :: FilePath}

data LSMDatabaseFilePath = LSMDatabaseFilePath {getLSMDatabaseDir :: FilePath}

data StandaloneFormat
  = Mem

data SnapshotsDirectoryWithFormat
  = StandaloneSnapshot SnapshotsDirectory StandaloneFormat
  | LSMSnapshot SnapshotsDirectory LSMDatabaseFilePath

data Snapshot = Snapshot
  { snapshotSnapShotDir :: SnapshotsDirectoryWithFormat
  , snapshotDiskSnapshot :: DiskSnapshot
  }

snapshotDirectory :: SnapshotsDirectoryWithFormat -> SnapshotsDirectory
snapshotDirectory (StandaloneSnapshot fp _) = fp
snapshotDirectory (LSMSnapshot fp _) = fp

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

data Error blk
  = SnapshotError (SnapshotFailure blk)
  | BadDirectoryName FilePath
  | WrongSlotDirectoryName FilePath SlotNo
  | SnapshotAtGenesis
  | InvalidMetadata String
  | BackendMismatch SnapshotBackend SnapshotBackend
  | CRCMismatch CRC CRC
  | ReadTablesError DeserialiseFailure
  | Cancelled
  deriving Exception

instance StandardHash blk => Show (Error blk) where
  show SnapshotAtGenesis =
    "The provided snapshot is at Genesis. This should be impossible, the cardano-node will never create those!"
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

data InEnv = InEnv
  { inState :: LedgerState (CardanoBlock StandardCrypto)
  -- ^ Ledger state that will be used to dispatch the per-era streaming.
  , inYielder :: IO (SnapshotYielder IO)
  -- ^ Yielder producing the stream of @(TxIn, TxOut)@ pairs.
  , inProgressMsg :: String
  -- ^ A progress message (just for displaying)
  , inCRC :: CRC
  -- ^ The CRC of the input @state@ file as read
  , inSnapReadCRC :: Maybe CRC
  -- ^ The CRC of the input snapshot from the metadata file
  }

data OutEnv = OutEnv
  { outSinker :: IO (SnapshotSinker IO)
  -- ^ Sinker consuming the stream of @(TxIn, TxOut)@ pairs.
  , outDeleteExtra :: Maybe FilePath
  -- ^ In case some other directory needs to be wiped out
  , outProgressMsg :: String
  -- ^ A progress message (just for displaying)
  , outBackend :: SnapshotBackend
  -- ^ The backend used for the output snapshot, to write it in the metadata
  }

convertSnapshot ::
  Bool ->
  ProtocolInfo IO (CardanoBlock StandardCrypto) ->
  Snapshot ->
  Snapshot ->
  ExceptT (Error (CardanoBlock StandardCrypto)) IO ()
convertSnapshot interactive (configCodec . pInfoConfig -> ccfg) from to = do
  InEnv{..} <- getInEnv

  o@OutEnv{..} <- getOutEnv inState

  wipeOutputPaths o

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

  eRes <- lift $ runExceptT (stream inState inYielder outSinker)

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
      lift $ putMetadata (SnapshotMetadata outBackend crcOut TablesCodecVersion1)

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

  wipeOutputPaths OutEnv{..} = do
    wipePath interactive (getSnapshotDir outSnapDir F.</> snapshotToDirName outSnap)
    maybe
      (pure ())
      (wipePath interactive)
      outDeleteExtra

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

  -- Produce an InEnv from the given arguments
  getInEnv :: ExceptT (Error (CardanoBlock StandardCrypto)) IO InEnv
  getInEnv = case from of
    Snapshot (StandaloneSnapshot _ Mem) _ -> do
      metadataCrc <- getMetadata inSnap UTxOHDMemSnapshot
      (st, c) <- getState inSnap
      checkSnapSlot st inSnap
      pure $
        InEnv
          st
          (pure $ inMemorySnapshotYielder inSomeHasFS inSnap)
          ("InMemory@[" <> snapshotToDirName inSnap <> "]")
          c
          metadataCrc
    Snapshot (LSMSnapshot _ (getLSMDatabaseDir -> lsmDbPath)) _ -> do
      metadataCrc <- getMetadata inSnap UTxOHDLSMSnapshot
      (st, c) <- getState inSnap
      checkSnapSlot st inSnap
      pure $
        InEnv
          st
          ( let (mount, dbPath) = splitLsmPath lsmDbPath
             in lsmSnapshotYielder mount dbPath inSnap
          )
          ("LSM@[" <> lsmDbPath <> "]")
          c
          metadataCrc

  -- Produce an OutEnv from the given arguments
  getOutEnv ::
    LedgerState (CardanoBlock StandardCrypto) ->
    ExceptT (Error (CardanoBlock StandardCrypto)) IO OutEnv
  getOutEnv st = case to of
    Snapshot (StandaloneSnapshot _ Mem) _ -> do
      checkSnapSlot st outSnap
      pure $
        OutEnv
          (pure $ inMemorySnapshotSinker outSomeHasFS outSnap)
          Nothing
          ("InMemory@[" <> snapshotToDirName outSnap <> "]")
          UTxOHDMemSnapshot
    Snapshot (LSMSnapshot _ (LSMDatabaseFilePath lsmDbPath)) _ -> do
      checkSnapSlot st outSnap
      let (mount, dbPath) = splitLsmPath lsmDbPath
      pure $
        OutEnv
          (lsmSnapshotSinker mount dbPath outSomeHasFS outSnap)
          (Just lsmDbPath)
          ("LSM@[" <> lsmDbPath <> "]")
          UTxOHDLSMSnapshot

-- | Split an absolute LSM database path into a filesystem mount point
-- (the parent directory, used to construct the 'HasFS') and a relative
-- 'FsPath' within that mount (the database directory's name). The
-- backend modules expect this split because they construct their
-- 'HasBlockIO' against a 'MountPoint' and then open the LSM database
-- under a path inside that mount.
splitLsmPath :: FilePath -> (FilePath, FsPath)
splitLsmPath p = (F.takeDirectory p, mkFsPath [F.takeFileName p])

-- | Stream the UTxO entries from the yielder to the sinker.
--
-- Dispatches on the era of the supplied 'LedgerState': Byron carries no
-- UTxO blob, so the stream is empty (returning @(Nothing, Nothing)@);
-- each Shelley-based era projects its @NewEpochState@ from the per-era
-- state and threads it through the yielder, which decodes entries
-- against the era's share context.
stream ::
  LedgerState (CardanoBlock StandardCrypto) ->
  IO (SnapshotYielder IO) ->
  IO (SnapshotSinker IO) ->
  ExceptT DeserialiseFailure IO (Maybe CRC, Maybe CRC)
stream (HardForkLedgerState hfs) mkYielder mkSinker =
  hcollapse $ hap perEra perEraState
 where
  -- @ns@ projects the current era's pure 'LedgerState' out of the
  -- HFC telescope. The per-era 'Fn' below receives it and runs the
  -- per-era streaming. Byron's slot in the NP is a no-op that
  -- short-circuits without ever opening a yielder or sinker.
  perEraState :: NS LedgerState (CardanoEras StandardCrypto)
  perEraState = hmap currentState (Telescope.tip (getHardForkState hfs))

  perEra ::
    NP
      (LedgerState -.-> K (ExceptT DeserialiseFailure IO (Maybe CRC, Maybe CRC)))
      (CardanoEras StandardCrypto)
  perEra =
    Fn (\_ -> K (pure (Nothing, Nothing)))
      :* shelleyFn
      :* shelleyFn
      :* shelleyFn
      :* shelleyFn
      :* shelleyFn
      :* shelleyFn
      :* shelleyFn
      :* Nil

  shelleyFn ::
    forall proto era.
    ( SL.Era era
    , MemPack (SL.TxOut era)
    , DecShareCBOR (SL.TxOut era)
    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
    , SL.EraCertState era
    ) =>
    (LedgerState -.-> K (ExceptT DeserialiseFailure IO (Maybe CRC, Maybe CRC)))
      (ShelleyBlock proto era)
  shelleyFn = Fn $ \(shelleyLedgerState -> nes) ->
    K $ ExceptT $ do
      yielder <- mkYielder
      sinker <- mkSinker
      runExceptT (runYielder yielder nes (runSinker sinker))
        `finally` (releaseYielder yielder >> releaseSinker sinker)

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
