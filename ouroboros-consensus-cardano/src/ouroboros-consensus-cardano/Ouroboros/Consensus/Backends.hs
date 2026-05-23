{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Backends (inMemoryBackendArgs, lsmBackendArgsIO) where

import Cardano.Ledger.Binary.Decoding
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Codec.Serialise
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import Data.Maybe
import Data.MemPack
import Data.SOP.BasicFunctors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.Word
import qualified Database.LSMTree as LSM
import Ouroboros.Consensus.Backends.InMemory
import Ouroboros.Consensus.Backends.LSM
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Ledger
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.FS.CRC

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

-- | Construct the 'LedgerDbBackendArgs' for the in-memory backend.
--
-- This is the entry point that the node and tools call to wire the
-- in-memory backend into the LedgerDB. The resources acquired here are
-- the 'MkHandle' and 'MkHandleFromSnapshot' factories produced from the
-- file system passed in by 'acquireBackend'; those factories are then
-- closed over by the fields of the resulting 'BackendResources'.
--
-- The in-memory backend has no long-lived resources of its own, so
-- 'brRelease' is a no-op and 'acquireBackend' does not allocate into the
-- temporary registry.
inMemoryBackendArgs ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  LedgerDbBackendArgs m (CardanoBlock c)
inMemoryBackendArgs = LedgerDbBackendArgs $ \_tr shfs ->
  let
    mkH :: MkHandle m
    mkH = mkInMemoryFactory shfs

    mkHs :: MkHandleFromSnapshot m
    mkHs = mkInMemoryFromSnapshot shfs
   in
    pure
      BackendResources
        { brLoadSnapshot = loadSnapshot UTxOHDMemSnapshot mkHs mkH
        , brSnapshotManager = mkSnapshotManager UTxOHDMemSnapshot
        , brRelease = pure ()
        , ledgerTablesFactory = mkH
        }

lsmBackendArgsIO ::
  forall c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c)) =>
  FsPath -> FilePath -> Word64 -> LedgerDbBackendArgs IO (CardanoBlock c)
lsmBackendArgsIO lsmPath fastStoragePath salt = LedgerDbBackendArgs $ \trcr shfs -> do
  (fs, blockio) <-
    allocateTemp
      (ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (\(_, bio) -> BIO.close bio >> pure True)
      impossibleToNotTransfer
  lift $ createDirectoryIfMissing fs True lsmPath
  session <-
    allocateTemp
      ( LSM.openSession
          (BackendTrace . SomeBackendTrace >$< trcr)
          fs
          blockio
          salt
          lsmPath
      )
      (\s -> LSM.closeSession s >> pure True)
      impossibleToNotTransfer
  let
    mkH :: MkHandle IO
    mkH = mkLSMFactory session shfs

    mkHs :: MkHandleFromSnapshot IO
    mkHs = mkLSMFromSnapshot session shfs
   in
    pure
      BackendResources
        { brLoadSnapshot = loadSnapshot UTxOHDLSMSnapshot mkHs mkH
        , brSnapshotManager = mkSnapshotManager UTxOHDLSMSnapshot
        , brRelease = do
            LSM.closeSession session
            BIO.close blockio
        , ledgerTablesFactory = mkH
        }

-- | Load an 'ExtStateHandle' from a snapshot using the in-memory backend.
--
-- Reads the 'ExtLedgerState' from disk, then delegates to
-- 'MkHandleFromSnapshot' to materialise the in-memory tables for the
-- snapshot's era.
loadSnapshot ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  -- | The backend tag that the snapshot's metadata is expected to carry.
  -- Snapshots tagged for any other backend are rejected with
  -- 'MetadataBackendMismatch'.
  SnapshotBackend ->
  MkHandleFromSnapshot m ->
  MkHandle m ->
  CodecConfig (CardanoBlock c) ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT
    (SnapshotFailure (CardanoBlock c))
    m
    (ExtStateHandle m (CardanoBlock c), RealPoint (CardanoBlock c))
loadSnapshot expectedBackend mkFromSnapshot mkH ccfg fs@(SomeHasFS hfs) ds = do
  fileEx <- lift $ doesFileExist hfs (snapshotToDirPath ds)
  Monad.when fileEx $ throwError $ InitFailureRead ReadSnapshotIsLegacy

  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= expectedBackend) $
    throwError $
      InitFailureRead $
        ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (ExtLedgerState ls hs, checksumAsRead) <-
    withExceptT
      (InitFailureRead . ReadSnapshotFailed)
      $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (getTip ls) of
    Origin -> throwError InitFailureGenesis
    NotOrigin pt -> do
      ns <-
        hsequence' $
          hzipWith
            apFn
            ( let sf ::
                    forall proto era.
                    ( SL.Era era
                    , MemPack (SL.TxOut era)
                    , IOLike m
                    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
                    , DecShareCBOR (SL.TxOut era)
                    , SL.EraCertState era
                    , Eq (SL.TxOut era)
                    ) =>
                    ( LedgerState
                        -.-> ExceptT (SnapshotFailure (CardanoBlock c)) m
                        :.: ( (,) (Maybe CRC)
                                :.: StateHandle m
                            )
                    )
                      (ShelleyBlock proto era)

                  sf = Fn $ \st@(shelleyLedgerState -> nes) ->
                    Comp $ withExceptT (InitFailureOther . show) $ do
                      (x, y) <- fromSnapshot mkFromSnapshot ds nes
                      pure $ Comp (y, ShelleyStateHandle st x)
                  np =
                    Fn (\bs -> Comp $ pure $ Comp $ (Just initCRC, ByronStateHandle bs))
                      :* sf
                      :* sf
                      :* sf
                      :* sf
                      :* sf
                      :* sf
                      :* sf
                      :* Nil
                  np ::
                    NP
                      ( LedgerState
                          -.-> ExceptT (SnapshotFailure (CardanoBlock c)) m
                          :.: ( (,) (Maybe CRC)
                                  :.: StateHandle m
                              )
                      )
                      (CardanoEras c)
               in np
            )
            (hardForkLedgerStatePerEra ls)
      let crcTables =
            fromMaybe initCRC $
              hcollapse $
                hmap (K . fst . unComp . currentState) $
                  Telescope.tip $
                    getHardForkState ns
          extLedgerSt :: HardForkState (StateHandle m) (CardanoEras c)
          extLedgerSt = hmap (snd . unComp) ns
      let computedCRC = crcOfConcat checksumAsRead crcTables
      Monad.when (computedCRC /= snapshotChecksum snapshotMeta) $
        throwError $
          InitFailureRead $
            ReadSnapshotDataCorruption
      pure (ExtStateHandle (HardForkStateHandle extLedgerSt mkH) hs, pt)

-- | The in-memory backend's 'SnapshotManager'.
--
-- 'listSnapshots' and 'deleteSnapshotIfTemporary' are the standard
-- filesystem-driven implementations. 'takeSnapshot' writes the pure
-- 'ExtLedgerState' first and then delegates to 'takeHandleSnapshot' on
-- the per-era 'TablesHandle' for the on-disk component.
mkSnapshotManager ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  -- | The backend tag to record for Byron-era snapshots. Non-Byron eras
  -- carry the tag returned by their per-era 'takeHandleSnapshot'.
  SnapshotBackend ->
  CodecConfig (CardanoBlock c) ->
  Tracer m (TraceSnapshotEvent (CardanoBlock c)) ->
  SomeHasFS m ->
  SnapshotManager m (CardanoBlock c) (ExtStateHandle m (CardanoBlock c))
mkSnapshotManager byronBackend ccfg snapTracer shfs@(SomeHasFS hasFS) =
  SnapshotManager
    { listSnapshots = defaultListSnapshots shfs
    , deleteSnapshotIfTemporary = defaultDeleteSnapshotIfTemporary shfs snapTracer
    , takeSnapshot = \suffix st ->
        case pointToWithOriginRealPoint (getTip $ extLedgerState st) of
          Origin -> return Nothing
          NotOrigin t -> do
            let number = unSlotNo (realPointSlot t)
                snapshot = DiskSnapshot number suffix
            diskSnapshots <- defaultListSnapshots shfs
            if List.any (== DiskSnapshot number suffix) diskSnapshots
              then
                return Nothing
              else do
                writeSnapshot snapshot st
                return $ Just (snapshot, t)
    }
 where
  writeSnapshot :: DiskSnapshot -> ExtStateHandle m (CardanoBlock c) -> m ()
  writeSnapshot ds st = do
    createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
    crc1 <-
      writeExtLedgerState
        shfs
        (encodeDiskExtLedgerState ccfg)
        (snapshotToStatePath ds)
        (extLedgerState st)
    (crc2, bknd) <-
      fmap hcollapse $
        hsequence' $
          hzipWith
            apFn
            ( let sf ::
                    (Current (StateHandle m) -.-> m :.: K (Maybe CRC, SnapshotBackend)) (ShelleyBlock proto era)
                  sf = Fn $ \(Current _ ss) -> Comp $ K <$> takeHandleSnapshot (stateRefHandle ss) ds
               in (Fn $ \_ -> Comp $ pure $ K (Nothing, byronBackend))
                    :* sf
                    :* sf
                    :* sf
                    :* sf
                    :* sf
                    :* sf
                    :* sf
                    :* Nil
            )
            (Telescope.tip $ getHardForkState $ hardForkStateHandlePerEra $ unExtStateHandle st)
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = bknd
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        , snapshotTablesCodecVersion = TablesCodecVersion1
        }
