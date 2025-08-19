{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Cardano.StreamingLedgerTables
  ( fromInMemory
  , fromLSM
  , fromLMDB
  , toLMDB
  , toLSM
  , toInMemory
  ) where

import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (ByronEra, Era, eraDecoder, toEraCBOR)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import qualified Codec.CBOR.Encoding
import Control.ResourceRegistry
import Control.Tracer (nullTracer)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import qualified Data.Text as T
import Lens.Micro
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.StreamingLedgerTables
import System.Directory
import System.FS.API
import System.FS.IO
import System.FilePath as FilePath
import System.IO.Temp

type L = LedgerState (CardanoBlock StandardCrypto)

fromInMemory :: FilePath -> L EmptyMK -> ResourceRegistry IO -> IO (YieldArgs L IO)
fromInMemory fp (HardForkLedgerState (HardForkState idx)) _ =
  let
    np ::
      NP
        (Current (Flip LedgerState EmptyMK) -.-> K (Decoders L))
        (CardanoEras StandardCrypto)
    np =
      (Fn $ const $ K $ error "Byron")
        :* (Fn $ K . fromEra ShelleyTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AllegraTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra MaryTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AlonzoTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra BabbageTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra ConwayTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra DijkstraTxOut . unFlip . currentState)
        :* Nil
   in
    pure $
      YieldInMemory
        (SomeHasFS . ioHasFS)
        fp
        (hcollapse $ hap np $ Telescope.tip idx)
 where
  fromEra ::
    forall proto era.
    ShelleyCompatible proto era =>
    (TxOut (LedgerState (ShelleyBlock proto era)) -> CardanoTxOut StandardCrypto) ->
    LedgerState (ShelleyBlock proto era) EmptyMK ->
    Decoders L
  fromEra toCardanoTxOut st =
    let certInterns =
          internsFromMap $
            shelleyLedgerState st
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
     in Decoders
          (eraDecoder @era decodeMemPack)
          (eraDecoder @era $ toCardanoTxOut <$> decShareCBOR certInterns)

fromLMDB :: FilePath -> L EmptyMK -> ResourceRegistry IO -> IO (YieldArgs L IO)
fromLMDB fp hint reg = do
  tempDir <- getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming"
  removePathForcibly lmdbTemp
  currDir <- getCurrentDirectory
  _ <-
    allocate
      reg
      (\_ -> System.Directory.createDirectory lmdbTemp)
      (\_ -> removePathForcibly lmdbTemp)
  (_, bs) <-
    allocate
      reg
      ( \_ ->
          LMDB.newLMDBBackingStore
            nullTracer
            limits
            (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint lmdbTemp)
            (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint currDir)
            (InitFromCopy hint (mkFsPath (splitDirectories fp)))
      )
      bsClose
  (_, bsvh) <- allocate reg (\_ -> bsValueHandle bs) bsvhClose
  pure (YieldLMDB 1000 bsvh)

limits :: LMDB.LMDBLimits
limits =
  LMDB.LMDBLimits
    { LMDB.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , LMDB.lmdbMaxDatabases = 10
    , LMDB.lmdbMaxReaders = 16
    }

fromLSM ::
  FilePath ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (YieldArgs L IO)
fromLSM fp hint reg = do
  (_, SomeHasFSAndBlockIO hasFS blockIO) <- stdMkBlockIOFS fp reg
  salt <- stdGenSalt
  (_, session) <-
    allocate reg (\_ -> openSession nullTracer hasFS blockIO salt (mkFsPath ["lsm"])) closeSession
  tb <-
    allocate
      reg
      ( \_ ->
          openTableFromSnapshot
            session
            (toSnapshotName $ show $ pointSlot $ Ouroboros.Consensus.Ledger.Abstract.getTip hint)
            (SnapshotLabel $ T.pack $ "UTxO table: " ++ showProxy (Proxy @(LedgerBlock L)))
      )
      closeTable
  YieldLSM 1000 <$> newLSMLedgerTablesHandle nullTracer reg tb

toLMDB ::
  FilePath ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (SinkArgs L IO)
toLMDB fp hint reg = do
  tempDir <- getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming"
  removePathForcibly lmdbTemp
  currDir <- getCurrentDirectory
  _ <-
    allocate reg (\_ -> System.Directory.createDirectory lmdbTemp) (\_ -> removePathForcibly lmdbTemp)
  (_, bs) <-
    allocate
      reg
      ( \_ ->
          LMDB.newLMDBBackingStore
            nullTracer
            limits
            (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint lmdbTemp)
            (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint currDir)
            (InitFromValues (At 0) hint emptyLedgerTables)
      )
      bsClose
  pure $ SinkLMDB 1000 (bsWrite bs) (\h -> bsCopy bs h (mkFsPath (splitDirectories fp)))

toInMemory ::
  FilePath ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (SinkArgs L IO)
toInMemory fp (HardForkLedgerState (HardForkState idx)) _ = do
  currDir <- getCurrentDirectory
  let
    np =
      (Fn $ const $ K $ encOne (Proxy @ByronEra))
        :* (Fn $ const $ K $ encOne (Proxy @ShelleyEra))
        :* (Fn $ const $ K $ encOne (Proxy @AllegraEra))
        :* (Fn $ const $ K $ encOne (Proxy @MaryEra))
        :* (Fn $ const $ K $ encOne (Proxy @AlonzoEra))
        :* (Fn $ const $ K $ encOne (Proxy @BabbageEra))
        :* (Fn $ const $ K $ encOne (Proxy @ConwayEra))
        :* (Fn $ const $ K $ encOne (Proxy @DijkstraEra))
        :* Nil
  pure $
    uncurry
      (SinkInMemory 1000)
      (hcollapse $ hap np $ Telescope.tip idx)
      (SomeHasFS $ ioHasFS $ MountPoint currDir)
      fp
 where
  encOne ::
    forall era.
    Era era =>
    Proxy era ->
    (TxIn L -> Codec.CBOR.Encoding.Encoding, TxOut L -> Codec.CBOR.Encoding.Encoding)
  encOne _ =
    (toEraCBOR @era . encodeMemPack, toEraCBOR @era . eliminateCardanoTxOut (const encodeMemPack))

toLSM ::
  FilePath ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (SinkArgs L IO)
toLSM fp _ reg = do
  removePathForcibly fp
  System.Directory.createDirectory fp
  (_, SomeHasFSAndBlockIO hasFS blockIO) <- stdMkBlockIOFS fp reg
  salt <- stdGenSalt
  (_, session) <-
    allocate reg (\_ -> newSession nullTracer hasFS blockIO salt (mkFsPath ["lsm"])) closeSession
  pure (SinkLSM 1000 session)

-- lstate :: L EmptyMK
-- lstate =
--   HardForkLedgerState
--     $ HardForkState
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
--     $ TZ
--     $ Current
--       (Bound (RelativeTime 0) 0 (EpochNo 0))
--     $ Flip
--       ShelleyLedgerState
--         { shelleyLedgerTip =
--             At
--               ShelleyTip
--                 { shelleyTipSlotNo = SlotNo 9
--                 , shelleyTipBlockNo = BlockNo 3
--                 , shelleyTipHash =
--                     ShelleyHash $ SL.unHashHeader $ pleHashHeader $ ledgerExamplesTPraos Conway.ledgerExamples
--                 }
--         , shelleyLedgerState =
--             leNewEpochState $ pleLedgerExamples $ ledgerExamplesTPraos Conway.ledgerExamples
--         , shelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
--         , shelleyLedgerTables = emptyLedgerTables
--         }
