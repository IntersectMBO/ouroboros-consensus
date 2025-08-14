{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-missing-export-lists #-}

module Ouroboros.Consensus.Cardano.StreamingLedgerTables where

import Cardano.Ledger.BaseTypes (BlockNo (..), EpochNo (..), SlotNo (..), WithOrigin (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (ByronEra, Era, eraDecoder, toEraCBOR)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Cardano.Slotting.Time
import Control.Monad.Except
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import qualified Debug.Trace as Debug
import Lens.Micro
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.HardFork.Combinator.State
import Ouroboros.Consensus.HardFork.History.Summary
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import Ouroboros.Consensus.Util.IOLike (bracket)
import Ouroboros.Consensus.Util.StreamingLedgerTables
import Streaming
import System.Directory
import System.FS.API
import System.FS.IO
import System.FilePath as FilePath
import System.IO.Temp
import qualified Test.Cardano.Ledger.Conway.Examples as Conway
import Test.Cardano.Protocol.TPraos.Examples

type L = LedgerState (CardanoBlock StandardCrypto)

fromInMemory ::
  SomeHasFS IO ->
  FsPath ->
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  ( Stream
      ( Of
          ( TxIn L
          , TxOut L
          )
      )
      (ExceptT DeserialiseFailure IO)
      (Stream (Of ByteString) IO ()) ->
    ExceptT DeserialiseFailure IO (Stream (Of ByteString) IO ())
  ) ->
  ExceptT DeserialiseFailure IO ()
fromInMemory shfs fp (HardForkLedgerState (HardForkState idx)) k =
  let
    np ::
      NP
        (Current (Flip LedgerState EmptyMK) -.-> K (ExceptT DeserialiseFailure IO ()))
        (CardanoEras StandardCrypto)
    np =
      (Fn $ const $ K $ pure ())
        :* (Fn $ K . fromEra ShelleyTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AllegraTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra MaryTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra AlonzoTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra BabbageTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra ConwayTxOut . unFlip . currentState)
        :* (Fn $ K . fromEra DijkstraTxOut . unFlip . currentState)
        :* Nil
   in
    hcollapse $ hap np $ Telescope.tip idx
 where
  fromEra ::
    forall proto era.
    ShelleyCompatible proto era =>
    (TxOut (LedgerState (ShelleyBlock proto era)) -> CardanoTxOut StandardCrypto) ->
    LedgerState (ShelleyBlock proto era) EmptyMK ->
    ExceptT DeserialiseFailure IO ()
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
     in yieldInMemoryS
          shfs
          fp
          (eraDecoder @era decodeMemPack)
          (eraDecoder @era $ toCardanoTxOut <$> decShareCBOR certInterns)
          k

toLMDB ::
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  FilePath ->
  Stream
    ( Of
        ( TxIn L
        , TxOut L
        )
    )
    (ExceptT DeserialiseFailure IO)
    (Stream (Of ByteString) IO ()) ->
  ExceptT DeserialiseFailure IO (Stream (Of ByteString) IO ())
toLMDB hint fp s = do
  tempDir <- lift $ getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming"
  lift $ removePathForcibly lmdbTemp
  currDir <- lift $ getCurrentDirectory
  lift $ System.Directory.createDirectory lmdbTemp
  bs <-
    lift $
      LMDB.newLMDBBackingStore
        nullTracer
        limits
        (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint lmdbTemp)
        (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint currDir)
        (InitFromValues (At 0) hint emptyLedgerTables)
  r <- sinkLmdbS @(ExceptT DeserialiseFailure IO) 1000 hint (\s' h d -> lift $ bsWrite bs s' h d) s
  lift $ bsCopy bs hint (mkFsPath (splitDirectories fp))
  lift $ bsClose bs
  pure r

fromLMDB ::
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  FilePath ->
  Stream (Of (TxIn L, TxOut L)) (ExceptT DeserialiseFailure IO) ()
fromLMDB hint fp = do
  tempDir <- lift $ lift $ getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming"
  lift $ lift $ removePathForcibly lmdbTemp
  Debug.traceM "Deleted directory"
  currDir <- lift $ lift $ getCurrentDirectory
  lift $ lift $ System.Directory.createDirectory lmdbTemp
  bs <-
    lift $
      lift $
        LMDB.newLMDBBackingStore
          nullTracer
          limits
          (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint lmdbTemp)
          (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint currDir)
          (InitFromCopy hint (mkFsPath (splitDirectories fp)))
  Debug.traceM "Opened LMDB"
  bsvh <- lift $ lift $ bsValueHandle bs
  Debug.traceM "Opened value handle"
  yieldLmdbS 1000 hint bsvh

toInMemory ::
  L EmptyMK ->
  FilePath ->
  Stream (Of (TxIn L, TxOut L)) (ExceptT DeserialiseFailure IO) () ->
  ExceptT DeserialiseFailure IO ()
toInMemory (HardForkLedgerState (HardForkState idx)) fp s = do
  currDir <- lift $ getCurrentDirectory
  let
    np =
      (Fn $ const $ K $ encOne (Proxy @ByronEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @ShelleyEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @AllegraEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @MaryEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @AlonzoEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @BabbageEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @ConwayEra) currDir)
        :* (Fn $ const $ K $ encOne (Proxy @DijkstraEra) currDir)
        :* Nil
  hcollapse $ hap np $ Telescope.tip idx
 where
  encOne :: forall era. Era era => Proxy era -> FilePath -> ExceptT DeserialiseFailure IO ()
  encOne _ currDir =
    sinkInMemoryS
      (Proxy @L)
      1000
      (toEraCBOR @era . encodeMemPack)
      (toEraCBOR @era . eliminateCardanoTxOut (const encodeMemPack))
      (SomeHasFS $ ioHasFS $ MountPoint currDir)
      fp
      s

limits :: LMDB.LMDBLimits
limits =
  LMDB.LMDBLimits
    { LMDB.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , LMDB.lmdbMaxDatabases = 10
    , LMDB.lmdbMaxReaders = 16
    }

foo ::
  SomeHasFS IO ->
  FsPath ->
  FilePath ->
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  ExceptT DeserialiseFailure IO ()
foo shfs fpFrom fpTo st = fromInMemory shfs fpFrom st (toLMDB st fpTo)

bar ::
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  FilePath ->
  FilePath ->
  ExceptT DeserialiseFailure IO ()
bar st fpFrom fpTo = do
  let s = fromLMDB st fpFrom
  toInMemory st fpTo s

lstate :: L EmptyMK
lstate =
  HardForkLedgerState
    $ HardForkState
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TS (K $ Past (Bound (RelativeTime 0) 0 (EpochNo 0)) (Bound (RelativeTime 0) 0 (EpochNo 0)))
    $ TZ
    $ Current
      (Bound (RelativeTime 0) 0 (EpochNo 0))
    $ Flip
      ShelleyLedgerState
        { shelleyLedgerTip =
            At
              ShelleyTip
                { shelleyTipSlotNo = SlotNo 9
                , shelleyTipBlockNo = BlockNo 3
                , shelleyTipHash =
                    ShelleyHash $ SL.unHashHeader $ pleHashHeader $ ledgerExamplesTPraos Conway.ledgerExamples
                }
        , shelleyLedgerState =
            leNewEpochState $ pleLedgerExamples $ ledgerExamplesTPraos Conway.ledgerExamples
        , shelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
        , shelleyLedgerTables = emptyLedgerTables
        }
