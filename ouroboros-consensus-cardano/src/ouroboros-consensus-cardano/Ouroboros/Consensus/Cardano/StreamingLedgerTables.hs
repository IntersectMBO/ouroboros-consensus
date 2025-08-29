{-# LANGUAGE ScopedTypeVariables #-}
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
import Ouroboros.Consensus.Util.StreamingLedgerTables
import System.Directory
import System.FS.API
import System.FS.IO
import System.FilePath as FilePath
import System.IO.Temp
import System.Random

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

fromLMDB :: FilePath -> LMDB.LMDBLimits -> L EmptyMK -> ResourceRegistry IO -> IO (YieldArgs L IO)
fromLMDB fp limits hint reg = do
  let (dbPath, snapName) = splitFileName fp
  tempDir <- getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming_in"
  removePathForcibly lmdbTemp
  _ <-
    allocate
      reg
      (\_ -> System.Directory.createDirectory lmdbTemp)
      (\_ -> removePathForcibly lmdbTemp)
  (_, bs) <-
    allocate
      reg
      ( \_ -> do
          LMDB.newLMDBBackingStore
            nullTracer
            limits
            (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint lmdbTemp)
            (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint dbPath)
            (InitFromCopy hint (mkFsPath [snapName]))
      )
      bsClose
  (_, bsvh) <- allocate reg (\_ -> bsValueHandle bs) bsvhClose
  pure (YieldLMDB 1000 bsvh)

fromLSM ::
  FilePath ->
  String ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (YieldArgs L IO)
fromLSM fp snapName _ reg = do
  (_, SomeHasFSAndBlockIO hasFS blockIO) <- stdMkBlockIOFS fp reg
  salt <- fst . genWord64 <$> newStdGen
  (_, session) <-
    allocate reg (\_ -> openSession nullTracer hasFS blockIO salt (mkFsPath [])) closeSession
  tb <-
    allocate
      reg
      ( \_ ->
          openTableFromSnapshot
            session
            (toSnapshotName snapName)
            (SnapshotLabel $ T.pack "UTxO table")
      )
      closeTable
  YieldLSM 1000 <$> newLSMLedgerTablesHandle nullTracer reg tb

toLMDB ::
  FilePath ->
  LMDB.LMDBLimits ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (SinkArgs L IO)
toLMDB fp limits hint reg = do
  let (snapDir, snapName) = splitFileName fp
  tempDir <- getCanonicalTemporaryDirectory
  let lmdbTemp = tempDir FilePath.</> "lmdb_streaming_out"
  removePathForcibly lmdbTemp
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
            (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint snapDir)
            (InitFromValues (At 0) hint emptyLedgerTables)
      )
      bsClose
  pure $ SinkLMDB 1000 (bsWrite bs) (\h -> bsCopy bs h (mkFsPath [snapName, "tables"]))

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
  String ->
  L EmptyMK ->
  ResourceRegistry IO ->
  IO (SinkArgs L IO)
toLSM fp snapName _ reg = do
  removePathForcibly fp
  System.Directory.createDirectory fp
  (_, SomeHasFSAndBlockIO hasFS blockIO) <- stdMkBlockIOFS fp reg
  salt <- fst . genWord64 <$> newStdGen
  (_, session) <-
    allocate reg (\_ -> newSession nullTracer hasFS blockIO salt (mkFsPath [])) closeSession
  pure (SinkLSM 1000 snapName session)
