{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Cardano.StreamingLedgerTables where

import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (eraDecoder)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import Control.Monad.Except
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Lens.Micro
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.Cardano.Ledger
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.HardFork.Combinator.State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import Ouroboros.Consensus.Util.IOLike (bracket)
import Ouroboros.Consensus.Util.StreamingLedgerTables
import Streaming
import System.FS.API
import System.FS.IO

fromInMemory ::
  SomeHasFS IO ->
  FsPath ->
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  ( Stream
      ( Of
          ( TxIn (LedgerState (CardanoBlock StandardCrypto))
          , TxOut (LedgerState (CardanoBlock StandardCrypto))
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
      ( Fn $ undefined ::
          (Current (Flip LedgerState EmptyMK) -.-> K (ExceptT DeserialiseFailure IO ())) ByronBlock
      )
        :* (Fn $ K . foo ShelleyTxOut . unFlip . currentState)
        :* (Fn $ K . foo AllegraTxOut . unFlip . currentState)
        :* (Fn $ K . foo MaryTxOut . unFlip . currentState)
        :* (Fn $ K . foo AlonzoTxOut . unFlip . currentState)
        :* (Fn $ K . foo BabbageTxOut . unFlip . currentState)
        :* (Fn $ K . foo ConwayTxOut . unFlip . currentState)
        :* (Fn $ K . foo DijkstraTxOut . unFlip . currentState)
        :* Nil
   in
    hcollapse $ hap np $ Telescope.tip idx
 where
  foo ::
    forall proto era.
    ShelleyCompatible proto era =>
    (TxOut (LedgerState (ShelleyBlock proto era)) -> CardanoTxOut StandardCrypto) ->
    LedgerState (ShelleyBlock proto era) EmptyMK ->
    ExceptT DeserialiseFailure IO ()
  foo toCardanoTxOut st =
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
        ( TxIn (LedgerState (CardanoBlock StandardCrypto))
        , TxOut (LedgerState (CardanoBlock StandardCrypto))
        )
    )
    (ExceptT DeserialiseFailure IO)
    (Stream (Of ByteString) IO ()) ->
  ExceptT DeserialiseFailure IO (Stream (Of ByteString) IO ())
toLMDB hint fp s = do
  bs <-
    lift $
      LMDB.newLMDBBackingStore
        nullTracer
        limits
        (LiveLMDBFS $ SomeHasFS $ ioHasFS $ MountPoint fp)
        (SnapshotsFS $ SomeHasFS $ ioHasFS $ MountPoint fp)
        (InitFromValues (At 0) hint emptyLedgerTables)
  r <- sinkLmdbS @(ExceptT DeserialiseFailure IO) 1000 hint (\s' h d -> lift $ bsWrite bs s' h d) s
  lift $ bsClose bs
  pure r

limits :: LMDB.LMDBLimits
limits = undefined

foo ::
  SomeHasFS IO ->
  FsPath ->
  FilePath ->
  LedgerState (CardanoBlock StandardCrypto) EmptyMK ->
  ExceptT DeserialiseFailure IO ()
foo shfs fpFrom fpTo st = fromInMemory shfs fpFrom st (toLMDB st fpTo)
