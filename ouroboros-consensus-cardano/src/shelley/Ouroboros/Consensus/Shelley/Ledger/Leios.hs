{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Leios
  ( applyDijkstraLeiosClosureTxs
  , deserialiseLeiosTx
  , resolveDijkstraLeiosClosureTxs
  ) where

import Cardano.Ledger.Api (Tx)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import qualified Cardano.Ledger.Block as Core
import Cardano.Ledger.Core (TopTx)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import qualified Cardano.Ledger.Shelley.UTxO as SL
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Control.Monad (foldM)
import qualified Control.State.Transition as STS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import LeiosDemoDb (LeiosDbConnection, leiosDbQueryCompletedEbByPoint)
import LeiosDemoTypes (EbAnnouncement (..), LeiosPoint (..))
import Lens.Micro ((.~), (^.))
import Ouroboros.Consensus.Ledger.Tables (ValuesMK, stowLedgerTables, unstowLedgerTables)
import Ouroboros.Consensus.Protocol.Praos (Praos, PraosCrypto, PraosState (..))
import Ouroboros.Consensus.Protocol.Praos.Header (Header (..), HeaderBody (..))
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock (..)
  , ShelleyCompatible
  , shelleyHeaderRaw
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger (LedgerState (..), ShelleyBasedEra)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx (ShelleyTx), mkShelleyTx)
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))

{-------------------------------------------------------------------------------
  ResolveLeiosBlock

  Only Dijkstra carries Leios certificates; earlier Shelley-based eras get
  the default no-op instance.
-------------------------------------------------------------------------------}

instance ResolveLeiosBlock (ShelleyBlock (TPraos c) ShelleyEra)
instance ResolveLeiosBlock (ShelleyBlock (TPraos c) AllegraEra)
instance ResolveLeiosBlock (ShelleyBlock (TPraos c) MaryEra)
instance ResolveLeiosBlock (ShelleyBlock (TPraos c) AlonzoEra)
instance ResolveLeiosBlock (ShelleyBlock (Praos c) BabbageEra)
instance ResolveLeiosBlock (ShelleyBlock (Praos c) ConwayEra)

instance
  forall c.
  (PraosCrypto c, ShelleyCompatible (Praos c) DijkstraEra) =>
  ResolveLeiosBlock (ShelleyBlock (Praos c) DijkstraEra)
  where
  resolveLeiosClosure leiosDb point _blk = do
    mAnnouncedEb <-
      leiosDbQueryCompletedEbByPoint
        leiosDb
        point
    case mAnnouncedEb of
      Nothing ->
        pure []
      Just closureEntries ->
        pure $ mkShelleyTx . deserialiseLeiosTx . snd <$> closureEntries

  inlineLeiosClosure blk txs = do
    blk{shelleyBlockRaw = Core.Block hdr body'}
   where
    body' = body & Core.txSeqBlockBodyL .~ StrictSeq.fromList (fromGenTx <$> txs)

    fromGenTx (ShelleyTx _ tx) = tx

    Core.Block hdr body = shelleyBlockRaw blk

  blockLeiosCert blk =
    strictMaybeToMaybe $ blk.shelleyBlockRaw.blockBody ^. leiosCertBlockBodyL

  headerLeiosAnnouncement hdr = do
    ann <- strictMaybeToMaybe headerBody.hbLeiosEbAnnouncement
    pure
      ( MkLeiosPoint
          { pointSlotNo = headerBody.hbSlotNo
          , pointEbHash = ann.ebAnnouncementHash
          }
      , ann.ebAnnouncementSize
      )
   where
    Header{headerBody} = shelleyHeaderRaw hdr

  protocolStateLeiosAnnouncement st = do
    ann <- strictMaybeToMaybe $ praosStateLeiosAnnouncement st
    pure
      ( MkLeiosPoint
          { pointSlotNo = fromWithOrigin (SlotNo 0) st.praosStateLastSlot
          , pointEbHash = ann.ebAnnouncementHash
          }
      , ann.ebAnnouncementSize
      )

-- | Deserialise a transaction supplied as Leios-stored bytes.
deserialiseLeiosTx :: forall era. ShelleyBasedEra era => BS.ByteString -> Tx TopTx era
deserialiseLeiosTx bs =
  case decodeFullAnnotator (Core.eraProtVerLow @era) "Leios Tx" decCBOR (BL.fromStrict bs) of
    Left err -> error $ "Failed to deserialise Leios tx: " <> show err
    Right tx -> tx

-- | Apply an EB closure's transactions onto an /unticked/ Shelley-based
-- ledger state, /without/ validation. Side-steps consensus' Ticked-state
-- mempool API by dropping down to the per-era ledger 'ApplyTx' class
-- ('Cardano.Ledger.Shelley.API.Mempool.applyTxValidation'), which works
-- directly on @LedgerState era@ — leaving us with an unticked state we
-- can hand to 'tickThenApply' for the CertRB.
--
-- We trust the closure: each tx was individually validated when inserted
-- into the LeiosDb, so re-running validation here is redundant and risks
-- spurious failures on UTxO drift. Hence 'SL.ValidateNone'.
--
-- The UTxO must be stowed inside 'NewEpochState' before the fold (the
-- ledger API only sees the in-state UTxO, not the consensus
-- 'shelleyLedgerTables') and unstowed back afterwards.
applyDijkstraLeiosClosureTxs ::
  forall proto.
  ShelleyCompatible proto DijkstraEra =>
  SL.Globals ->
  SlotNo ->
  [Tx TopTx DijkstraEra] ->
  LedgerState (ShelleyBlock proto DijkstraEra) ValuesMK ->
  Either
    (SL.ApplyTxError DijkstraEra)
    (LedgerState (ShelleyBlock proto DijkstraEra) ValuesMK)
applyDijkstraLeiosClosureTxs globals slot txs st = do
  ms' <- foldM (applyOne env) ms0 txs
  let nes' =
        nes
          { SL.nesEs =
              (SL.nesEs nes){SL.esLState = ms'}
          }
      st' = stowed{shelleyLedgerState = nes'}
  pure (unstowLedgerTables st')
 where
  stowed = stowLedgerTables st
  nes = shelleyLedgerState stowed
  env = SL.mkMempoolEnv nes slot
  ms0 = SL.mkMempoolState nes

  applyOne envv ms tx = do
    let stAnnTx =
          SL.mkStAnnTx
            (SL.epochInfo globals)
            (SL.systemStart globals)
            (envv ^. ledgerPpL)
            (ms ^. SL.utxoG)
            tx
    fst <$> SL.applyTxValidation STS.ValidateNone globals envv ms stAnnTx

-- | For a Dijkstra-era CertRB, look up the EB closure that the cert in this
-- block's body attests to and return it as a list of ledger transactions
-- (in EB order). 'Nothing' for non-CertRB blocks.
--
-- This sibling of 'resolveLeiosBlock' returns the closure as raw
-- 'Tx TopTx DijkstraEra' values rather than splicing them into the block
-- body; callers wrap them into 'GenTx' to apply them onto the ticked
-- ledger via 'applyTx' (see 'Ouroboros.Consensus.Cardano.Block').
--
-- Defined here (rather than as a class method) because the GenTx
-- constructor lives in 'Shelley.Ledger.Mempool', and 'Ledger' must not
-- depend on 'Mempool' (cycle).
resolveDijkstraLeiosClosureTxs ::
  forall c m.
  Monad m =>
  LeiosDbConnection m ->
  PraosState ->
  ShelleyBlock (Praos c) DijkstraEra ->
  m (Maybe [Tx TopTx DijkstraEra])
resolveDijkstraLeiosClosureTxs leiosDb praosSt blk =
  case body ^. leiosCertBlockBodyL of
    SNothing -> pure Nothing
    SJust cert ->
      case praosStateLeiosAnnouncement praosSt of
        SNothing ->
          error $
            "resolveDijkstraLeiosClosureTxs: certifying but no parent announcement: "
              <> show cert
        SJust ann -> do
          mAnnouncedEb <-
            leiosDbQueryCompletedEbByPoint
              leiosDb
              MkLeiosPoint
                { pointSlotNo = fromWithOrigin (SlotNo 0) (praosStateLastSlot praosSt)
                , pointEbHash = ebAnnouncementHash ann
                }
          case mAnnouncedEb of
            Nothing ->
              error $
                "resolveDijkstraLeiosClosureTxs: announced EB missing in LeiosDb: "
                  <> show ann
                  <> " at last-slot "
                  <> show (praosStateLastSlot praosSt)
                  <> " (cert: "
                  <> show cert
                  <> ")"
            Just announcedEb ->
              pure . Just $
                fmap (deserialiseLeiosTx @DijkstraEra . snd) announcedEb
 where
  Core.Block _hdr body = shelleyBlockRaw blk
