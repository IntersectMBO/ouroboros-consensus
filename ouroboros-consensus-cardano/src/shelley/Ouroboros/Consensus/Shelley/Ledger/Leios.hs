{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Leios () where

import Cardano.Ledger.Api (Tx)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import qualified Cardano.Ledger.Block as Core
import Cardano.Ledger.Core (TopTx, injectFailure)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import qualified Cardano.Ledger.Shelley.UTxO as SL
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Control.Monad (foldM)
import qualified Control.State.Transition as STS
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Maybe.Strict (strictMaybeToMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import LeiosDemoDb (leiosDbQueryCompletedEbByPoint)
import LeiosDemoTypes (EbAnnouncement (..), LeiosPoint (..))
import Lens.Micro ((.~), (^.))
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.SupportsMempool (getTransactionKeySets)
import Ouroboros.Consensus.Ledger.Tables (stowLedgerTables, unstowLedgerTables)
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
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( LedgerState (..)
  , ShelleyBasedEra
  , shelleyLedgerGlobals
  )
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

  leiosClosureTxKeySets = getTransactionKeySets

  -- Apply an EB closure's transactions onto an /unticked/ Dijkstra-era
  -- ledger state, /without/ validation. Side-steps consensus' Ticked-state
  -- mempool API by dropping down to the per-era ledger LEDGER rule
  -- ('ruleApplyTxValidation' @"LEDGER"'), which works directly on
  -- @LedgerState era@ — leaving us with an unticked state we can hand to
  -- 'tickThenApply' for the CertRB.
  --
  -- The UTxO must be stowed inside 'NewEpochState' before the fold (the
  -- ledger API only sees the in-state UTxO, not the consensus
  -- 'shelleyLedgerTables') and unstowed back afterwards.
  applyLeiosClosure cfg txs lst = do
    ms' <-
      first (SL.BlockTransitionError . fmap injectFailure) $
        foldM (applyOne env) ms0 innerTxs
    let nes' = nes{SL.nesEs = (SL.nesEs nes){SL.esLState = ms'}}
        lst' = stowed{shelleyLedgerState = nes'}
    pure (unstowLedgerTables lst')
   where
    globals = shelleyLedgerGlobals cfg
    innerTxs = [tx | ShelleyTx _ tx <- txs]
    stowed = stowLedgerTables lst
    nes = shelleyLedgerState stowed
    env = SL.mkMempoolEnv nes (fromWithOrigin (SlotNo 0) (getTipSlot lst))
    ms0 = SL.mkMempoolState nes

    applyOne envv ms tx =
      fmap fst
        . SL.ruleApplyTxValidation @"LEDGER" STS.ValidateNone globals envv ms
        $ SL.mkStAnnTx
          (SL.epochInfo globals)
          (SL.systemStart globals)
          (envv ^. ledgerPpL)
          (ms ^. SL.utxoG)
          tx

-- | Deserialise a transaction supplied as Leios-stored bytes.
deserialiseLeiosTx :: forall era. ShelleyBasedEra era => BS.ByteString -> Tx TopTx era
deserialiseLeiosTx bs =
  case decodeFullAnnotator (Core.eraProtVerLow @era) "Leios Tx" decCBOR (BL.fromStrict bs) of
    Left err -> error $ "Failed to deserialise Leios tx: " <> show err
    Right tx -> tx
