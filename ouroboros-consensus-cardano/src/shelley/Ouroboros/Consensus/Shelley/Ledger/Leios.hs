{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- 'SL.unsafeMakeValidated' is deprecated in favour of the ledger's newer
-- 'ValidatedTx', which consensus has not moved to yet (see the same FIXME in
-- "Ouroboros.Consensus.Shelley.Ledger.Mempool"). Suppressed here the way the
-- sibling Shelley modules do.
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Leios () where

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Crypto (hashToBytesShort)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import qualified Cardano.Ledger.Block as Core
import Cardano.Ledger.Core (injectFailure)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import Cardano.Ledger.Hashes (KeyHash (..))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import qualified Cardano.Ledger.Shelley.UTxO as SL
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Control.Monad (foldM, forM)
import Control.Monad.Except (catchError, throwError)
import qualified Control.State.Transition as STS
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Text as Text
import LeiosDemoDb (leiosDbLookupEbClosure)
import LeiosDemoLogic.Announcements.ElBimap (ElId (MkElId))
import LeiosDemoTypes
  ( EbAnnouncement (..)
  , LeiosClosureError (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , RbHash (..)
  , hashLeiosTx
  )
import Lens.Micro ((.~), (^.))
import Ouroboros.Consensus.Block (ChainHash (..), blockPrevHash, toRawHash)
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.SupportsMempool (getTransactionKeySets)
import Ouroboros.Consensus.Ledger.Tables (stowLedgerTables, unstowLedgerTables)
import Ouroboros.Consensus.Protocol.Praos
  ( BasePraosState (..)
  , ConsensusConfig (..)
  , Praos
  , PraosCrypto
  , PraosParams (..)
  , PraosWithLeios
  , Ticked (..)
  , WhetherToUpperBoundOCERT (..)
  )
import qualified Ouroboros.Consensus.Protocol.Praos as PP
import Ouroboros.Consensus.Protocol.Praos.Common (StrictMaybeLeios (..))
import Ouroboros.Consensus.Protocol.Praos.Views (plvPoolDistr)
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
  , shelleyLedgerGlobals
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( GenTx (ShelleyTx)
  , mkShelleyTx
  , mkShelleyValidatedTx
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( pHeaderIssuer
  , pHeaderLeiosContainsCert
  , pHeaderLeiosEbAnnouncement
  , pHeaderSlot
  )
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( OCINStaleness (..)
  , ResolveLeiosBlock (..)
  )

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
  (PraosCrypto c, ShelleyCompatible (PraosWithLeios c) DijkstraEra) =>
  ResolveLeiosBlock (ShelleyBlock (PraosWithLeios c) DijkstraEra)
  where
  -- The on-wire bytes and 'TxHash' a forged EB records for each tx (see
  -- 'forgeLeiosEb'): 'serialize'' the tx, and hash exactly those bytes. Matching
  -- this encoding is what lets the mempool key its txs by the same 'TxHash' an EB
  -- lists, so the body-arrival mempool pull can find them.
  leiosTxBytesOfGenTx (ShelleyTx _ tx) = Just (serialize' tx)
  leiosTxHashOfGenTx (ShelleyTx _ tx) = Just (hashLeiosTx (MkLeiosTx (serialize' tx)))

  resolveLeiosClosure leiosDb ebHash = do
    leiosDbLookupEbClosure leiosDb ebHash >>= \case
      Nothing ->
        pure $ Left $ LeiosClosureMissing ebHash
      Just closureEntries ->
        -- Stops at the first undecodable tx: a closure is only useful whole, so
        -- decoding the rest would be work thrown away.
        pure $ forM closureEntries $ \(txh, bs) ->
          case decodeFullAnnotator (Core.eraProtVerLow @DijkstraEra) "Leios Tx" decCBOR (BL.fromStrict bs) of
            Left err -> Left $ LeiosClosureTxUndecodable ebHash txh $ Text.pack (show err)
            Right !tx -> Right (txh, mkShelleyTx tx)

  -- The ledger's 'Validated' is a bare newtype over the tx, so rebuilding the
  -- token costs only the tx-id hash; 'SL.reapplyTx' derives the state-dependent
  -- annotation itself, so nothing stale rides along.
  assumeValidatedClosureTx (ShelleyTx _ tx) =
    mkShelleyValidatedTx (SL.unsafeMakeValidated tx)

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
        -- Mirror the bookkeeping the Shelley ledger update does on
        -- rb-body txs (see 'shelleyCumulativeTxBytes' update in
        -- 'updateShelleyLedgerState'): the counter has to account for EB
        -- closure txs too, otherwise a cert-RB fresh-apply — where the
        -- on-wire body is empty and the txs come from
        -- 'applyLeiosClosure' — silently drops them from
        -- 'shelleyCumulativeTxBytes'. The immutable-DB replay path
        -- already sums them via 'inlineLeiosClosure' → block body.
        !closureBytes =
          sum (fromIntegral . (^. Core.sizeTxF) <$> innerTxs)
        !lst' =
          stowed
            { shelleyLedgerState = nes'
            , shelleyCumulativeTxBytes =
                shelleyCumulativeTxBytes stowed + closureBytes
            }
    pure (unstowLedgerTables lst')
   where
    globals = shelleyLedgerGlobals cfg
    innerTxs = [tx | ShelleyTx _ tx <- txs]
    stowed = stowLedgerTables lst
    nes = shelleyLedgerState stowed
    env = SL.mkMempoolEnv nes (fromWithOrigin (SlotNo 0) (getTipSlot lst))
    ms0 = SL.mkMempoolState nes

    -- TODO: Ask ledger for an 'applyTxNoValidation' to replace this
    applyOne envv ms !tx =
      fmap fst
        . SL.ruleApplyTxValidation @"LEDGER" STS.ValidateNone globals envv ms
        $ SL.mkStAnnTx
          (SL.epochInfo globals)
          (SL.systemStart globals)
          (envv ^. ledgerPpL)
          (ms ^. SL.utxoG)
          -- The script-decoding cache only saves re-decoding work, and with
          -- ValidateNone no script is evaluated, so start empty.
          mempty
          tx

  inlineLeiosClosure blk txs = do
    blk{shelleyBlockRaw = Core.Block hdr body'}
   where
    body' = body & Core.txSeqBlockBodyL .~ StrictSeq.fromList (fromGenTx <$> txs)

    fromGenTx (ShelleyTx _ tx) = tx

    Core.Block hdr body = shelleyBlockRaw blk

  blockLeiosCert blk =
    strictMaybeToMaybe $ blk.shelleyBlockRaw.blockBody ^. leiosCertBlockBodyL

  headerContainsLeiosCert = pHeaderLeiosContainsCert . shelleyHeaderRaw

  headerLeiosAnnouncement hdr = do
    ann <- strictMaybeToMaybe $ pHeaderLeiosEbAnnouncement raw
    pure
      ( MkLeiosPoint
          { pointSlotNo = pHeaderSlot raw
          , pointEbHash = ann.ebAnnouncementHash
          }
      , ann.ebAnnouncementSize
      )
   where
    raw = shelleyHeaderRaw hdr

  headerElId hdr =
    MkElId
      (pHeaderSlot raw)
      (Crypto.hashToBytesShort . unKeyHash . SL.hashKey $ pHeaderIssuer raw)
   where
    raw = shelleyHeaderRaw hdr

  -- The announcement is validated out-of-context against a (possibly lagging)
  -- immutable tip. We skip the OCERT counter's upper bound
  -- ('DoNotUpperBoundOCERT') — a counter ahead of our recorded view is honest
  -- lag — and we /reinterpret/ its lower-bound failure ('CounterTooSmallOCERT'),
  -- and the absence of any recorded counter ('NoCounterForKeyHashOCERT'), as
  -- 'StaleOCIN' rather than a rejection: under this lagging view either is
  -- honestly explained by the lag. The election proof (VRF) and the signature
  -- (KES + opcert) are still checked in full; any other failure is a genuine
  -- rejection. See 'LeiosDemoLogic.Announcements.Validate.validateAnnouncementHeader'
  -- for why accepting-but-not-propagating a 'StaleOCIN' announcement is safe.
  validateAnnouncementChainDepState cfg hv _slot tcs = do
    -- validate the claimed election
    PP.doValidateVRFSignature
      (praosStateEpochNonce cs)
      pd
      (praosLeaderF prms)
      hv
    -- authenticate the message; the OCIN counter checks report staleness rather
    -- than reject
    (FreshOCIN <$ authenticate) `catchError` \err -> case err of
      PP.CounterTooSmallOCERT{} -> pure StaleOCIN
      PP.NoCounterForKeyHashOCERT{} -> pure StaleOCIN
      _ -> throwError err
   where
    prms = praosParams cfg
    cs = tickedPraosStateChainDepState tcs
    SL.PoolDistr pd _ = plvPoolDistr (tickedPraosStateLedgerView tcs)
    authenticate =
      PP.doValidateKESSignatureWorker
        DoNotUpperBoundOCERT
        (praosMaxKESEvo prms)
        (praosSlotsPerKESPeriod prms)
        pd
        (praosStateOCertCounters cs)
        hv

  protocolStateLeiosAnnouncement st = do
    -- 'SNothingLeios' is unreachable at this extension, so this is total.
    ann <- case praosStateLeiosAnnouncement st of
      SJustLeios mbAnn -> strictMaybeToMaybe mbAnn
    pure
      ( MkLeiosPoint
          { pointSlotNo = fromWithOrigin (SlotNo 0) st.praosStateLastSlot
          , pointEbHash = ann.ebAnnouncementHash
          }
      , ann.ebAnnouncementSize
      )

  -- The announcing RB is this block's parent
  announcingRbHash blk =
    case blockPrevHash blk of
      GenesisHash -> Nothing
      BlockHash h ->
        Just $ MkRbHash $ toRawHash (Proxy @(ShelleyBlock (PraosWithLeios c) DijkstraEra)) h
