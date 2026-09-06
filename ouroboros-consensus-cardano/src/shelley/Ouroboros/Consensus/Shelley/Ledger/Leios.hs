{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import Cardano.Ledger.Api (Tx)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import qualified Cardano.Ledger.Block as Core
import Cardano.Ledger.Core (TopTx, injectFailure)
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
import qualified Data.ByteString as BS
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
  , EbHash
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
  ( ConsensusConfig (..)
  , Praos
  , PraosCrypto
  , PraosParams (..)
  , PraosState (..)
  , Ticked (..)
  , WhetherToUpperBoundOCERT (..)
  )
import qualified Ouroboros.Consensus.Protocol.Praos as PP
import Ouroboros.Consensus.Protocol.Praos.Header
  ( Header (..)
  , HeaderBody (..)
  , hbLeiosContainsCert
  , hbLeiosEbAnnouncement
  )
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
  , ShelleyBasedEra
  , shelleyLedgerGlobals
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( GenTx (ShelleyTx)
  , mkShelleyTx
  , mkShelleyValidatedTx
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
  (PraosCrypto c, ShelleyCompatible (Praos c) DijkstraEra) =>
  ResolveLeiosBlock (ShelleyBlock (Praos c) DijkstraEra)
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
        -- A missing closure means the EB payload is not in this node's
        -- LeiosDb. On the apply path that is fatal -- chain-sel selected a
        -- cert-RB without its closure, which under the intended parking design
        -- it would not have done yet -- and 'unsafeClosure' still says so
        -- loudly. Reporting it rather than dying here lets the vote path,
        -- where it is merely a reason to abstain, decide for itself.
        pure $ Left $ LeiosClosureMissing ebHash
      Just closureEntries ->
        -- 'traverse' over 'Either' stops at the first undecodable tx: a
        -- closure is only useful whole, so decoding the rest would be work
        -- thrown away.
        pure $ forM closureEntries $ \(txh, bs) ->
          (txh,) . mkShelleyTx <$> deserialiseLeiosTx ebHash txh bs
   where
    deserialiseLeiosTx ebHash txHash bs =
      case decodeFullAnnotator (Core.eraProtVerLow @era) "Leios Tx" decCBOR (BL.fromStrict bs) of
        Left err -> Left $ LeiosClosureTxUndecodable ebHash txHash $ Text.pack (show err)
        Right !tx -> Right tx

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

  headerContainsLeiosCert hdr = hbLeiosContainsCert headerBody
   where
    Header{headerBody} = shelleyHeaderRaw hdr

  headerLeiosAnnouncement hdr = do
    ann <- strictMaybeToMaybe $ hbLeiosEbAnnouncement headerBody
    pure
      ( MkLeiosPoint
          { pointSlotNo = headerBody.hbSlotNo
          , pointEbHash = ann.ebAnnouncementHash
          }
      , ann.ebAnnouncementSize
      )
   where
    Header{headerBody} = shelleyHeaderRaw hdr

  headerElId hdr =
    MkElId
      headerBody.hbSlotNo
      (Crypto.hashToBytesShort . unKeyHash . SL.hashKey $ headerBody.hbVk)
   where
    Header{headerBody} = shelleyHeaderRaw hdr

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
    ann <- strictMaybeToMaybe $ praosStateLeiosAnnouncement st
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
        Just $ MkRbHash $ toRawHash (Proxy @(ShelleyBlock (Praos c) DijkstraEra)) h
