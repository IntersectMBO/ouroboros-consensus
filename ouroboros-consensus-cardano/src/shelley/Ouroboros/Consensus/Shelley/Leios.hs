{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Endorser blocks of a Shelley-based era: the 'BlockSupportsLeios' instance.
module Ouroboros.Consensus.Shelley.Leios () where

import qualified Cardano.Ledger.BaseTypes as SL (strictMaybeToMaybe)
import Cardano.Ledger.Binary
  ( Annotator
  , DecCBOR (decCBOR)
  , DecoderError (DecoderErrorCustom)
  , decodeFullAnnotator
  )
import qualified Cardano.Ledger.Block as Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra (DijkstraApplyBlock, applyEndorserBlockNoValidation)
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
import Cardano.Ledger.Dijkstra.Rules (EbPredFailure)
import Cardano.Ledger.Hashes (EraIndependentTx, SafeHash, extractHash)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Control.Monad (forM)
import Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word32)
import qualified GHC.IsList as IsList
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , SerialisedEndorserBlock (..)
  , TxHash
  , TxOffset
  )
import Ouroboros.Consensus.Shelley.Eras (LeiosEraDict (..))
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx, mkShelleyValidatedTx)
import Ouroboros.Consensus.Util (coerceSet)

instance ShelleyCompatible proto era => BlockSupportsLeios (ShelleyBlock proto era) where
  blockLeiosCert ShelleyBlock{shelleyBlockRaw = SL.Block _ blockBody} =
    SL.strictMaybeToMaybe (bodyLeiosCert blockBody)

  ledgerAnnouncedLeiosPoint st =
    toPoint
      <$> SL.strictMaybeToMaybe
        (shelleyLedgerState st ^. SL.nesEsL . SL.esLStateL . SL.lsLastEbAnnouncementL)
   where
    toPoint SL.EbAnnouncement{SL.ebAnnouncement = ann, SL.ebAnnouncementSlotNo = slot} =
      MkLeiosPoint slot (extractHash (Core.ebReferencesAnnouncementHash ann))

  -- Only an era with a 'LeiosEraDict' has endorser blocks.
  applyEndorserBlock cfg readTables serialised st =
    case getLeiosEraDict (Proxy @era) of
      Nothing -> pure . Left $ DecoderErrorCustom "EndorserBlock" "an era without Leios"
      Just LeiosEraDict -> applyEraEndorserBlock cfg readTables serialised st

  validateEndorserBlock cfg readTables lookupTxCache recordTxCache serialised st =
    case getLeiosEraDict (Proxy @era) of
      Nothing -> pure $ Left EndorserBlocksUnsupported
      Just LeiosEraDict ->
        validateEraEndorserBlock cfg readTables lookupTxCache recordTxCache serialised st

-- | Decode an endorser block of an era with endorser blocks.
decodeEndorserBlock ::
  forall era.
  (EraEndorserBlock era, DecCBOR (Annotator (Core.Tx Core.TopTx era))) =>
  SerialisedEndorserBlock ->
  Either DecoderError (EndorserBlock era)
decodeEndorserBlock (MkSerialisedEndorserBlock (Serialised ebBytes) txsBytes) = do
  references <- decodeFullAnnotator (Core.eraProtVerLow @era) "EndorserBlock" decCBOR ebBytes
  txs <- forM txsBytes $ \(MkLeiosTx bytes) ->
    decodeFullAnnotator (Core.eraProtVerLow @era) "Tx" decCBOR (Lazy.fromStrict bytes)
  pure $
    mkBasicEndorserBlock
      & referencesEndorserBlockL .~ references
      & transactionsEndorserBlockL .~ EbTransactions (StrictSeq.fromList txs)

-- | The inputs of the transactions of an endorser block.
endorserBlockKeys ::
  EraEndorserBlock era => EndorserBlock era -> LedgerTables (ShelleyBlock proto era) KeysMK
endorserBlockKeys eb =
  LedgerTables . KeysMK . coerceSet $
    foldMap (^. Core.bodyTxL . Core.allInputsTxBodyF) (transactionsOf eb)

-- | 'applyEndorserBlock' in an era with endorser blocks.
applyEraEndorserBlock ::
  forall m proto era.
  ( Monad m
  , ShelleyCompatible proto era
  , DecCBOR (Annotator (Core.Tx Core.TopTx era))
  , DijkstraApplyBlock (Leios.Header StandardCrypto) era
  , DijkstraEraPParams era
  , Show (EbPredFailure era)
  ) =>
  LedgerConfig (ShelleyBlock proto era) ->
  ReadLedgerTables m (ShelleyBlock proto era) ->
  SerialisedEndorserBlock ->
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  m (Either DecoderError (LedgerState (ShelleyBlock proto era) DiffMK))
applyEraEndorserBlock cfg readTables serialised st =
  case decodeEndorserBlock serialised of
    Left err -> pure $ Left err
    Right eb -> do
      values <- readTables $ endorserBlockKeys eb
      pure . Right $ apply eb values
 where
  apply eb values =
    let before = st `withLedgerTables` values
        stowed = stowLedgerTables before
        after =
          stowed
            { shelleyLedgerState =
                applyEndorserBlockNoValidation @(Leios.Header StandardCrypto)
                  (shelleyLedgerGlobals cfg)
                  (shelleyLedgerState stowed)
                  eb
            }
     in trackingToDiffs $ calculateDifference before $ unstowLedgerTables after

-- | 'validateEndorserBlock' in an era with endorser blocks: apply its
-- transactions to the announcing block's ledger state, reapplying those
-- validated before.
validateEraEndorserBlock ::
  forall m proto era.
  ( Monad m
  , ShelleyCompatible proto era
  , EraEndorserBlock era
  , DecCBOR (Annotator (Core.Tx Core.TopTx era))
  ) =>
  LedgerConfig (ShelleyBlock proto era) ->
  ReadLedgerTables m (ShelleyBlock proto era) ->
  LookupTxCache m ->
  RecordTxCache m ->
  SerialisedEndorserBlock ->
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  m (Either InvalidEndorserBlock ValidationSummary)
validateEraEndorserBlock cfg readTables lookupTxCache recordTxCache serialised st =
  case decodeEndorserBlock serialised of
    Left err -> invalid err
    Right eb
      | missing@(_ : _) <- drop (length (transactionsOf eb)) (referencesOf eb) ->
          invalid $ EndorsedTxsMissing (map fst missing)
      | otherwise -> do
          -- Resolve their input UTxOs
          values <- readTables $ endorserBlockKeys eb
          -- Determine which txs we can just reapply
          let txHashes = map (extractHash . fst) (referencesOf eb)
          decided <-
            zipWith3 decide txHashes (transactionsOf eb)
              <$> lookupTxCache protocolVersion txHashes
          goValidate 0 unticked values Measure.zero (MkValidationSummary 0 0) decided
 where
  globals = shelleyLedgerGlobals cfg

  -- The endorser block's slot is also its announcer's.
  slot = fromWithOrigin (SlotNo 0) (getTipSlot st)

  protocolVersion = SL.pvMajor $ getPParams (shelleyLedgerState st) ^. Core.ppProtocolVersionL

  decide txHash tx validatedBefore
    | validatedBefore = Cached tx
    | otherwise = Novel txHash tx

  -- NOTE Each transaction runs on a state holding only its own inputs, as the
  -- mempool runs it, and its effect is folded back one key at a time. Holding the
  -- whole input table instead makes diffing and merging it after every
  -- transaction quadratic in the endorser block (see 29a98627e9 on the
  -- prototype).
  --
  -- TODO Validate through ledger re-builder once it exists cardano-ledger#6211
  goValidate ::
    TxOffset ->
    TickedLedgerState (ShelleyBlock proto era) EmptyMK ->
    LedgerTables (ShelleyBlock proto era) ValuesMK ->
    TxEbMeasure (ShelleyBlock proto era) ->
    ValidationSummary ->
    [TxToValidate era] ->
    m (Either InvalidEndorserBlock ValidationSummary)
  goValidate off ticked values taken summary = \case
    [] -> pure $ Right summary
    (toValidate : rest) -> do
      let tx = mkShelleyTx (UnsafeMkMempoolTx (txToValidate toValidate))
          keys = getTransactionKeySets tx
          -- The state this tx runs on: the values of its own inputs only.
          stTx = ticked `withLedgerTables` ltliftA2 restrictValuesMK values keys
      case Measure.plus taken <$> measure stTx tx of
        Left err -> invalid $ EndorsedTxInvalid off err
        Right taken'
          | not (taken' Measure.<= capacity) -> invalid $ EndorserBlockTooBig off
          | otherwise -> do
              let continue st' values' summary' =
                    goValidate (off + 1) st' values' taken' summary' rest
              case toValidate of
                Cached tx' -> do
                  -- TODO The ledger documents 'SL.unsafeMakeValidatedTx' as for testing only.
                  let innerSt = tickedShelleyLedgerState (stowLedgerTables stTx)
                      vtx =
                        mkShelleyValidatedTx $
                          SL.unsafeMakeValidatedTx
                            globals
                            (SL.mkMempoolEnv innerSt slot)
                            (SL.mkMempoolState innerSt)
                            tx'
                  case runExcept $ reapplyTx cfg slot vtx stTx of
                    Left err -> invalid $ EndorsedTxInvalid off err
                    Right st' ->
                      continue
                        (forgetLedgerTables st')
                        (ltliftA3 rebase values keys (projectLedgerTables st'))
                        summary{summaryReapplied = summaryReapplied summary + 1}
                Novel txHash _ ->
                  -- 'Intervene' so that a wrong 'isValid' flag fails, not gets rewritten.
                  case runExcept $ applyTx cfg Intervene slot tx stTx of
                    Left err -> invalid $ EndorsedTxInvalid off err
                    Right (st', _vtx) -> do
                      recordTxCache protocolVersion off txHash
                      continue
                        (forgetLedgerTables st')
                        (ltliftA2 applyDelta values (projectLedgerTables st'))
                        summary{summaryValidated = summaryValidated summary + 1}

  -- The announcing block's state as the mempool API takes it, without ticking,
  -- and what an endorser block may hold on it.
  --
  -- TODO Replace with the ledger re-builder once it exists cardano-ledger#6211
  unticked =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = shelleyLedgerTip st
      , tickedShelleyLedgerTransition = shelleyLedgerTransition st
      , tickedShelleyLedgerState = shelleyLedgerState st
      , tickedShelleyLedgerTables = emptyLedgerTables
      }

  capacity = ebCapacityTxMeasure cfg unticked

  measure stTx tx =
    runExcept $
      txEbMeasure (Proxy @(ShelleyBlock proto era))
        <$> (TxMeasure <$> txMeasurePhase1 cfg (forgetLedgerTables stTx) tx <*> txMeasurePhase2 cfg stTx tx)

  invalid :: (Eq e, Show e, Typeable e) => e -> m (Either InvalidEndorserBlock a)
  invalid = pure . Left . InvalidEndorserBlock

-- | After a reapplied tx: what 'reapplyTx' left of the tx's own entries
-- replaces them, so the inputs it consumed are gone and its outputs are new.
rebase :: Ord k => ValuesMK k v -> KeysMK k v -> ValuesMK k v -> ValuesMK k v
rebase (ValuesMK values) (KeysMK keys) (ValuesMK after) =
  ValuesMK (Map.union after (Map.withoutKeys values keys))

-- | A tx's diff folded into the table, one update per changed key.
applyDelta :: Ord k => ValuesMK k v -> DiffMK k v -> ValuesMK k v
applyDelta (ValuesMK m) (DiffMK (Diff.Diff ds)) = ValuesMK (Map.foldlWithKey' step m ds)
 where
  step acc k (Diff.Insert v) = Map.insert k v acc
  step acc k Diff.Delete = Map.delete k acc

-- | A transaction of an endorser block, as validating it needs.
data TxToValidate era
  = -- | In the tx cache, so only the state-dependent checks re-run.
    Cached !(Core.Tx Core.TopTx era)
  | -- | Not in the tx cache, so validated in full and recorded under its hash.
    Novel !TxHash !(Core.Tx Core.TopTx era)

txToValidate :: TxToValidate era -> Core.Tx Core.TopTx era
txToValidate (Cached tx) = tx
txToValidate (Novel _ tx) = tx

referencesOf :: EraEndorserBlock era => EndorserBlock era -> [(SafeHash EraIndependentTx, Word32)]
referencesOf eb = IsList.toList (eb ^. referencesEndorserBlockL . txRefsEbReferencesG)

transactionsOf :: EraEndorserBlock era => EndorserBlock era -> [Core.Tx Core.TopTx era]
transactionsOf eb = toList (unEbTransactions (eb ^. transactionsEndorserBlockL))

-- | The transactions up to the given one take more than an endorser block may.
-- TODO Use ledger types and errors once re-builder exists cardano-ledger#6211
newtype EndorserBlockTooBig = EndorserBlockTooBig TxOffset
  deriving (Eq, Show)

-- | A transaction of an endorser block does not apply.
data EndorsedTxInvalid era
  = EndorsedTxInvalid !TxOffset !(SL.ApplyTxError era)

deriving instance Eq (SL.ApplyTxError era) => Eq (EndorsedTxInvalid era)
deriving instance Show (SL.ApplyTxError era) => Show (EndorsedTxInvalid era)

-- | The node holds no transaction for these references of an endorser block.
newtype EndorsedTxsMissing
  = EndorsedTxsMissing [SafeHash EraIndependentTx]
  deriving (Eq, Show)
