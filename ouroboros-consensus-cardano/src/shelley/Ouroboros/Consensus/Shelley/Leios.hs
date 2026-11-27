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
import Cardano.Ledger.Dijkstra.Block.Endorser.ReBuilder
  ( EbReBuilderResult (..)
  , EraEndorserBlockReBuilder (..)
  )
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams)
import Cardano.Ledger.Dijkstra.Rules (EbPredFailure)
import Cardano.Ledger.Hashes (EraIndependentTx, SafeHash, extractHash)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (toList)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word32)
import qualified GHC.IsList as IsList
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , SerialisedEndorserBlock (..)
  )
import Ouroboros.Consensus.Shelley.Eras (LeiosEraDict (..))
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
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

-- | 'validateEndorserBlock' in an era with endorser blocks: re-build it with
-- the ledger's endorser block re-builder on the announcing block's ledger
-- state, reapplying the transactions validated before.
--
-- The re-builder checks what the EB rule will: the announcement (hash, size,
-- slot), each transaction against its reference (hash, size, position), the
-- limits, and that no transaction is missing.
validateEraEndorserBlock ::
  forall m proto era.
  ( Monad m
  , ShelleyCompatible proto era
  , EraEndorserBlockReBuilder era
  , DecCBOR (Annotator (Core.Tx Core.TopTx era))
  , Eq (EbReBuilderTxError era)
  , Show (EbReBuilderTxError era)
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
    Right eb -> do
      -- The ledger state with the inputs of all the transactions.
      values <- readTables $ endorserBlockKeys eb
      let nes = shelleyLedgerState $ stowLedgerTables (st `withLedgerTables` values)
      case mkInitEbReBuilderState nes (eb ^. referencesEndorserBlockL) of
        Left err -> invalid err
        Right reBuilder -> do
          -- Determine which txs we can just reapply
          let txHashes = map (extractHash . fst) (referencesOf eb)
          validatedBefore <- lookupTxCache protocolVersion txHashes
          go 0 reBuilder (MkValidationSummary 0 0) $
            zip3 txHashes (transactionsOf eb) validatedBefore
 where
  globals = shelleyLedgerGlobals cfg

  -- The endorser block's slot is also its announcer's.
  slot = fromWithOrigin (SlotNo 0) (getTipSlot st)

  protocolVersion = SL.pvMajor $ getPParams (shelleyLedgerState st) ^. Core.ppProtocolVersionL

  go off reBuilder summary = \case
    [] -> case finalizeEbReBuilder reBuilder of
      Left err -> invalid err
      Right _ -> pure $ Right summary
    (txHash, tx, cached) : rest ->
      case snocTxEbReBuilder globals reBuilder (supply reBuilder tx cached) of
        EbReBuilderSuccess reBuilder' _vtx
          | cached ->
              go (off + 1) reBuilder' summary{summaryReapplied = summaryReapplied summary + 1} rest
          | otherwise -> do
              recordTxCache protocolVersion off txHash
              go (off + 1) reBuilder' summary{summaryValidated = summaryValidated summary + 1} rest
        EbReBuilderError err _vtx -> invalid err
        EbReBuilderTxError ix err -> invalid $ EndorsedTxInvalid ix err

  -- The tx cache records only that a tx validated, not its 'SL.ValidatedTx'.
  --
  -- TODO The ledger documents 'SL.unsafeMakeValidatedTx' as for testing only;
  -- the re-builder could take the tx as validated before instead.
  supply reBuilder tx cached
    | cached =
        let nes = getEbReBuilderStateNewEpochState reBuilder
         in Left $ SL.unsafeMakeValidatedTx globals (SL.mkMempoolEnv nes slot) (SL.mkMempoolState nes) tx
    | otherwise = Right tx

  invalid :: (Eq e, Show e, Typeable e) => e -> m (Either InvalidEndorserBlock a)
  invalid = pure . Left . InvalidEndorserBlock

referencesOf :: EraEndorserBlock era => EndorserBlock era -> [(SafeHash EraIndependentTx, Word32)]
referencesOf eb = IsList.toList (eb ^. referencesEndorserBlockL . txRefsEbReferencesG)

transactionsOf :: EraEndorserBlock era => EndorserBlock era -> [Core.Tx Core.TopTx era]
transactionsOf eb = toList (unEbTransactions (eb ^. transactionsEndorserBlockL))

-- | A transaction of an endorser block does not apply.
data EndorsedTxInvalid era
  = EndorsedTxInvalid !SL.TxIx !(EbReBuilderTxError era)

deriving instance Eq (EbReBuilderTxError era) => Eq (EndorsedTxInvalid era)
deriving instance Show (EbReBuilderTxError era) => Show (EndorsedTxInvalid era)
