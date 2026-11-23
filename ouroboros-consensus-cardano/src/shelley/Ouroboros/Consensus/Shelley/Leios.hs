{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Endorser blocks of a Shelley-based era: the 'BlockSupportsLeios' instance.
module Ouroboros.Consensus.Shelley.Leios () where

import Cardano.Ledger.Alonzo.Core (ppProtocolVersionL)
import qualified Cardano.Ledger.BaseTypes as L
import Cardano.Ledger.Binary (DecCBOR (..), DecoderError (DecoderErrorCustom), decodeFullAnnotator)
import Cardano.Ledger.Block (EbReferencesAnnouncement (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra (applyEndorserBlockNoValidation)
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import qualified Cardano.Ledger.Hashes as SL
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsLastEbAnnouncementL, nesEsL)
import Cardano.Ledger.State (EbAnnouncement (..))
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosLedger
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Lens.Micro ((&), (.~), (^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Leios.ClosureValidation (validateClosureInMempool)
import qualified Ouroboros.Consensus.Leios.Types as Leios
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( BigEndianTxIn (..)
  , LedgerState (shelleyLedgerState)
  , ShelleyLedgerConfig (shelleyLedgerGlobals)
  , Ticked (tickedShelleyLedgerState)
  , getPParams
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx (..), Validated (..), mkShelleyTx)

-- | Only an era with a 'LeiosEraDict' has endorser blocks, whose transactions
-- are encoded as in a block of the era.
instance ShelleyCompatible proto era => BlockSupportsLeios (ShelleyBlock proto era) where
  blockLeiosCert ShelleyBlock{shelleyBlockRaw = SL.Block _ blockBody} =
    L.strictMaybeToMaybe (bodyLeiosCert blockBody)

  ledgerAnnouncedLeiosPoint st =
    toPoint
      <$> L.strictMaybeToMaybe (shelleyLedgerState st ^. nesEsL . esLStateL . lsLastEbAnnouncementL)
   where
    toPoint EbAnnouncement{ebAnnouncement = ann, ebAnnouncementSlotNo = slot} =
      Leios.MkLeiosPoint slot (SL.extractHash (ebReferencesAnnouncementHash ann))

  applyEndorserBlock cfg readTables serialised st =
    case leiosEraDict @era of
      Left err -> pure (Left err)
      Right dict -> applyEraEndorserBlock dict cfg readTables serialised st

  -- TODO Validate through the ledger's re-builder of an endorser block for
  -- voting, cardano-ledger#6211, once it exists.
  validateLeiosClosure = validateClosureInMempool decodeTx parity assumeValidated
   where
    decodeTx tx = do
      dict <- leiosEraDict @era
      mkShelleyTx . UnsafeMkMempoolTx <$> decodeEndorsedTx dict tx

    parity st = Leios.protVerParity $ getPParams (tickedShelleyLedgerState st) ^. ppProtocolVersionL

    -- What 'SL.reapplyValidatedTx' needs of the state, its annotation of the
    -- transaction, comes from the state the transaction is reapplied to.
    --
    -- TODO The ledger documents 'SL.unsafeMakeValidatedTx' as for testing
    -- only. It should offer this for a transaction validated before.
    assumeValidated cfg slot st (ShelleyTx txid tx) =
      Just . ShelleyValidatedTx txid $
        SL.unsafeMakeValidatedTx
          (shelleyLedgerGlobals cfg)
          (SL.mkMempoolEnv innerSt slot)
          (SL.mkMempoolState innerSt)
          (SL.unwrapMempoolTx tx)
     where
      innerSt = tickedShelleyLedgerState (stowLedgerTables st)

-- | 'applyEndorserBlock' in an era with endorser blocks.
applyEraEndorserBlock ::
  forall m proto era.
  (Monad m, ShelleyCompatible proto era) =>
  LeiosEraDict era ->
  LedgerConfig (ShelleyBlock proto era) ->
  ReadLedgerTables m (ShelleyBlock proto era) ->
  Leios.SerialisedEndorserBlock ->
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  m (Either DecoderError (LedgerState (ShelleyBlock proto era) DiffMK))
applyEraEndorserBlock dict@LeiosEraDict cfg readTables serialised st =
  case goDecode serialised of
    Left err -> pure $ Left err
    Right eb -> do
      values <- readTables $ keys eb
      pure . Right $ apply eb values
 where
  goDecode (Leios.MkSerialisedEndorserBlock (Leios.Serialised ebBytes) txsBytes) = do
    references <-
      decodeFullAnnotator (Core.eraProtVerLow @era) (Text.pack "EndorserBlock") decCBOR ebBytes
    txs <- mapM (decodeEndorsedTx dict) txsBytes
    pure $
      mkBasicEndorserBlock
        & referencesEndorserBlockL .~ references
        & transactionsEndorserBlockL .~ EbTransactions (StrictSeq.fromList txs)

  keys eb =
    LedgerTables . KeysMK . Set.mapMonotonic BigEndianTxIn $
      foldMap
        (^. Core.bodyTxL . Core.allInputsTxBodyF)
        (unEbTransactions (eb ^. transactionsEndorserBlockL))

  apply eb values =
    let before = st `withLedgerTables` values
        stowed = stowLedgerTables before
        after =
          stowed
            { shelleyLedgerState =
                applyEndorserBlockNoValidation @(LeiosLedger.Header StandardCrypto)
                  (shelleyLedgerGlobals cfg)
                  (shelleyLedgerState stowed)
                  eb
            }
     in trackingToDiffs $ calculateDifference before $ unstowLedgerTables after

-- | The era's 'LeiosEraDict', as only such an era has endorser blocks.
leiosEraDict :: forall era. ShelleyBasedEra era => Either DecoderError (LeiosEraDict era)
leiosEraDict =
  maybe
    (Left $ DecoderErrorCustom (Text.pack "EndorserBlock") (Text.pack "an era without Leios"))
    Right
    (getLeiosEraDict (Proxy @era))

-- | A transaction of a closure is encoded as in a block of the era.
decodeEndorsedTx ::
  forall era. LeiosEraDict era -> Leios.LeiosTx -> Either DecoderError (Core.Tx Core.TopTx era)
decodeEndorsedTx LeiosEraDict (Leios.MkLeiosTx bytes) =
  decodeFullAnnotator (Core.eraProtVerLow @era) (Text.pack "Tx") decCBOR (BSL.fromStrict bytes)
