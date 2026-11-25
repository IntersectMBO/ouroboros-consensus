{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Endorser blocks of a Shelley-based era: the 'BlockSupportsLeios' instance.
module Ouroboros.Consensus.Shelley.Leios () where

import qualified Cardano.Ledger.BaseTypes as SL (strictMaybeToMaybe)
import Cardano.Ledger.Binary
  ( DecCBOR (decCBOR)
  , DecoderError (DecoderErrorCustom)
  , decodeFullAnnotator
  )
import qualified Cardano.Ledger.Block as Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra (applyEndorserBlockNoValidation)
import Cardano.Ledger.Dijkstra.Block.Endorser (EbTransactions (..), EraEndorserBlock (..))
import Cardano.Ledger.Hashes (extractHash)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosLedger
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Sequence.Strict as StrictSeq
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
import Ouroboros.Consensus.Shelley.Eras (LeiosEraDict (..), ShelleyBasedEra (..))
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
      Just dict -> applyEraEndorserBlock dict cfg readTables serialised st

-- | 'applyEndorserBlock' in an era with endorser blocks.
applyEraEndorserBlock ::
  forall m proto era.
  (Monad m, ShelleyCompatible proto era) =>
  LeiosEraDict era ->
  LedgerConfig (ShelleyBlock proto era) ->
  ReadLedgerTables m (ShelleyBlock proto era) ->
  SerialisedEndorserBlock ->
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  m (Either DecoderError (LedgerState (ShelleyBlock proto era) DiffMK))
applyEraEndorserBlock LeiosEraDict cfg readTables serialised st =
  case goDecode serialised of
    Left err -> pure $ Left err
    Right eb -> do
      values <- readTables $ keys eb
      pure . Right $ apply eb values
 where
  goDecode (MkSerialisedEndorserBlock (Serialised ebBytes) txsBytes) = do
    references <- decodeFullAnnotator (Core.eraProtVerLow @era) "EndorserBlock" decCBOR ebBytes
    txs <- forM txsBytes $ \(MkLeiosTx bytes) ->
      decodeFullAnnotator (Core.eraProtVerLow @era) "Tx" decCBOR (Lazy.fromStrict bytes)
    pure $
      mkBasicEndorserBlock
        & referencesEndorserBlockL .~ references
        & transactionsEndorserBlockL .~ EbTransactions (StrictSeq.fromList txs)

  keys eb =
    LedgerTables . KeysMK . coerceSet $
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
