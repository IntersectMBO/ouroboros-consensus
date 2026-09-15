{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The 'HasAnalysis' instances of the Shelley-based eras. db-analyser's
-- analyses are per-era, so the Cardano instance dispatches to these. There is
-- no Shelley-only db-analyser, hence no 'HasProtocolInfo' instance.
module Cardano.Tools.DBAnalyser.Block.Shelley () where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.RewardUpdate as SL
import Cardano.Tools.DBAnalyser.HasAnalysis
import Data.Foldable as Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Maybe.Strict
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Word (Word64)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Ouroboros.Consensus.Protocol.TPraos ()
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyCompatible
  , shelleyLedgerState
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import Ouroboros.Network.SizeInBytes (SizeInBytes (SizeInBytes))
import TextBuilder (decimal)

-- | Usable for each Shelley-based era
instance
  ( ShelleyCompatible proto era
  , PerEraAnalysis era
  ) =>
  HasAnalysis (ShelleyBlock proto era)
  where
  countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
    SL.Block _ body -> getSum $ foldMap (Sum . countOutputs) (body ^. Core.txSeqBlockBodyL)
   where
    countOutputs :: Core.Tx Core.TopTx era -> Int
    countOutputs tx = length $ tx ^. Core.bodyTxL . Core.outputsTxBodyL

  blockTxSizes blk = case Shelley.shelleyBlockRaw blk of
    SL.Block _ body ->
      toList $
        fmap (SizeInBytes . view Core.sizeTxF) (body ^. Core.txSeqBlockBodyL)

  knownEBBs = const Map.empty

  emitTraces (WithLedgerState _blk lsb lsa) =
    catMaybes
      [ let be = SL.nesEL . shelleyLedgerState $ lsb
            ae = SL.nesEL . shelleyLedgerState $ lsa
         in if be /= ae
              then
                Just $ "EPOCH_START_" <> show ae
              else Nothing
      , let brp = SL.nesRu . shelleyLedgerState $ lsb
            arp = SL.nesRu . shelleyLedgerState $ lsa
         in case (brp, arp) of
              (SNothing, SJust _) -> Just "RWDPULSER_START"
              (SJust (SL.Pulsing _ _), SJust (SL.Complete _)) -> Just "RWDPULSER_COMPLETE"
              (SJust _, SNothing) -> Just "RWDPULSER_RESET"
              (_, _) -> Nothing
      ]

  blockStats blk =
    [ decimal $ length $ blockTxSizes blk
    , decimal $ sum $ blockTxSizes blk
    ]
      ++ [ decimal $ Foldable.foldl' (\acc tx -> acc + f tx) 0 txs
         | f <- maybeToList txExUnitsSteps
         ]
   where
    txs :: StrictSeq (Core.Tx Core.TopTx era)
    txs = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body -> body ^. Core.txSeqBlockBodyL

  -- For the time being we do not support any block application
  -- metrics for Shelley-only eras.
  blockApplicationMetrics = []

class PerEraAnalysis era where
  txExUnitsSteps :: Maybe (Core.Tx Core.TopTx era -> Word64)

instance PerEraAnalysis ShelleyEra where txExUnitsSteps = Nothing
instance PerEraAnalysis AllegraEra where txExUnitsSteps = Nothing
instance PerEraAnalysis MaryEra where txExUnitsSteps = Nothing

instance PerEraAnalysis AlonzoEra where
  txExUnitsSteps = Just $ \tx ->
    let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
     in toEnum $ fromEnum steps

instance PerEraAnalysis BabbageEra where
  txExUnitsSteps = Just $ \tx ->
    let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
     in toEnum $ fromEnum steps

instance PerEraAnalysis ConwayEra where
  txExUnitsSteps = Just $ \tx ->
    let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
     in toEnum $ fromEnum steps

instance PerEraAnalysis DijkstraEra where
  txExUnitsSteps = Just $ \tx ->
    let (Alonzo.ExUnits _mem steps) = Alonzo.totExUnits tx
     in toEnum $ fromEnum steps
