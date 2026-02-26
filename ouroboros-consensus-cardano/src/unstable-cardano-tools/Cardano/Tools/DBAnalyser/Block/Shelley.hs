{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.Tools.DBAnalyser.Block.Shelley
  ( Args (..)
  , ShelleyBlockArgs
  ) where

import qualified Data.ByteString.Short as ByteString.Short
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.BaseTypes as CL (natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.RewardUpdate as SL
import Cardano.Tools.DBAnalyser.HasAnalysis
import qualified Data.Aeson as Aeson
import Data.Foldable as Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Maybe.Strict
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Word (Word64)
import Lens.Micro ((^.), folded, to, toListOf, SimpleGetter, SimpleFold, foldMapOf, has, traversed)
import Lens.Micro.Extras (view)
import Ouroboros.Consensus.Block (blockNo)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyCompatible
  , shelleyLedgerState
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import Ouroboros.Consensus.Shelley.Node
  ( Nonce (..)
  , ProtocolParamsShelleyBased (..)
  , ShelleyGenesis
  , protocolInfoShelley
  )
import Ouroboros.Network.SizeInBytes (SizeInBytes (SizeInBytes))
import TextBuilder (decimal)
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.TxIn as Ledger
import Ouroboros.Consensus.Util.IndexedMemPack
import Cardano.Ledger.Allegra.Scripts
import Data.Text (Text)
import Data.Function ((&))
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ShelleyProtocolHeader)
import Cardano.Protocol.TPraos.BHeader (BHeader(..), BHeaderRaw (..), BHBody (..))
import Cardano.Ledger.MemoBytes (MemoBytes(..), getMemoRawBytes)
import qualified Ouroboros.Consensus.Protocol.Praos
import Cardano.Protocol.Crypto (Crypto)
import Ouroboros.Consensus.Protocol.Praos.Header (Header(..),HeaderBody(..))
import Cardano.Ledger.Api.Tx.Wits
import Data.Set (Set)
import Cardano.Ledger.Api (BabbageEraTxBody(..), BabbageEraTxOut (..), EraTxBody)
import qualified Cardano.Ledger.Conway.TxCert as Conway
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import qualified Cardano.Ledger.Dijkstra.TxCert as Dijkstra
import qualified Cardano.Ledger.State as LState
import Data.Functor.Identity (Identity (..))
import Cardano.Ledger.Coin (Coin(..))
import qualified Cardano.Ledger.Shelley.LedgerState as SL

-- | Usable for each Shelley-based era
instance
  ( ShelleyCompatible proto era
  , PerEraAnalysis era
  , HasProtoVer proto
  , EraScripts (Ledger.Script era)
  , EraHasName era
  , EraDatum era
  , EraTx era
  , EraTxWits era
  , EraTxBody era
  , EraClassifyCert (Core.TxCert era)
  , EraClassifyOutputs era
  )  =>
  HasAnalysis (ShelleyBlock proto era) where

  countTxOutputs blk = case Shelley.shelleyBlockRaw blk of
    SL.Block _ body -> getSum $ foldMap (Sum . countOutputs) (body ^. Core.txSeqBlockBodyL)
   where
    countOutputs :: Core.Tx era -> Int
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
      ++ [ decimal $ Foldable.foldl' (\acc tx -> acc + f tx) 0 the_txs
         | f <- maybeToList txExUnitsSteps
         ]
   where
    the_txs :: StrictSeq (Core.Tx era)
    the_txs = case Shelley.shelleyBlockRaw blk of
      SL.Block _ body -> body ^. Core.txSeqBlockBodyL

  -- For the time being we do not support any block application
  -- metrics for Shelley-only eras.
  blockApplicationMetrics = []

  txFeatures :: WithLedgerState (ShelleyBlock proto era) -> [TxFeatures Identity]
  txFeatures (WithLedgerState blk lsb _lsa) = toListOf (txs . to mkTxFeatures) blk

    where
      inputs :: SimpleFold (Ledger.Tx era) TxIn
      inputs = Core.bodyTxL . Core.inputsTxBodyL . folded

      outputs :: SimpleFold (Ledger.Tx era) (Ledger.TxOut era)
      outputs = Core.bodyTxL . Core.outputsTxBodyL . folded

      certs :: SimpleFold (Ledger.Tx era) (Shelley.TxCert era)
      certs = Core.bodyTxL . Core.certsTxBodyL . folded

      txs = to (Shelley.shelleyBlockRaw @proto @era)
        . to SL.blockBody . Ledger.txSeqBlockBodyL @era . folded

      mkTxFeatures tx =
        MkTxFeatures
            { src_block = Identity $ blockNo blk
            , num_script_wits = Identity $ length $ toListOf (Ledger.witsTxL . Ledger.scriptTxWitsL) tx
            , num_addr_wits = Identity $ length $ toListOf (Ledger.witsTxL . Ledger.addrTxWitsL . folded) tx

            , size_script_wits = Identity $ getSum $
                foldMapOf
                  ( Ledger.witsTxL
                  . Ledger.scriptTxWitsL
                  . folded
                  . to eraScriptSize
                  ) Sum tx

            -- Here defaulting to 0 is part of the logic, as if an input isn't
            -- in utxo_scripts_summary, it means its not a reference to a
            -- script, in which case it contributes 0 bytes to the total size.
            , size_ref_scripts = Identity refScriptSize
            , size_datum = Identity $ tx ^. (Ledger.witsTxL . to eraDatumSize)
            , num_inputs = Identity $ length $ toListOf inputs tx
            -- We shouldn't need the 0 default here. But it's perilous to use
            -- partial functions in analysis as an exception will stop the
            -- analysis altogether. Instead we record missing inputs in
            -- num_abs_inputs.
            , size_inputs = Identity $ getSum $
                foldMapOf
                  ( inputs
                  . to (\txin -> Map.findWithDefault 0 txin utxo_summary)
                  ) Sum tx

            , num_abs_inputs = Identity $ getSum $
                foldMapOf
                  ( inputs
                  . to (\txin -> maybe 1 (const 0) $ Map.lookup txin utxo_summary)
                  ) Sum tx
            , num_outputs = Identity $ length $ toListOf outputs tx
            , num_ref_inputs = Identity $ length $ toListOf eraReferenceInputs tx
            -- We shouldn't need the 0 default here. But it's perilous to use
            -- partial functions in analysis as an exception will stop the
            -- analysis altogether. Instead we record missing inputs in
            -- num_abs_ref_inputs.
            , size_ref_inputs = Identity $ getSum $
                foldMapOf
                  ( eraReferenceInputs
                  . folded
                  . to (\txin -> Map.findWithDefault 0 txin utxo_summary)
                  ) Sum tx
            , num_abs_ref_inputs = Identity $ getSum $
                foldMapOf
                  ( eraReferenceInputs
                  . folded
                  . to (\txin -> maybe 1 (const 0) $ Map.lookup txin utxo_summary)
                  ) Sum tx
            , num_certs = Identity $ length $ toListOf certs tx
            , num_pool_certs = Identity $ length $ toListOf (certs . eraFilterPoolCert Proxy) tx
            , num_gov_certs = Identity $ length $ toListOf (certs . eraFilterGovCert Proxy) tx
            , num_deleg_certs = Identity $ length $ toListOf (certs . eraFilterDelegCert Proxy) tx
            , min_fee = Identity $ fromIntegral $ unCoin $
                Core.getMinFeeTx
                  (view (to shelleyLedgerState . SL.newEpochStateGovStateL . LState.curPParamsGovStateL) lsb)
                  tx
                  refScriptSize

            }
            where
              refScriptSize :: Int
              refScriptSize = getSum
                $ foldMapOf (eraReferenceInputs
                . folded
                . to (\txin -> Map.findWithDefault 0 txin utxo_scripts_summary)) Sum tx

              utxo_summary =
                view (to shelleyLedgerState . LState.utxoL . to LState.unUTxO . to (Map.map packedByteCount)) lsb

              utxo_scripts_summary =
                view (to shelleyLedgerState . LState.utxoL . to LState.unUTxO . to (Map.filter (has eraFilterScriptTxOut)) . to (Map.map packedByteCount)) lsb


class PerEraAnalysis era where
  txExUnitsSteps :: Maybe (Core.Tx era -> Word64)

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

-- | Shelley-era specific
instance HasProtocolInfo (ShelleyBlock (TPraos StandardCrypto) ShelleyEra) where
  data Args (ShelleyBlock (TPraos StandardCrypto) ShelleyEra) = ShelleyBlockArgs
    { configFileShelley :: FilePath
    , initialNonce :: Nonce
    }
    deriving Show

  mkProtocolInfo ShelleyBlockArgs{configFileShelley, initialNonce} = do
    config <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' configFileShelley
    return $ mkShelleyProtocolInfo config initialNonce

type ShelleyBlockArgs = Args (ShelleyBlock (TPraos StandardCrypto) ShelleyEra)

mkShelleyProtocolInfo ::
  ShelleyGenesis ->
  Nonce ->
  ProtocolInfo (ShelleyBlock (TPraos StandardCrypto) ShelleyEra)
mkShelleyProtocolInfo genesis initialNonce =
  fst $
    protocolInfoShelley @IO
      genesis
      ProtocolParamsShelleyBased
        { shelleyBasedInitialNonce = initialNonce
        , shelleyBasedLeaderCredentials = []
        }
      (SL.ProtVer (CL.natVersion @2) 0)

-- | From here on is the implementation of 'HasFeatures' for the "dump features"
-- analysis. This uses multiple type classes to dispatch on the era in different
-- ways.
instance
  ( ShelleyCompatible proto era
  , HasProtoVer proto
  , EraHasName era
  , EraTxBody era
  ) =>
  HasFeatures (ShelleyBlock proto era) where
  protVer blk = Shelley.shelleyBlockRaw blk & SL.blockHeader & eraProtoVer
  eraName _ = eraEraName @era

class EraClassifyOutputs era where
  eraFilterScriptTxOut :: SimpleFold (Ledger.TxOut era) (Ledger.Script era)

defaultEraFilterScriptTxOut :: BabbageEraTxOut era => SimpleFold (Ledger.TxOut era) (Ledger.Script era)
defaultEraFilterScriptTxOut = referenceScriptTxOutL . traversed

instance EraClassifyOutputs AlonzoEra where
  eraFilterScriptTxOut = mempty

instance EraClassifyOutputs BabbageEra where
  eraFilterScriptTxOut = defaultEraFilterScriptTxOut

instance EraClassifyOutputs ConwayEra where
  eraFilterScriptTxOut = defaultEraFilterScriptTxOut

instance EraClassifyOutputs DijkstraEra where
  eraFilterScriptTxOut = defaultEraFilterScriptTxOut

instance EraClassifyOutputs ShelleyEra where
  eraFilterScriptTxOut = mempty

instance EraClassifyOutputs AllegraEra where
  eraFilterScriptTxOut = mempty

instance EraClassifyOutputs MaryEra where
  eraFilterScriptTxOut = mempty

class EraClassifyCert cert where
  eraFilterPoolCert :: proxy cert -> SimpleFold cert cert
  eraFilterGovCert :: proxy cert -> SimpleFold cert cert
  eraFilterDelegCert :: proxy cert -> SimpleFold cert cert

instance EraClassifyCert (Conway.ConwayTxCert era) where
  eraFilterPoolCert _ inner cert@(Conway.ConwayTxCertPool _) = inner cert
  eraFilterPoolCert _ _ _ = mempty

  eraFilterGovCert _ inner cert@(Conway.ConwayTxCertGov _) = inner cert
  eraFilterGovCert _ _ _ = mempty

  eraFilterDelegCert _ inner cert@(Conway.ConwayTxCertDeleg _) = inner cert
  eraFilterDelegCert _ _ _ = mempty

instance EraClassifyCert (Shelley.ShelleyTxCert era) where
  eraFilterPoolCert _ inner cert@(Shelley.ShelleyTxCertPool _) = inner cert
  eraFilterPoolCert _ _ _ = mempty

  eraFilterGovCert _ _ _ = mempty

  eraFilterDelegCert _ inner cert@(Shelley.ShelleyTxCertDelegCert _) = inner cert
  eraFilterDelegCert _ inner cert@(Shelley.ShelleyTxCertGenesisDeleg _) = inner cert
  eraFilterDelegCert _ _ _ = mempty

instance EraClassifyCert (Dijkstra.DijkstraTxCert era) where
  eraFilterPoolCert _ inner cert@(Dijkstra.DijkstraTxCertPool _) = inner cert
  eraFilterPoolCert _ _ _ = mempty
  eraFilterGovCert _ inner cert@(Dijkstra.DijkstraTxCertGov _) = inner cert
  eraFilterGovCert _ _ _ = mempty
  eraFilterDelegCert _ inner cert@(Dijkstra.DijkstraTxCertDeleg _) = inner cert
  eraFilterDelegCert _ _ _ = mempty

-- | This is a rather prototypical of these class we're using to extract
-- features. Frequently there are type classes to access this feature, but it's
-- only implemented from the era where the feature is available on. So type
-- classes like 'EraTx' play the role of a wrapper over the appropriate classes
-- which manifests in a default implementation for when the appropriate class is
-- defined. Then the eras where the feature isn't available get a zero-ing
-- implementation.
class EraTx era where
  eraReferenceInputs :: SimpleGetter (Ledger.Tx era) (Set Ledger.TxIn)

eraReferenceInputsDefault :: (Ledger.EraTx era, BabbageEraTxBody era) => SimpleGetter (Ledger.Tx era) (Set Ledger.TxIn)
eraReferenceInputsDefault = Core.bodyTxL . referenceInputsTxBodyL

instance EraTx BabbageEra where
  eraReferenceInputs = eraReferenceInputsDefault

instance EraTx ConwayEra where
  eraReferenceInputs = eraReferenceInputsDefault

instance EraTx DijkstraEra where
  eraReferenceInputs = eraReferenceInputsDefault

instance EraTx ShelleyEra where
  eraReferenceInputs = to (const mempty)

instance EraTx AllegraEra where
  eraReferenceInputs = to (const mempty)

instance EraTx MaryEra where
  eraReferenceInputs = to (const mempty)

instance EraTx AlonzoEra where
  eraReferenceInputs = to (const mempty)

class HasProtoVer proto where
  eraProtoVer :: ShelleyProtocolHeader proto -> SL.ProtVer

instance HasProtoVer (Ouroboros.Consensus.Protocol.TPraos.TPraos c) where
  eraProtoVer blk = blk & (\ (BHeaderConstr h ) -> h) & (\ (Memo h _) -> h) & bhrBody & bprotver

instance Crypto c => HasProtoVer (Ouroboros.Consensus.Protocol.Praos.Praos c) where
  eraProtoVer blk = blk & headerBody & hbProtVer

class EraScripts s where
  eraScriptSize :: s -> Int

instance EraScripts (Timelock AllegraEra) where
  eraScriptSize _ = 0 -- dummy

instance EraScripts (Timelock MaryEra) where
  eraScriptSize _ = 0 -- dummy

instance EraScripts (SL.MultiSig ShelleyEra) where
  eraScriptSize _ = 0 -- dummy

instance MemPack (Alonzo.AlonzoScript era) => EraScripts (Alonzo.AlonzoScript era) where
  eraScriptSize scr = packedByteCount scr

class EraHasName era where
  eraEraName :: Text

instance EraHasName ShelleyEra where
  eraEraName = "Shelley"

instance EraHasName AllegraEra where
  eraEraName = "Allegra"

instance EraHasName MaryEra where
  eraEraName = "Mary"

instance EraHasName AlonzoEra where
  eraEraName = "Alonzo"

instance EraHasName BabbageEra where
  eraEraName = "Babbage"

instance EraHasName ConwayEra where
  eraEraName = "Conway"

instance EraHasName DijkstraEra where
  eraEraName = "Dijkstra"

class EraDatum era where
  eraDatumSize :: Ledger.TxWits era -> Int

eraDatumSizeDefault :: AlonzoEraTxWits era => Ledger.TxWits era -> Int
eraDatumSizeDefault = ByteString.Short.length . getMemoRawBytes . view datsTxWitsL

instance EraDatum AlonzoEra where
  eraDatumSize = eraDatumSizeDefault

instance EraDatum BabbageEra where
  eraDatumSize = eraDatumSizeDefault

instance EraDatum ConwayEra where
  eraDatumSize = eraDatumSizeDefault

instance EraDatum DijkstraEra where
  eraDatumSize = eraDatumSizeDefault

instance EraDatum ShelleyEra where
  eraDatumSize _ = 0 -- Shelley era has no datums

instance EraDatum AllegraEra where
  eraDatumSize _ = 0 -- Allegra era has no datums

instance EraDatum MaryEra where
  eraDatumSize _ = 0 -- Mary era has no datums
