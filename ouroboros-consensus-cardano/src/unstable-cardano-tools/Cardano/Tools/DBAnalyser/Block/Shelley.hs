{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.Sequence.Strict (StrictSeq)
import Data.Word (Word64)
import Lens.Micro ((^.), folded, to, toListOf, SimpleGetter)
import Lens.Micro.Extras (view)
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
import Cardano.Ledger.Api (BabbageEraTxBody(..))

-- | Usable for each Shelley-based era
instance
  ( ShelleyCompatible proto era
  , PerEraAnalysis era
  , HasProtoVer proto 
  , Scriptitude (Ledger.Script era)
  , EraHasName era
  , EraDatum era
  , EraTx era
  ) =>
  HasAnalysis (ShelleyBlock proto era)
  where

  protVer blk = Shelley.shelleyBlockRaw blk & SL.blockHeader & eraProtoVer

  type TxOf (ShelleyBlock proto era) = Ledger.Tx era

  txs = to (Shelley.shelleyBlockRaw @proto @era)  . to SL.blockBody  . Ledger.txSeqBlockBodyL @era . folded

  numInputs tx = length $ toListOf (Core.bodyTxL . Core.inputsTxBodyL) tx
  numOutputs tx = length $ toListOf (Core.bodyTxL . Core.outputsTxBodyL) tx

  referenceInputs = eraReferenceInputs

  datumSize = eraDatumSize

  type WitsOf (ShelleyBlock proto era) = Ledger.TxWits era
  type ScriptType (ShelleyBlock proto era) = Ledger.Script era
  wits = Ledger.witsTxL
  addrWits = Ledger.addrTxWitsL
  scriptWits = Ledger.scriptTxWitsL
  scriptSize = scripty_size

  eraName _ = eraEraName @era

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

class EraTx era where
  eraReferenceInputs :: SimpleGetter (Ledger.Tx era) (Set Ledger.TxIn)

instance {-# OVERLAPPABLE #-} (Ledger.EraTx era, BabbageEraTxBody era) => EraTx era where
  eraReferenceInputs =  Core.bodyTxL . referenceInputsTxBodyL

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
    
class Scriptitude s where
  scripty_size :: s -> Int

instance Scriptitude (Timelock AllegraEra) where
  scripty_size _ = 0 -- dummy
  
instance Scriptitude (Timelock MaryEra) where
  scripty_size _ = 0 -- dummy
  
instance Scriptitude (SL.MultiSig ShelleyEra) where
  scripty_size _ = 0 -- dummy
  
instance {-# OVERLAPPABLE #-} Scriptitude (Alonzo.AlonzoScript era) where
  scripty_size _ = 0 -- dummy
  
instance Scriptitude (Alonzo.AlonzoScript ConwayEra) where
  scripty_size scr = packedByteCount scr

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

instance AlonzoEraTxWits era => EraDatum era where
  eraDatumSize = ByteString.Short.length . getMemoRawBytes . view datsTxWitsL

instance {-# OVERLAPPING #-} EraDatum ShelleyEra where
  eraDatumSize _ = 0 -- Shelley era has no datums

instance {-# OVERLAPPING #-} EraDatum AllegraEra where
  eraDatumSize _ = 0 -- Allegra era has no datums

instance {-# OVERLAPPING #-} EraDatum MaryEra where
  eraDatumSize _ = 0 -- Mary era has no datums

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
