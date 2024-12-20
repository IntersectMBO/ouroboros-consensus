{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ledger fixed/changed the serialisation of @PParams@ in a
-- backwards-incompatible way in
-- <https://github.com/IntersectMBO/ouroboros-network/pull/4349/>.
--
-- This module contains the legacy serialisation in order to keep compatibility
-- with applications (like cardano-cli or Ogmios) that still use the old
-- serialisation logic. We use the negotiated node-to-client version to detect
-- when the client does not yet support the fixed serialisation.
--
-- This module can be removed once the next HF (Conway) has happened.
module Ouroboros.Consensus.Shelley.Ledger.Query.PParamsLegacyEncoder (
    LegacyPParams (..)
  , decodeLegacyPParams
  , encodeLegacyPParams
  ) where

import           Cardano.Ledger.Allegra
import           Cardano.Ledger.Alonzo
import           Cardano.Ledger.Alonzo.PParams
import           Cardano.Ledger.Babbage
import           Cardano.Ledger.Babbage.PParams
import           Cardano.Ledger.Binary
import           Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import           Cardano.Ledger.Conway
import           Cardano.Ledger.Core
import           Cardano.Ledger.Crypto
import           Cardano.Ledger.Mary
import           Cardano.Ledger.Shelley
import           Data.Functor.Identity

newtype LegacyPParams era = LegacyPParams
  { unLegacyPParams :: PParams era
  }

encodeLegacyPParams :: ToCBOR (LegacyPParams era) => PParams era -> Plain.Encoding
encodeLegacyPParams pp = toCBOR (LegacyPParams pp)

decodeLegacyPParams :: FromCBOR (LegacyPParams era) => Plain.Decoder s (PParams era)
decodeLegacyPParams = unLegacyPParams <$> fromCBOR

instance Crypto c => ToCBOR (LegacyPParams (ShelleyEra c)) where
  toCBOR (LegacyPParams pp) = toCBOR pp

instance Crypto c => FromCBOR (LegacyPParams (ShelleyEra c)) where
  fromCBOR = LegacyPParams <$> fromCBOR

instance Crypto c => ToCBOR (LegacyPParams (MaryEra c)) where
  toCBOR (LegacyPParams pp) = toCBOR pp

instance Crypto c => FromCBOR (LegacyPParams (MaryEra c)) where
  fromCBOR = LegacyPParams <$> fromCBOR

instance Crypto c => ToCBOR (LegacyPParams (AllegraEra c)) where
  toCBOR (LegacyPParams pp) = toCBOR pp

instance Crypto c => FromCBOR (LegacyPParams (AllegraEra c)) where
  fromCBOR = LegacyPParams <$> fromCBOR

instance Crypto c => ToCBOR (LegacyPParams (AlonzoEra c)) where
  toCBOR (LegacyPParams (PParams AlonzoPParams{..})) =
    toPlainEncoding (eraProtVerLow @(AlonzoEra c)) $
      encode
        ( Rec mkLegacyAlonzoPParams
            !> To appMinFeeA
            !> To appMinFeeB
            !> To appMaxBBSize
            !> To appMaxTxSize
            !> To appMaxBHSize
            !> To appKeyDeposit
            !> To appPoolDeposit
            !> To appEMax
            !> To appNOpt
            !> To appA0
            !> To appRho
            !> To appTau
            !> To appD
            !> To appExtraEntropy
            !> E encCBORGroup appProtocolVersion
            !> To appMinPoolCost
            -- new/updated for alonzo
            !> To appCoinsPerUTxOWord
            !> To appCostModels
            !> To appPrices
            !> To appMaxTxExUnits
            !> To appMaxBlockExUnits
            !> To appMaxValSize
            !> To appCollateralPercentage
            !> To appMaxCollateralInputs
        )
    where
      mkLegacyAlonzoPParams a b c d e f g h i j k l m n o p q r s t u v w x =
        LegacyPParams $
          PParams $
            AlonzoPParams @Identity @(AlonzoEra c) a b c d e f g h i j k l m n o p q r s t u v w x

instance Crypto c => FromCBOR (LegacyPParams (AlonzoEra c)) where
  fromCBOR =
    eraDecoder @(AlonzoEra c) $
      decode $
        RecD mkLegacyAlonzoPParams
          <! From -- appMinFeeA
          <! From -- appMinFeeB
          <! From -- appMaxBBSize
          <! From -- appMaxTxSize
          <! From -- appMaxBHSize
          <! From -- appKeyDeposit
          <! From -- appPoolDeposit
          <! From -- appEMax
          <! From -- appNOpt
          <! From -- appA0
          <! From -- appRho
          <! From -- appTau
          <! From -- appD
          <! From -- appExtraEntropy
          <! D decCBORGroup -- appProtocolVersion
          <! From -- appMinPoolCost
          -- new/updated for alonzo
          <! From -- appCoinsPerUTxOWord
          <! From -- appCostModels
          <! From -- appPrices
          <! From -- appMaxTxExUnits
          <! From -- appMaxBlockExUnits
          <! From -- appMaxValSize
          <! From -- appCollateralPercentage
          <! From -- appMaxCollateralInputs
    where
      mkLegacyAlonzoPParams a b c d e f g h i j k l m n o p q r s t u v w x =
        LegacyPParams $
          PParams $
            AlonzoPParams @Identity @(AlonzoEra c) a b c d e f g h i j k l m n o p q r s t u v w x

instance Crypto c => ToCBOR (LegacyPParams (BabbageEra c)) where
  toCBOR (LegacyPParams (PParams BabbagePParams{..})) =
    toPlainEncoding (eraProtVerLow @(BabbageEra c)) $
      encode
        ( Rec mkLegacyBabbagePParams
            !> To bppMinFeeA
            !> To bppMinFeeB
            !> To bppMaxBBSize
            !> To bppMaxTxSize
            !> To bppMaxBHSize
            !> To bppKeyDeposit
            !> To bppPoolDeposit
            !> To bppEMax
            !> To bppNOpt
            !> To bppA0
            !> To bppRho
            !> To bppTau
            !> E encCBORGroup bppProtocolVersion
            !> To bppMinPoolCost
            !> To bppCoinsPerUTxOByte
            !> To bppCostModels
            !> To bppPrices
            !> To bppMaxTxExUnits
            !> To bppMaxBlockExUnits
            !> To bppMaxValSize
            !> To bppCollateralPercentage
            !> To bppMaxCollateralInputs
        )
    where
      mkLegacyBabbagePParams a b c d e f g h i j k l m n o p q r s t u v =
        LegacyPParams $
          PParams $
            BabbagePParams @Identity @(BabbageEra c) a b c d e f g h i j k l m n o p q r s t u v

instance Crypto c => FromCBOR (LegacyPParams (BabbageEra c)) where
  fromCBOR =
    eraDecoder @(BabbageEra c) $
      decode $
        RecD mkLegacyBabbagePParams
          <! From -- bppMinFeeA
          <! From -- bppMinFeeB
          <! From -- bppMaxBBSize
          <! From -- bppMaxTxSize
          <! From -- bppMaxBHSize
          <! From -- bppKeyDeposit
          <! From -- bppPoolDeposit
          <! From -- bppEMax
          <! From -- bppNOpt
          <! From -- bppA0
          <! From -- bppRho
          <! From -- bppTau
          <! D decCBORGroup -- bppProtocolVersion
          <! From -- bppMinPoolCost
          <! From -- bppCoinsPerUTxOByte
          <! From -- bppCostModels
          <! From -- bppPrices
          <! From -- bppMaxTxExUnits
          <! From -- bppMaxBlockExUnits
          <! From -- maxValSize
          <! From -- collateralPercentage
          <! From -- maxCollateralInputs
    where
      mkLegacyBabbagePParams a b c d e f g h i j k l m n o p q r s t u v =
        LegacyPParams $
          PParams $
            BabbagePParams @Identity @(BabbageEra c) a b c d e f g h i j k l m n o p q r s t u v

instance Crypto c => ToCBOR (LegacyPParams (ConwayEra c)) where
  toCBOR (LegacyPParams pp) = toCBOR pp

instance Crypto c => FromCBOR (LegacyPParams (ConwayEra c)) where
  fromCBOR = LegacyPParams <$> fromCBOR
