{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains copies of older versions of types from Ledger in order
-- to retain backwards-compatibility. Eventually, types likes this should be
-- defined in Ledger instead of here, see
-- <https://github.com/IntersectMBO/cardano-ledger/issues/4415>.
module Ouroboros.Consensus.Shelley.Ledger.Query.Types
  ( IndividualPoolStake (..)
  , PoolDistr (..)
  , fromLedgerIndividualPoolStake
  , fromLedgerPoolDistr
  , LegacyPParams (..)
  , decodeLegacyPParams
  , encodeLegacyPParams
  , LegacyShelleyGenesis (..)
  , encodeLegacyShelleyGenesis
  , decodeLegacyShelleyGenesis
  ) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Allegra
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Conway
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Keys as SL
import Cardano.Ledger.Mary
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.PParams
import qualified Cardano.Ledger.State as SL
import Cardano.Protocol.Crypto (Crypto, VRF)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.Generics (Generic)
import NoThunks.Class

-- | Copy of 'SL.IndividualPoolStake' before
-- <https://github.com/IntersectMBO/cardano-ledger/pull/4324>.
data IndividualPoolStake c = IndividualPoolStake
  { individualPoolStake :: !Rational
  , individualPoolStakeVrf :: !(Hash.Hash HASH (VRF.VerKeyVRF (VRF c)))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

fromLedgerIndividualPoolStake :: SL.IndividualPoolStake -> IndividualPoolStake c
fromLedgerIndividualPoolStake ips =
  IndividualPoolStake
    { individualPoolStake = SL.individualPoolStake ips
    , individualPoolStakeVrf = SL.fromVRFVerKeyHash $ SL.individualPoolStakeVrf ips
    }

instance Crypto c => EncCBOR (IndividualPoolStake c) where
  encCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2
      , encCBOR stake
      , encCBOR vrf
      ]

instance Crypto c => DecCBOR (IndividualPoolStake c) where
  decCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> decCBOR
        <*> decCBOR

-- | Copy of 'SL.PoolDistr' before
-- <https://github.com/IntersectMBO/cardano-ledger/pull/4324>.
newtype PoolDistr c = PoolDistr
  { unPoolDistr :: Map (SL.KeyHash SL.StakePool) (IndividualPoolStake c)
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (EncCBOR, DecCBOR)

fromLedgerPoolDistr :: SL.PoolDistr -> PoolDistr c
fromLedgerPoolDistr pd =
  PoolDistr
    { unPoolDistr = Map.map fromLedgerIndividualPoolStake $ SL.unPoolDistr pd
    }

--------------------------------------------------------------------------------
-- LegacyPParams
--
-- After node 10.5, Ledger started encoding the protocol version in a TList
-- instead of as two contiguous terms. These codecs respect the old version.
--
-- This can be deleted once we cross the Dijkstra HF
newtype LegacyPParams era = LegacyPParams
  { unLegacyPParams :: PParams era
  }

encodeLegacyPParams :: ToCBOR (LegacyPParams era) => PParams era -> Plain.Encoding
encodeLegacyPParams pp = toCBOR (LegacyPParams pp)

decodeLegacyPParams :: FromCBOR (LegacyPParams era) => Plain.Decoder s (PParams era)
decodeLegacyPParams = unLegacyPParams <$> fromCBOR

instance ToCBOR (LegacyPParams ShelleyEra) where
  toCBOR = toEraCBOR @ShelleyEra

instance EncCBOR (LegacyPParams ShelleyEra) where
  encCBOR (LegacyPParams (PParams ShelleyPParams{..})) =
    encode
      ( Rec mkLegacyShelleyPParams
          !> To sppMinFeeA
          !> To sppMinFeeB
          !> To sppMaxBBSize
          !> To sppMaxTxSize
          !> To sppMaxBHSize
          !> To sppKeyDeposit
          !> To sppPoolDeposit
          !> To sppEMax
          !> To sppNOpt
          !> To sppA0
          !> To sppRho
          !> To sppTau
          !> To sppD
          !> To sppExtraEntropy
          !> To (pvMajor sppProtocolVersion)
          !> To (pvMinor sppProtocolVersion)
          !> To sppMinUTxOValue
          !> To sppMinPoolCost
      )
   where
    mkLegacyShelleyPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @ShelleyEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance DecCBOR (LegacyPParams ShelleyEra) where
  decCBOR =
    decode
      ( RecD mkLegacyShelleyPParams
          <! From -- sppMinFeeA
          <! From -- sppMinFeeB
          <! From -- sppMaxBBSize
          <! From -- sppMaxTxSize
          <! From -- sppMaxBHSize
          <! From -- sppKeyDeposit
          <! From -- sppPoolDeposit
          <! From -- sppEMax
          <! From -- sppNOpt
          <! From -- sppA0
          <! From -- sppRho
          <! From -- sppTau
          <! From -- sppD
          <! From -- sppExtraEntropy
          <! From -- (pvMajor sppProtocolVersion)
          <! From -- (pvMinor sppProtocolVersion)
          <! From -- sppMinUTxOValue
          <! From -- sppMinPoolCost
      )
   where
    mkLegacyShelleyPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @ShelleyEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance FromCBOR (LegacyPParams ShelleyEra) where
  fromCBOR = fromEraCBOR @ShelleyEra

instance ToCBOR (LegacyPParams AllegraEra) where
  toCBOR (LegacyPParams (PParams ShelleyPParams{..})) =
    toPlainEncoding (eraProtVerLow @AllegraEra) $
      encode
        ( Rec mkLegacyAllegraPParams
            !> To sppMinFeeA
            !> To sppMinFeeB
            !> To sppMaxBBSize
            !> To sppMaxTxSize
            !> To sppMaxBHSize
            !> To sppKeyDeposit
            !> To sppPoolDeposit
            !> To sppEMax
            !> To sppNOpt
            !> To sppA0
            !> To sppRho
            !> To sppTau
            !> To sppD
            !> To sppExtraEntropy
            !> To (pvMajor sppProtocolVersion)
            !> To (pvMinor sppProtocolVersion)
            !> To sppMinUTxOValue
            !> To sppMinPoolCost
        )
   where
    mkLegacyAllegraPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @AllegraEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance FromCBOR (LegacyPParams AllegraEra) where
  fromCBOR =
    eraDecoder @AllegraEra $
      decode
        ( RecD mkLegacyAllegraPParams
            <! From -- sppMinFeeA
            <! From -- sppMinFeeB
            <! From -- sppMaxBBSize
            <! From -- sppMaxTxSize
            <! From -- sppMaxBHSize
            <! From -- sppKeyDeposit
            <! From -- sppPoolDeposit
            <! From -- sppEMax
            <! From -- sppNOpt
            <! From -- sppA0
            <! From -- sppRho
            <! From -- sppTau
            <! From -- sppD
            <! From -- sppExtraEntropy
            <! From -- (pvMajor sppProtocolVersion)
            <! From -- (pvMinor sppProtocolVersion)
            <! From -- sppMinUTxOValue
            <! From -- sppMinPoolCost
        )
   where
    mkLegacyAllegraPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @AllegraEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance ToCBOR (LegacyPParams MaryEra) where
  toCBOR (LegacyPParams (PParams ShelleyPParams{..})) =
    toPlainEncoding (eraProtVerLow @MaryEra) $
      encode
        ( Rec mkLegacyMaryPParams
            !> To sppMinFeeA
            !> To sppMinFeeB
            !> To sppMaxBBSize
            !> To sppMaxTxSize
            !> To sppMaxBHSize
            !> To sppKeyDeposit
            !> To sppPoolDeposit
            !> To sppEMax
            !> To sppNOpt
            !> To sppA0
            !> To sppRho
            !> To sppTau
            !> To sppD
            !> To sppExtraEntropy
            !> To (pvMajor sppProtocolVersion)
            !> To (pvMinor sppProtocolVersion)
            !> To sppMinUTxOValue
            !> To sppMinPoolCost
        )
   where
    mkLegacyMaryPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @MaryEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance FromCBOR (LegacyPParams MaryEra) where
  fromCBOR =
    eraDecoder @MaryEra $
      decode
        ( RecD mkLegacyMaryPParams
            <! From -- sppMinFeeA
            <! From -- sppMinFeeB
            <! From -- sppMaxBBSize
            <! From -- sppMaxTxSize
            <! From -- sppMaxBHSize
            <! From -- sppKeyDeposit
            <! From -- sppPoolDeposit
            <! From -- sppEMax
            <! From -- sppNOpt
            <! From -- sppA0
            <! From -- sppRho
            <! From -- sppTau
            <! From -- sppD
            <! From -- sppExtraEntropy
            <! From -- (pvMajor sppProtocolVersion)
            <! From -- (pvMinor sppProtocolVersion)
            <! From -- sppMinUTxOValue
            <! From -- sppMinPoolCost
        )
   where
    mkLegacyMaryPParams a b c d e f g h i j k l m n o p q r =
      LegacyPParams $
        PParams $
          ShelleyPParams @Identity @MaryEra a b c d e f g h i j k l m n (ProtVer o p) q r

instance ToCBOR (LegacyPParams AlonzoEra) where
  toCBOR (LegacyPParams (PParams AlonzoPParams{..})) =
    toPlainEncoding (eraProtVerLow @AlonzoEra) $
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
            !> To (pvMajor appProtocolVersion)
            !> To (pvMinor appProtocolVersion)
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
    mkLegacyAlonzoPParams a b c d e f g h i j k l m n o p q r s t u v w x y =
      LegacyPParams $
        PParams $
          AlonzoPParams @Identity @AlonzoEra a b c d e f g h i j k l m n (ProtVer o p) q r s t u v w x y

instance FromCBOR (LegacyPParams AlonzoEra) where
  fromCBOR =
    eraDecoder @AlonzoEra $
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
          <! From -- pvMajor appProtocolVersion
          <! From -- pvMinor
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
    mkLegacyAlonzoPParams a b c d e f g h i j k l m n o p q r s t u v w x y =
      LegacyPParams $
        PParams $
          AlonzoPParams @Identity @AlonzoEra a b c d e f g h i j k l m n (ProtVer o p) q r s t u v w x y

instance ToCBOR (LegacyPParams BabbageEra) where
  toCBOR (LegacyPParams (PParams BabbagePParams{..})) =
    toPlainEncoding (eraProtVerLow @BabbageEra) $
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
            !> To (pvMajor bppProtocolVersion)
            !> To (pvMinor bppProtocolVersion)
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
    mkLegacyBabbagePParams a b c d e f g h i j k l m n o p q r s t u v w =
      LegacyPParams $
        PParams $
          BabbagePParams @Identity @BabbageEra a b c d e f g h i j k l (ProtVer m n) o p q r s t u v w

instance FromCBOR (LegacyPParams BabbageEra) where
  fromCBOR =
    eraDecoder @BabbageEra $
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
          <! From -- pvMajor bppProtocolVersion
          <! From -- pvMinor bppProtocolVersion
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
    mkLegacyBabbagePParams a b c d e f g h i j k l m n o p q r s t u v w =
      LegacyPParams $
        PParams $
          BabbagePParams @Identity @BabbageEra a b c d e f g h i j k l (ProtVer m n) o p q r s t u v w

instance ToCBOR (LegacyPParams ConwayEra) where
  toCBOR = toCBOR . unLegacyPParams

instance FromCBOR (LegacyPParams ConwayEra) where
  fromCBOR = LegacyPParams <$> fromCBOR

--------------------------------------------------------------------------------
-- CompactGenesis
--
-- Because of the change to PParams, the CompactGenesis changed too

newtype LegacyShelleyGenesis = LegacyShelleyGenesis
  { unLegacyShelleyGenesis :: ShelleyGenesis
  }

encodeLegacyShelleyGenesis :: ShelleyGenesis -> Plain.Encoding
encodeLegacyShelleyGenesis pp = toCBOR (LegacyShelleyGenesis pp)

decodeLegacyShelleyGenesis :: Plain.Decoder s ShelleyGenesis
decodeLegacyShelleyGenesis = unLegacyShelleyGenesis <$> fromCBOR

instance ToCBOR LegacyShelleyGenesis where
  toCBOR
    ( LegacyShelleyGenesis
        ShelleyGenesis
          { ..
          }
      ) =
      toPlainEncoding shelleyProtVer $
        encodeListLen 15
          <> encCBOR sgSystemStart
          <> encCBOR sgNetworkMagic
          <> encCBOR sgNetworkId
          <> activeSlotsCoeffEncCBOR sgActiveSlotsCoeff
          <> encCBOR sgSecurityParam
          <> encCBOR (unEpochSize sgEpochLength)
          <> encCBOR sgSlotsPerKESPeriod
          <> encCBOR sgMaxKESEvolutions
          <> encCBOR sgSlotLength
          <> encCBOR sgUpdateQuorum
          <> encCBOR sgMaxLovelaceSupply
          <> encCBOR (LegacyPParams sgProtocolParams)
          <> encCBOR sgGenDelegs
          <> encCBOR sgInitialFunds
          <> encCBOR sgStaking

instance FromCBOR LegacyShelleyGenesis where
  fromCBOR = toPlainDecoder Nothing shelleyProtVer $ do
    decodeRecordNamed "ShelleyGenesis" (const 15) $ do
      sgSystemStart <- decCBOR
      sgNetworkMagic <- decCBOR
      sgNetworkId <- decCBOR
      sgActiveSlotsCoeff <- activeSlotsCoeffDecCBOR
      sgSecurityParam <- decCBOR
      sgEpochLength <- decCBOR
      sgSlotsPerKESPeriod <- decCBOR
      sgMaxKESEvolutions <- decCBOR
      sgSlotLength <- decCBOR
      sgUpdateQuorum <- decCBOR
      sgMaxLovelaceSupply <- decCBOR
      (LegacyPParams sgProtocolParams) <- decCBOR
      sgGenDelegs <- decCBOR
      sgInitialFunds <- decCBOR
      sgStaking <- decCBOR
      pure $
        LegacyShelleyGenesis $
          ShelleyGenesis
            sgSystemStart
            sgNetworkMagic
            sgNetworkId
            sgActiveSlotsCoeff
            sgSecurityParam
            (EpochSize sgEpochLength)
            sgSlotsPerKESPeriod
            sgMaxKESEvolutions
            sgSlotLength
            sgUpdateQuorum
            sgMaxLovelaceSupply
            sgProtocolParams
            sgGenDelegs
            sgInitialFunds
            sgStaking

activeSlotsCoeffEncCBOR :: PositiveUnitInterval -> Encoding
activeSlotsCoeffEncCBOR = enforceEncodingVersion shelleyProtVer . encCBOR . unboundRational

activeSlotsCoeffDecCBOR :: Decoder s PositiveUnitInterval
activeSlotsCoeffDecCBOR = do
  r <- enforceDecoderVersion shelleyProtVer $ decodeRational
  case boundRational r of
    Nothing ->
      cborError $ DecoderErrorCustom "ActiveSlotsCoeff (PositiveUnitInterval)" (Text.pack $ show r)
    Just u -> pure u
