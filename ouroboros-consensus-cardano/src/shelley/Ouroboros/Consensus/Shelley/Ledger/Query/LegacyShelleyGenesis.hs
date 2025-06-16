{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.Shelley.Ledger.Query.LegacyShelleyGenesis
  ( LegacyShelleyGenesis (..)
  , encodeLegacyShelleyGenesis
  , decodeLegacyShelleyGenesis
  ) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Shelley.Genesis
import qualified Data.Text as Text
import Ouroboros.Consensus.Shelley.Ledger.Query.LegacyPParams

-- | The encoding of the PParams changed in node 10.5.
--
-- We can delete this once we cross a HF.
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
