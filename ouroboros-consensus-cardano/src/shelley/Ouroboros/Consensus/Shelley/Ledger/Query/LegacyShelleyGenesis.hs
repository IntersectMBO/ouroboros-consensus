{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.Shelley.Ledger.Query.LegacyShelleyGenesis
  ( LegacyShelleyGenesis (..)
  , encodeLegacyShelleyGenesis
  , decodeLegacyShelleyGenesis
  , encodeShelleyGenesisNoExtraConfig
  , decodeShelleyGenesisWithOptionalExtraConfig
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
            SNothing

-- | Encode 'ShelleyGenesis' with 15 fields, and drop @sgExtraConfig@.
--
-- cardano-ledger-shelley 1.19.0.0 added @sgExtraConfig@ and grew the CBOR record
-- from 15 fields to 16, with no version guard.
-- This codec keeps the 15-field encoding of cardano-ledger-shelley 1.18.
--
-- 'Ouroboros.Consensus.Shelley.Ledger.Config.compactGenesis' erases
-- @sgExtraConfig@, so the query answer never carries a value for it.
--
-- 'encodeLegacyShelleyGenesis' also writes 15 fields, but with the
-- 'LegacyPParams' encoding from before node 10.5.
encodeShelleyGenesisNoExtraConfig :: ShelleyGenesis -> Plain.Encoding
encodeShelleyGenesisNoExtraConfig
  ShelleyGenesis
    { ..
    } =
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
        <> encCBOR sgProtocolParams
        <> encCBOR sgGenDelegs
        <> encCBOR sgInitialFunds
        <> encCBOR sgStaking

-- | Decode 'ShelleyGenesis' from 15 or 16 fields.
--
-- cardano-ledger-shelley 1.19.0.0 and later encode 16 fields.
-- cardano-ledger-shelley 1.18 encodes 15.
-- We accept both.
decodeShelleyGenesisWithOptionalExtraConfig :: Plain.Decoder s ShelleyGenesis
decodeShelleyGenesisWithOptionalExtraConfig = toPlainDecoder Nothing shelleyProtVer $ do
  len <- decodeListLen
  case len of
    15 -> pure ()
    16 -> pure ()
    _ ->
      cborError $
        DecoderErrorCustom "ShelleyGenesis" (Text.pack $ "unexpected record length " <> show len)
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
  sgProtocolParams <- decCBOR
  sgGenDelegs <- decCBOR
  sgInitialFunds <- decCBOR
  sgStaking <- decCBOR
  sgExtraConfig <- if len == 16 then decCBOR else pure SNothing
  pure $
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
      sgExtraConfig

activeSlotsCoeffEncCBOR :: PositiveUnitInterval -> Encoding
activeSlotsCoeffEncCBOR = enforceEncodingVersion shelleyProtVer . encCBOR . unboundRational

activeSlotsCoeffDecCBOR :: Decoder s PositiveUnitInterval
activeSlotsCoeffDecCBOR = do
  r <- enforceDecoderVersion shelleyProtVer $ decodeRational
  case boundRational r of
    Nothing ->
      cborError $ DecoderErrorCustom "ActiveSlotsCoeff (PositiveUnitInterval)" (Text.pack $ show r)
    Just u -> pure u
