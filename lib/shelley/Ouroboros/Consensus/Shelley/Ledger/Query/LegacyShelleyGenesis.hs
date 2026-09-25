{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.Shelley.Ledger.Query.LegacyShelleyGenesis
  ( encodeShelleyGenesisNoExtraConfig
  , decodeShelleyGenesisNoExtraConfig
  ) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Shelley.Genesis
import qualified Data.Text as Text

-- | Encode 'ShelleyGenesis' with 15 fields, and drop @sgExtraConfig@.
--
-- cardano-ledger-shelley 1.19.0.0 added @sgExtraConfig@ and grew the CBOR record
-- from 15 fields to 16, with no version guard.
-- This codec keeps the 15-field encoding of cardano-ledger-shelley 1.18.
--
-- 'Ouroboros.Consensus.Shelley.Ledger.Config.compactGenesis' erases
-- @sgExtraConfig@, so the query answer never carries a value for it.
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

-- | Decode 'ShelleyGenesis' from the 15 fields written by
-- 'encodeShelleyGenesisNoExtraConfig'.
decodeShelleyGenesisNoExtraConfig :: Plain.Decoder s ShelleyGenesis
decodeShelleyGenesisNoExtraConfig = toPlainDecoder Nothing shelleyProtVer $
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
    sgProtocolParams <- decCBOR
    sgGenDelegs <- decCBOR
    sgInitialFunds <- decCBOR
    sgStaking <- decCBOR
    let sgExtraConfig = SNothing
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
