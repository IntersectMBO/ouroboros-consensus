{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Peras support for Shelley.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Shelley.Node.Serialisation' needs some of these
-- instances, but defining them there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras
  ( -- * Exported for testing purposes only
    toOpaqueLedgerPerasCert
  , fromOpaqueLedgerPerasCert
  ) where

import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..), serialize)
import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes (Nonce (NeutralNonce))
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Dijkstra.BlockBody as SL
import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (when)
import Data.Array.Byte (ByteArray)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Short as ShortByteString
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Measure as Measure
import Data.MemPack.Buffer
  ( byteArrayFromShortByteString
  , byteArrayToShortByteString
  )
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Lens.Micro ((.~), (^.))
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , ValidatedPerasCert (..)
  , VoidPerasCert
  , VoidPerasCrypto
  , VoidPerasError
  , VoidPerasVote
  , VoidPerasVotingCommitteeScheme
  , defaultForgePerasCert
  , defaultForgePerasVoteIfEligible
  , defaultVerifyPerasCert
  , defaultVerifyPerasVote
  )
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraPerasCert)
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import Ouroboros.Consensus.HeaderValidation
  ( HeaderState (headerStateChainDep)
  , Ticked (..)
  )
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , HasSizeForBlockBodyInclusion (..)
  , IgnoringOverflow (..)
  , TxLimits (..)
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , dummyPoolDistr
  , mkBoundedPerasEpochContextWith
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafePerasBLSPrivateKeyFromEnv
  )
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.Params
  ( PerasParams
  , dijkstraPerasMaxCertSize
  , mkPerasParams
  )
import Ouroboros.Consensus.Peras.Protocol (PerasProtocol (..))
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.PolyPraos
  ( PolyPraosState (praosStateEpochNonce)
  )
import qualified Ouroboros.Consensus.Protocol.PolyPraos as PolyPraos
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
import Ouroboros.Consensus.Protocol.Praos2 (LeiosCrypto, Praos2)
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( LedgerPerasCertError
  , ShelleyBlock (..)
  , ShelleyPerasCertCompatibleWithLedger (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( LedgerState (..)
  , Ticked (..)
  , shelleyLedgerGenesis
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (AlonzoMeasure (..))

{-------------------------------------------------------------------------------
  StateSupportsPerasEpochContext
-------------------------------------------------------------------------------}

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ShelleyEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ShelleyEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "ShelleyEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "ShelleyEra does not support Peras"

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AllegraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AllegraEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "AllegraEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "AllegraEra does not support Peras"

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto MaryEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: MaryEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "MaryEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "MaryEra does not support Peras"

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AlonzoEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AlonzoEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "AlonzoEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "AlonzoEra does not support Peras"

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto BabbageEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: BabbageEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "BabbageEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "BabbageEra does not support Peras"

instance
  Typeable proto =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ConwayEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger _ = dummyPoolDistr
  getPerasPoolDistrFromTickedLedger _ = dummyPoolDistr
  getPerasNonceFromHeaderState _ = NeutralNonce
  getPerasNonceFromTickedHeaderState _ = NeutralNonce
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ConwayEra does not support Peras"
  mkPerasParamsFromLedger _ _ = Left "ConwayEra does not support Peras"
  mkPerasParamsFromTickedLedger _ _ = Left "ConwayEra does not support Peras"

instance
  ( Typeable (Praos2 c)
  , LeiosCrypto c
  , Dijkstra.DijkstraEraForecast DijkstraEra
  , SL.EraForecast DijkstraEra
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock (Praos2 c) DijkstraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock (Praos2 c) DijkstraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  getPerasPoolDistrFromLedger =
    (^. SL.nesStakePoolDistrG)
      . shelleyLedgerState
  getPerasPoolDistrFromTickedLedger =
    (^. SL.nesStakePoolDistrG)
      . tickedShelleyLedgerState
  getPerasNonceFromHeaderState =
    praosStateEpochNonce
      . headerStateChainDep
  getPerasNonceFromTickedHeaderState =
    praosStateEpochNonce
      . PolyPraos.tickedPraosStateChainDepState
      . tickedHeaderStateChainDep
  mkBoundedPerasEpochContext =
    mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput
  mkPerasParamsFromLedger cfg ShelleyLedgerState{shelleyLedgerState} =
    mkPerasParamsFromNewEpochState cfg shelleyLedgerState
  mkPerasParamsFromTickedLedger cfg TickedShelleyLedgerState{tickedShelleyLedgerState} =
    mkPerasParamsFromNewEpochState cfg tickedShelleyLedgerState

-- | Common implementation of 'mkPerasParamsFromLedger' and
-- 'mkPerasParamsFromTickedLedger' for DijkstraEra.
mkPerasParamsFromNewEpochState ::
  forall c.
  LeiosCrypto c =>
  LedgerConfig (ShelleyBlock (Praos2 c) DijkstraEra) ->
  SL.NewEpochState DijkstraEra ->
  Either String (PerasParams (ShelleyBlock (Praos2 c) DijkstraEra))
mkPerasParamsFromNewEpochState cfg nes =
  case ( getPerasLedgerView (Proxy @(Praos2 c)) $
           Praos.forecastToPolyPraosLedgerView $
             SL.currentForecast nes
       ) of
    Nothing ->
      Left
        "Dijkstra protocol ledger view does not contain Peras parameters"
    Just perasLedgerView ->
      Right $
        mkPerasParams
          (SecurityParam (SL.sgSecurityParam (shelleyLedgerGenesis cfg)))
          (SL.sgActiveSlotCoeff (shelleyLedgerGenesis cfg))
          perasLedgerView

{-------------------------------------------------------------------------------
  HasSizeForBlockBodyInclusion
-------------------------------------------------------------------------------}

instance Typeable blk => HasSizeForBlockBodyInclusion (IgnoringOverflow ByteSize32) (V1.PerasCert blk) where
  getSizeForBlockBodyInclusion cert =
    IgnoringOverflow $ ByteSize32 $ fromIntegral $ LazyByteString.length $ serialize cert

instance
  HasSizeForBlockBodyInclusion (IgnoringOverflow ByteSize32) (V1.PerasCert blk) =>
  HasSizeForBlockBodyInclusion AlonzoMeasure (V1.PerasCert blk)
  where
  getSizeForBlockBodyInclusion = toAlonzo . getSizeForBlockBodyInclusion

instance
  HasSizeForBlockBodyInclusion (IgnoringOverflow ByteSize32) (OneEraPerasCert xs) =>
  HasSizeForBlockBodyInclusion AlonzoMeasure (OneEraPerasCert xs)
  where
  getSizeForBlockBodyInclusion = toAlonzo . getSizeForBlockBodyInclusion

toAlonzo :: (IgnoringOverflow ByteSize32) -> AlonzoMeasure
toAlonzo sz =
  AlonzoMeasure
    { byteSize = sz
    , exUnits = Measure.zero
    }

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto ShelleyEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto ShelleyEra)
  where
  type PerasVote (ShelleyBlock proto ShelleyEra) = VoidPerasVote (ShelleyBlock proto ShelleyEra)
  type PerasCert (ShelleyBlock proto ShelleyEra) = VoidPerasCert (ShelleyBlock proto ShelleyEra)
  type PerasError (ShelleyBlock proto ShelleyEra) = VoidPerasError (ShelleyBlock proto ShelleyEra)
  type PerasCrypto (ShelleyBlock proto ShelleyEra) = VoidPerasCrypto (ShelleyBlock proto ShelleyEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto ShelleyEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto AllegraEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto AllegraEra)
  where
  type PerasVote (ShelleyBlock proto AllegraEra) = VoidPerasVote (ShelleyBlock proto AllegraEra)
  type PerasCert (ShelleyBlock proto AllegraEra) = VoidPerasCert (ShelleyBlock proto AllegraEra)
  type PerasError (ShelleyBlock proto AllegraEra) = VoidPerasError (ShelleyBlock proto AllegraEra)
  type PerasCrypto (ShelleyBlock proto AllegraEra) = VoidPerasCrypto (ShelleyBlock proto AllegraEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto AllegraEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto MaryEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto MaryEra)
  where
  type PerasVote (ShelleyBlock proto MaryEra) = VoidPerasVote (ShelleyBlock proto MaryEra)
  type PerasCert (ShelleyBlock proto MaryEra) = VoidPerasCert (ShelleyBlock proto MaryEra)
  type PerasError (ShelleyBlock proto MaryEra) = VoidPerasError (ShelleyBlock proto MaryEra)
  type PerasCrypto (ShelleyBlock proto MaryEra) = VoidPerasCrypto (ShelleyBlock proto MaryEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto MaryEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto AlonzoEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto AlonzoEra)
  where
  type PerasVote (ShelleyBlock proto AlonzoEra) = VoidPerasVote (ShelleyBlock proto AlonzoEra)
  type PerasCert (ShelleyBlock proto AlonzoEra) = VoidPerasCert (ShelleyBlock proto AlonzoEra)
  type PerasError (ShelleyBlock proto AlonzoEra) = VoidPerasError (ShelleyBlock proto AlonzoEra)
  type PerasCrypto (ShelleyBlock proto AlonzoEra) = VoidPerasCrypto (ShelleyBlock proto AlonzoEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto AlonzoEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto BabbageEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto BabbageEra)
  where
  type PerasVote (ShelleyBlock proto BabbageEra) = VoidPerasVote (ShelleyBlock proto BabbageEra)
  type PerasCert (ShelleyBlock proto BabbageEra) = VoidPerasCert (ShelleyBlock proto BabbageEra)
  type PerasError (ShelleyBlock proto BabbageEra) = VoidPerasError (ShelleyBlock proto BabbageEra)
  type PerasCrypto (ShelleyBlock proto BabbageEra) = VoidPerasCrypto (ShelleyBlock proto BabbageEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto BabbageEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto ConwayEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto ConwayEra)
  where
  type PerasVote (ShelleyBlock proto ConwayEra) = VoidPerasVote (ShelleyBlock proto ConwayEra)
  type PerasCert (ShelleyBlock proto ConwayEra) = VoidPerasCert (ShelleyBlock proto ConwayEra)
  type PerasError (ShelleyBlock proto ConwayEra) = VoidPerasError (ShelleyBlock proto ConwayEra)
  type PerasCrypto (ShelleyBlock proto ConwayEra) = VoidPerasCrypto (ShelleyBlock proto ConwayEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto ConwayEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , TxLimits (ShelleyBlock proto DijkstraEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto DijkstraEra)
  where
  type PerasVote (ShelleyBlock proto DijkstraEra) = V1.PerasVote (ShelleyBlock proto DijkstraEra)
  type PerasCert (ShelleyBlock proto DijkstraEra) = V1.PerasCert (ShelleyBlock proto DijkstraEra)
  type PerasError (ShelleyBlock proto DijkstraEra) = V1.PerasError (ShelleyBlock proto DijkstraEra)
  type PerasCrypto (ShelleyBlock proto DijkstraEra) = BLS.PerasBLSCrypto
  type PerasVotingCommitteeScheme (ShelleyBlock proto DijkstraEra) = V1.PerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert ctx votes = do
    cert <- defaultForgePerasCert ctx votes
    let sizeUpperBound = V1.perasCertSizeUpperBound $ vpcCert cert
    when (sizeUpperBound > dijkstraPerasMaxCertSize) $
      Left $
        V1.PerasCertTooLargeError sizeUpperBound dijkstraPerasMaxCertSize
    pure cert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock blk =
    bimap V1.PerasTemporaryCertInBlockError id
      . extractPerasCertFromShelleyBlockBody
      . SL.blockBody
      . shelleyBlockRaw
      $ blk
  readPerasPrivateKeyFromEnv _ =
    unsafePerasBLSPrivateKeyFromEnv

{-------------------------------------------------------------------------------
  ShelleyPerasCertCompatibleWithLedger
-------------------------------------------------------------------------------}

-- NOTE: these instances will be removed once we have a proper type for Peras
-- certificates in the ledger.

instance ShelleyPerasCertCompatibleWithLedger proto ShelleyEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance ShelleyPerasCertCompatibleWithLedger proto AllegraEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance ShelleyPerasCertCompatibleWithLedger proto MaryEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance ShelleyPerasCertCompatibleWithLedger proto AlonzoEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance ShelleyPerasCertCompatibleWithLedger proto BabbageEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance ShelleyPerasCertCompatibleWithLedger proto ConwayEra where
  extractPerasCertFromShelleyBlockBody _ = Right Nothing
  injectPerasCertIntoShelleyBlockBody _ = id

instance
  Typeable proto =>
  ShelleyPerasCertCompatibleWithLedger proto DijkstraEra
  where
  extractPerasCertFromShelleyBlockBody blockBody =
    case blockBody ^. SL.perasCertBlockBodyL of
      SNothing ->
        Right Nothing
      SJust ledgerCert ->
        case fromOpaqueLedgerPerasCert ledgerCert of
          Left err -> Left err
          Right cert -> Right (Just cert)

  injectPerasCertIntoShelleyBlockBody cert =
    SL.perasCertBlockBodyL .~ SJust (toOpaqueLedgerPerasCert cert)

toOpaqueLedgerPerasCert ::
  Typeable blk =>
  V1.PerasCert blk ->
  SL.PerasCert
toOpaqueLedgerPerasCert =
  SL.PerasCert . toByteArray . toCBOR
 where
  toByteArray :: Encoding -> ByteArray
  toByteArray =
    byteArrayFromShortByteString
      . ShortByteString.toShort
      . CBOR.toStrictByteString

fromOpaqueLedgerPerasCert ::
  Typeable blk =>
  SL.PerasCert ->
  Either LedgerPerasCertError (V1.PerasCert blk)
fromOpaqueLedgerPerasCert (SL.PerasCert byteArray) =
  fromByteArray fromCBOR byteArray
 where
  fromByteArray ::
    (forall s. Decoder s (V1.PerasCert blk)) ->
    ByteArray ->
    Either LedgerPerasCertError (V1.PerasCert blk)
  fromByteArray decoder =
    handleParseErrors
      . CBOR.deserialiseFromBytes decoder
      . LazyByteString.fromStrict
      . ShortByteString.fromShort
      . byteArrayToShortByteString

  handleParseErrors ::
    Either CBOR.DeserialiseFailure (ByteString, a) ->
    Either LedgerPerasCertError a
  handleParseErrors = \case
    Left err -> failure err
    Right (trailing, a)
      | not (LazyByteString.null trailing) -> failure "trailing bytes"
      | otherwise -> pure a
   where
    failure err =
      Left $
        "Failed to deserialize opaque Peras certificate from byte array: "
          <> show err
