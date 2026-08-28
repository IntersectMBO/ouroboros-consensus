{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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

import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Api
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Dijkstra.BlockBody as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Codec.CBOR.Read as CBOR
import Data.Array.Byte (ByteArray)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Short as ShortByteString
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MemPack.Buffer
  ( byteArrayFromShortByteString
  , byteArrayToShortByteString
  )
import Data.Typeable (Typeable)
import Lens.Micro ((.~), (^.))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
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
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , mkBoundedPerasEpochContextWith
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepStateSupportsPeras
  , ConsensusProtocol (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( LedgerPerasCertError
  , ShelleyBlock (..)
  , ShelleyPerasCertCompatibleWithLedger (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger ()
import Ouroboros.Consensus.Ticked (Ticked)

{-------------------------------------------------------------------------------
  StateSupportsPerasEpochContext
-------------------------------------------------------------------------------}

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ShelleyEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ShelleyEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AllegraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AllegraEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto MaryEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: MaryEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AlonzoEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AlonzoEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto BabbageEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: BabbageEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ConwayEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ConwayEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto DijkstraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto DijkstraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ShelleyEra) where
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AllegraEra) where
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto MaryEra) where
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AlonzoEra) where
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto BabbageEra) where
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ConwayEra) where
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
  , ConvertRawHash (ShelleyBlock proto DijkstraEra)
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
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock blk =
    bimap V1.PerasTemporaryCertInBlockError id
      . extractPerasCertFromShelleyBlockBody
      . SL.blockBody
      . shelleyBlockRaw
      $ blk

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
