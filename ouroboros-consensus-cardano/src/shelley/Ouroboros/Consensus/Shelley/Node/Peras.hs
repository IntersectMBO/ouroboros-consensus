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
-- defining it there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras () where

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
  )
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , mkBoundedPerasEpochContextWith
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafePerasBLSPrivateKeyFromEnv
  )
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

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ShelleyEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AllegraEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto MaryEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AlonzoEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto BabbageEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ConwayEra)

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

  getPerasCertInBlock blk =
    bimap V1.PerasTemporaryCertInBlockError id
      . extractPerasCertFromShelleyBlockBody
      . SL.blockBody
      . shelleyBlockRaw
      $ blk

  readPerasPrivateKeyFromEnv _proxy =
    unsafePerasBLSPrivateKeyFromEnv

{-------------------------------------------------------------------------------
  ShelleyPerasCertCompatibleWithLedger
-------------------------------------------------------------------------------}

instance ShelleyPerasCertCompatibleWithLedger proto ShelleyEra
instance ShelleyPerasCertCompatibleWithLedger proto AllegraEra
instance ShelleyPerasCertCompatibleWithLedger proto MaryEra
instance ShelleyPerasCertCompatibleWithLedger proto AlonzoEra
instance ShelleyPerasCertCompatibleWithLedger proto BabbageEra
instance ShelleyPerasCertCompatibleWithLedger proto ConwayEra

instance
  Typeable proto =>
  ShelleyPerasCertCompatibleWithLedger proto DijkstraEra
  where
  toLedgerPerasCert =
    SL.PerasCert . toByteArray . toCBOR
   where
    toByteArray :: Encoding -> ByteArray
    toByteArray =
      byteArrayFromShortByteString
        . ShortByteString.toShort
        . CBOR.toStrictByteString

  fromLedgerPerasCert (SL.PerasCert byteArray) =
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

  extractPerasCertFromShelleyBlockBody blockBody =
    case blockBody ^. SL.perasCertBlockBodyL of
      SNothing ->
        Right Nothing
      SJust ledgerCert ->
        case fromLedgerPerasCert ledgerCert of
          Left err -> Left err
          Right cert -> Right (Just cert)

  injectPerasCertIntoShelleyBlockBody cert =
    SL.perasCertBlockBodyL .~ SJust (toLedgerPerasCert cert)
