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
-- 'Ouroboros.Consensus.Shelley.Node.Serialisation' needs these instances, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras () where

import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Api
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Dijkstra.BlockBody as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Codec.CBOR.Read as CBOR
import Data.Array.Byte (ByteArray)
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
  ( LedgerPerasCertError (..)
  , ShelleyBlock (..)
  , ShelleyPerasCertCompatibleWithLedger (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger ()
import Ouroboros.Consensus.Ticked (Ticked)

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
    let blockBody = SL.blockBody (shelleyBlockRaw blk)
     in case extractPerasCertFromShelleyBlockBody blockBody of
          Left _err ->
            -- NOTE: for now, we just ignore any conversion error between the
            -- (opaque) Peras certificate stored in the block body and the one
            -- expected here. This is to avoid propagating errors cases caused by
            -- an implementation detail that will eventually disappear when Ledger
            -- becomes aware of the Peras types used by Consensus. Until then,
            -- discarding invalid Peras certificates should be safe enough here.
            Right Nothing
          Right mbCert ->
            Right mbCert

  readPerasPrivateKeyFromEnv _proxy =
    unsafePerasBLSPrivateKeyFromEnv

{-------------------------------------------------------------------------------
  StateSupportsPerasEpochContext
-------------------------------------------------------------------------------}

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)

instance
  ( Typeable proto
  , ConvertRawHash (ShelleyBlock proto DijkstraEra)
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto DijkstraEra)
  where
  mkBoundedPerasEpochContext =
    mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput

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
          LedgerPerasCertError $
            "Failed to deserialize opaque Peras certificate from byte array: "
              <> show err

  extractPerasCertFromShelleyBlockBody blockBody =
    case blockBody ^. SL.perasCertBlockBodyL of
      SNothing ->
        Right Nothing
      SJust ledgerCert ->
        case fromLedgerPerasCert ledgerCert of
          Left err ->
            Left err
          Right cert ->
            Right (Just cert)

  injectPerasCertIntoShelleyBlockBody cert =
    SL.perasCertBlockBodyL .~ SJust (toLedgerPerasCert cert)
