{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras certificate types using BLS signatures.
module Ouroboros.Consensus.Peras.Crypto.Cert
  ( PerasCert (..)
  , PerasCertVoters (..)
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Ouroboros.Consensus.Block (ConvertRawHash, HeaderHash)
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo)
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.Types
  ( PerasBoostedBlock (..)
  , PerasSeatIndex (..)
  )
import Ouroboros.Consensus.Peras.Crypto.Vote (PerasVoteElegibilityProof (..))
import Ouroboros.Consensus.Util.Bitmap (Bitmap)
import qualified Ouroboros.Consensus.Util.Bitmap as Bitmap
import Ouroboros.Network.Block (StandardHash)

-- | Concrete Peras certificates using BLS signatures
data PerasCert blk
  = PerasCert
  { pcRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pcBoostedBlock :: !(PerasBoostedBlock blk)
  -- ^ Certificate message, i.e., the hash of the block being boosted
  , pcVoters :: !PerasCertVoters
  -- ^ Voters who contributed to this certificate
  , pcSignature :: !(BLS.PerasSignature BLS.SIGN)
  -- ^ Aggregate BLS signature on the hash of the election identifier and
  -- the certificate message
  }

deriving instance Eq (HeaderHash blk) => Eq (PerasCert blk)
deriving instance Show (HeaderHash blk) => Show (PerasCert blk)

instance
  ( Typeable blk
  , ConvertRawHash blk
  , StandardHash blk
  ) =>
  FromCBOR (PerasCert blk)
  where
  fromCBOR = do
    decodeListLenOf 4
    pcRoundNo <- fromCBOR
    pcBoostedBlock <- fromCBOR
    pcVoters <- fromCBOR
    pcSignature <- fromCBOR
    pure
      PerasCert
        { pcRoundNo
        , pcBoostedBlock
        , pcVoters
        , pcSignature
        }

instance
  ( Typeable blk
  , ConvertRawHash blk
  , StandardHash blk
  ) =>
  ToCBOR (PerasCert blk)
  where
  toCBOR cert =
    encodeListLen 4
      <> toCBOR (pcRoundNo cert)
      <> toCBOR (pcBoostedBlock cert)
      <> toCBOR (pcVoters cert)
      <> toCBOR (pcSignature cert)

-- | Voters contained in a certificate with their appropriate elegibility proof
newtype PerasCertVoters
  = PerasCertVoters
  { unPerasCertVoters ::
      NE (Map PerasSeatIndex PerasVoteElegibilityProof)
  }
  deriving stock (Eq, Show)

instance FromCBOR PerasCertVoters where
  fromCBOR = do
    decodeListLenOf 2
    votersBitmap <- fromCBOR
    nonPersistentProofs <- fromCBOR

    either (\err -> fail ("Invalid Peras certificate: " <> err)) pure $
      reifyVoters votersBitmap nonPersistentProofs

instance ToCBOR PerasCertVoters where
  toCBOR voters =
    encodeListLen 2
      <> toCBOR votersBitmap
      <> toCBOR nonPersistentProofs
   where
    (votersBitmap, nonPersistentProofs) =
      either (\err -> error ("Internal encoding error: " <> err)) id $
        reflectVoters voters

reifyVoters ::
  Bitmap Word16 ->
  [BLS.PerasSignature BLS.VRF] ->
  Either String PerasCertVoters
reifyVoters votersBitmap nonPersistentSignatures = do
  let voterSeatIndices =
        PerasSeatIndex <$> Bitmap.toIndices votersBitmap

  when (null voterSeatIndices) $
    throwError "Invalid Peras certificate: empty voters bitmap"

  when (length nonPersistentSignatures > length voterSeatIndices) $
    throwError $
      unlines
        [ "Invalid Peras certificate:"
            <> " more non-persistent voter elegibility proofs where provided"
            <> " than the number of voters in the certificate"
        , " * number of voters: "
            <> show (length voterSeatIndices)
        , " * number of proofs: "
            <> show (length nonPersistentSignatures)
        ]

  let numPersistentVoters =
        length voterSeatIndices - length nonPersistentSignatures
  let persistentProofs =
        take numPersistentVoters (repeat PersistentPerasVoteElegibilityProof)
  let nonPersistentProofs =
        fmap NonPersistentPerasVoteElegibilityProof nonPersistentSignatures
  let voters =
        NEMap.fromAscList
          . NonEmpty.fromList
          . zip voterSeatIndices
          $ persistentProofs <> nonPersistentProofs

  pure (PerasCertVoters voters)

reflectVoters ::
  PerasCertVoters ->
  Either String (Bitmap Word16, [BLS.PerasSignature BLS.VRF])
reflectVoters (PerasCertVoters voters) = do
  let logicalUpperBound =
        unPerasSeatIndex (fst (NEMap.findMax voters))
  let votersByAscSeatIndex =
        NonEmpty.toList (NEMap.toAscList voters)
  let votersSeatIndices =
        fmap (unPerasSeatIndex . fst) votersByAscSeatIndex

  when (length votersSeatIndices > fromIntegral logicalUpperBound + 1) $
    throwError $
      unlines
        [ "Invalid Peras certificate:"
            <> " the number of voters exceeds the declared size of the voters"
            <> " bitmap in the certificate"
        , " * number of voters: "
            <> show (length votersSeatIndices)
        , " * voters bitmap size: "
            <> show @Int (fromIntegral logicalUpperBound + 1)
        ]

  let votersBitmap =
        Bitmap.fromIndices
          logicalUpperBound
          votersSeatIndices
  let nonPersistentProofs =
        catMaybes $ flip fmap votersByAscSeatIndex $ \(_, proof) ->
          case proof of
            PersistentPerasVoteElegibilityProof -> Nothing
            NonPersistentPerasVoteElegibilityProof p -> Just p

  pure (votersBitmap, nonPersistentProofs)
