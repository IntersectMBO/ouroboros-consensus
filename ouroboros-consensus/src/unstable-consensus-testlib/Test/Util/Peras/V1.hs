{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common utilities for writing tests for Peras types.
module Test.Util.Peras.V1
  ( -- * Predicates
    perasVoteIsPersistent
  , perasCertContainsOnlyPersistentVotes

    -- * Generators
  , genPerasVote
  , genPerasCert

    -- * Tabulators
  , tabulatePerasCert
  , tabulatePerasVote
  ) where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Ledger.BaseTypes (SlotNo (..))
import Control.Monad (forM)
import qualified Data.ByteString as ByteString
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString
import Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Traversable (mapAccumM)
import Data.Word (Word8)
import GHC.Word (Word16)
import Ouroboros.Consensus.Block (HeaderHash)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..), WithOrigin (..))
import Ouroboros.Consensus.Block.RealPoint (RealPoint (..), toBytes32RealPoint)
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCryptoAggregateVoteSignature (..)
  , VRFOutput (..)
  , VoteSignature (..)
  )
import Ouroboros.Consensus.Peras.Types
  ( PerasBoostedBlock (..)
  , PerasSeatIndex (..)
  )
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , choose
  , frequency
  , sized
  , tabulate
  , vectorOf
  )
import Test.Util.Peras.Internal (genRoundNo, genSeatIndex, mkBucket)

-- * Predicates

-- | Whether a Peras vote is a persistent one
perasVoteIsPersistent :: V1.PerasVote tag -> Bool
perasVoteIsPersistent vote
  | V1.PersistentPerasVoteEligibilityProof{} <- V1.pvEligibilityProof vote = True
  | otherwise = False

-- | Whether a Peras certifcate only contains persistent votes
perasCertContainsOnlyPersistentVotes :: V1.PerasCert tag -> Bool
perasCertContainsOnlyPersistentVotes cert =
  all
    ( \case
        V1.PersistentPerasVoteEligibilityProof -> True
        V1.NonPersistentPerasVoteEligibilityProof{} -> False
    )
    ( NEMap.elems
        . V1.unPerasCertVoters
        . V1.pcVoters
        $ cert
    )

-- * Generators

data BlockWith32BytesHeaderHash
type instance HeaderHash BlockWith32BytesHeaderHash = ShortByteString
instance ConvertRawHash (BlockWith32BytesHeaderHash) where
  toShortRawHash _ = coerce
  fromShortRawHash _ = coerce
  hashSize _ = 32

genBoostedBlock :: Gen PerasBoostedBlock
genBoostedBlock = PerasBoostedBlock <$> genWithOrigin genBytes32RealPoint
 where
  genWithOrigin gen =
    frequency
      [ (1, pure Origin)
      , (9, NotOrigin <$> gen)
      ]
  genBytes32RealPoint = do
    slotNo <- SlotNo <$> arbitrary
    hash <- ShortByteString.pack <$> vectorOf 32 arbitrary
    pure $
      toBytes32RealPoint @BlockWith32BytesHeaderHash $
        RealPoint slotNo hash

genPrivateKey :: Proxy r -> Gen (BLS.PrivateKey r)
genPrivateKey _ =
  fromMaybe (error "genPrivateKey: invalid key bytes")
    . BLS.rawDeserialisePrivateKey "ROUNDTRIP"
    . ByteString.pack
    <$> vectorOf 32 (arbitrary @Word8)

genSignature ::
  forall r.
  BLS.HasBLSContext r =>
  Proxy r ->
  Gen (BLS.Signature r)
genSignature _ = do
  key <- genPrivateKey (Proxy @r)
  msg <- fromString @ByteString <$> arbitrary
  pure $ BLS.signWithRole key msg

genVoteEligibilityProof :: Bool -> Gen V1.PerasVoteEligibilityProof
genVoteEligibilityProof shouldGenNonPersistent = do
  frequency
    [
      ( 4
      , pure V1.PersistentPerasVoteEligibilityProof
      )
    ,
      ( if shouldGenNonPersistent then 1 else 0
      , V1.NonPersistentPerasVoteEligibilityProof
          . PerasBLSCryptoVRFOutput
          <$> genSignature (Proxy @BLS.VRF)
      )
    ]

genVoters :: Bool -> Gen V1.PerasCertVoters
genVoters shouldGenNonPersistent = do
  numVoters <-
    sized $ \size ->
      fmap (+ 1) $
        choose @Word16 (0, fromIntegral size * 10)
  numPersistentVoters <-
    case shouldGenNonPersistent of
      True -> choose (0, numVoters)
      False -> pure numVoters
  persistentVoters <-
    if numPersistentVoters == 0
      then pure []
      else forM [0 .. numPersistentVoters - 1] $ \i -> do
        let proof = V1.PersistentPerasVoteEligibilityProof
        pure (PerasSeatIndex i, proof)
  nonPersistentVoters <-
    if numPersistentVoters == numVoters
      then pure []
      else forM [numPersistentVoters .. numVoters - 1] $ \i -> do
        proof <-
          V1.NonPersistentPerasVoteEligibilityProof
            . PerasBLSCryptoVRFOutput
            <$> genSignature (Proxy @BLS.VRF)
        pure (PerasSeatIndex i, proof)
  voters <-
    fmap (snd . fmap catMaybes)
      . mapAccumM
        ( \canDrop (i, proof) -> do
            voter <-
              frequency
                [ (75, pure (Just (i, proof)))
                , (if canDrop then 25 else 0, pure Nothing)
                ]
            pure
              ( canDrop || voter == Nothing
              , voter
              )
        )
        False
      $ persistentVoters <> nonPersistentVoters
  pure $
    V1.PerasCertVoters (NEMap.fromList (NonEmpty.fromList voters))

genPerasVote :: Bool -> Gen (V1.PerasVote tag)
genPerasVote shouldGenNonPersistent = do
  pvRoundNo <- genRoundNo
  pvBoostedBlock <- genBoostedBlock
  pvSeatIndex <- genSeatIndex
  pvEligibilityProof <- genVoteEligibilityProof shouldGenNonPersistent
  pvSignature <-
    PerasBLSCryptoVoteSignature
      <$> genSignature (Proxy @BLS.SIGN)
  pure
    V1.PerasVote
      { V1.pvRoundNo
      , V1.pvBoostedBlock
      , V1.pvSeatIndex
      , V1.pvEligibilityProof
      , V1.pvSignature
      }

genPerasCert :: Bool -> Gen (V1.PerasCert tag)
genPerasCert shouldGenNonPersistent = do
  pcRoundNo <- genRoundNo
  pcBoostedBlock <- genBoostedBlock
  pcVoters <- genVoters shouldGenNonPersistent
  pcSignature <-
    PerasBLSCryptoAggregateVoteSignature
      <$> genSignature (Proxy @BLS.SIGN)
  pure
    V1.PerasCert
      { V1.pcRoundNo
      , V1.pcBoostedBlock
      , V1.pcVoters
      , V1.pcSignature
      }

-- * Tabulators

tabulatePerasCert :: V1.PerasCert tag -> Property -> Property
tabulatePerasCert cert =
  foldr (flip (.)) id $
    [ tabulate
        "Number of voters"
        [mkBucket 100 numVoters " voters"]
    , tabulate
        "Proportion of persistent voters"
        [mkBucket 10 persistentVotersRatio "%"]
    ]
 where
  numVoters =
    length
      . V1.unPerasCertVoters
      . V1.pcVoters
      $ cert
  numPersistentVoters =
    length
      . filter (== V1.PersistentPerasVoteEligibilityProof)
      . NonEmpty.toList
      . NEMap.elems
      . V1.unPerasCertVoters
      . V1.pcVoters
      $ cert

  persistentVotersRatio
    | numVoters == 0 = 0
    | otherwise = numPersistentVoters * 100 `div` numVoters

tabulatePerasVote :: V1.PerasVote tag -> Property -> Property
tabulatePerasVote vote =
  foldr (flip (.)) id $
    [ tabulate
        "Voter type"
        [voterType]
    ]
 where
  voterType =
    case V1.pvEligibilityProof vote of
      V1.PersistentPerasVoteEligibilityProof -> "persistent"
      V1.NonPersistentPerasVoteEligibilityProof _ -> "non-persistent"
