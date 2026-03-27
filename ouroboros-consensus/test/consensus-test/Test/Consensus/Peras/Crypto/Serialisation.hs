{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Serialisation tests for crypto-related Peras types
module Test.Consensus.Peras.Crypto.Serialisation
  ( tests
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull, serialize)
import Cardano.Crypto.Hash (ByteString)
import Control.Monad (forM)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Traversable (mapAccumM)
import Data.Word (Word8)
import GHC.Word (Word16)
import Ouroboros.Consensus.Block (PerasRoundNo (..))
import Ouroboros.Consensus.Peras.Crypto
  ( HasPerasBLSContext
  , PerasCert (..)
  , PerasCertVoters (..)
  , PerasVote (..)
  , PerasVoteElegibilityProof (..)
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.Types
  ( PerasBoostedBlock (..)
  , PerasSeatIndex (..)
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , choose
  , counterexample
  , forAll
  , frequency
  , listOf1
  , sized
  , tabulate
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock (TestBlock, testHashFromList)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "roundtrip serialisation properties for Peras crypto types"
    [ adjustQuickCheckTests (* 10) $
        testProperty "roundtrip for PerasCert" $
          prop_roundtrip genPerasCert tabulatePerasCert
    , adjustQuickCheckTests (* 10) $
        testProperty "roundtrip for PerasVote" $
          prop_roundtrip genPerasVote tabulatePerasVote
    ]

-- * Properties

prop_roundtrip ::
  forall a.
  ( Eq a
  , Show a
  , ToCBOR a
  , FromCBOR a
  ) =>
  Gen a ->
  (a -> Property -> Property) ->
  Property
prop_roundtrip gen tabulateValue =
  forAll gen $ \a -> do
    let encoded = serialize a
    let decoded = decodeFull encoded
    tabulateEncodedSize encoded
      . tabulateValue a
      . counterexample
        ( unlines
            [ "Original value:"
            , show a
            , "Decoded value:"
            , show decoded
            ]
        )
      $ Right a === decoded

-- * Generators

genRoundNo :: Gen PerasRoundNo
genRoundNo = PerasRoundNo <$> arbitrary

genBoostedBlock :: Gen (PerasBoostedBlock TestBlock)
genBoostedBlock = PerasBoostedBlock . testHashFromList <$> listOf1 arbitrary

genSeatIndex :: Gen PerasSeatIndex
genSeatIndex = PerasSeatIndex <$> arbitrary

genPrivateKey :: Proxy r -> Gen (BLS.PerasPrivateKey r)
genPrivateKey _ =
  fromMaybe (error "genPrivateKey: invalid key bytes")
    . BLS.rawDeserialisePerasPrivateKey "ROUNDTRIP"
    . ByteString.pack
    <$> vectorOf 32 (arbitrary @Word8)

genSignature ::
  forall r.
  HasPerasBLSContext r =>
  Proxy r ->
  Gen (BLS.PerasSignature r)
genSignature _ = do
  key <- genPrivateKey (Proxy @r)
  msg <- fromString @ByteString <$> arbitrary
  pure $ BLS.perasSignWithRole key msg

genVoteElegibilityProof :: Gen PerasVoteElegibilityProof
genVoteElegibilityProof = do
  frequency
    [ (4, pure PersistentPerasVoteElegibilityProof)
    , (1, NonPersistentPerasVoteElegibilityProof <$> genSignature (Proxy @BLS.VRF))
    ]

genVoters :: Gen PerasCertVoters
genVoters = do
  -- The number of voters on this certificate (not null)
  numVoters <-
    sized $ \size ->
      fmap (+ 1) $
        choose @Word16 (0, fromIntegral size * 10)
  -- Pick some voters to be persistent and leave the rest as non-persistent
  numPersistentVoters <-
    choose (0, numVoters)
  -- Generate all possible persistent voters and their proofs
  persistentVoters <-
    if numPersistentVoters == 0
      then pure []
      else forM [0 .. numPersistentVoters - 1] $ \i -> do
        pure (PerasSeatIndex i, PersistentPerasVoteElegibilityProof)
  -- Generate all possible non-persistent voters and their proofs
  nonPersistentVoters <-
    if numPersistentVoters == numVoters
      then pure []
      else forM [numPersistentVoters .. numVoters - 1] $ \i -> do
        proof <- genSignature (Proxy @BLS.VRF)
        pure (PerasSeatIndex i, NonPersistentPerasVoteElegibilityProof proof)
  -- Randomly drop some of the voters while keeping the map non-empty
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
    PerasCertVoters (NEMap.fromList (NonEmpty.fromList voters))

genPerasCert :: Gen (PerasCert TestBlock)
genPerasCert = do
  pcRoundNo <- genRoundNo
  pcBoostedBlock <- genBoostedBlock
  pcVoters <- genVoters
  pcSignature <- genSignature (Proxy @BLS.SIGN)
  pure
    PerasCert
      { pcRoundNo
      , pcBoostedBlock
      , pcVoters
      , pcSignature
      }

genPerasVote :: Gen (PerasVote TestBlock)
genPerasVote = do
  pvRoundNo <- genRoundNo
  pvBoostedBlock <- genBoostedBlock
  pvSeatIndex <- genSeatIndex
  pvElegibilityProof <- genVoteElegibilityProof
  pvSignature <- genSignature (Proxy @BLS.SIGN)
  pure
    PerasVote
      { pvRoundNo
      , pvBoostedBlock
      , pvSeatIndex
      , pvElegibilityProof
      , pvSignature
      }

-- * Tabulators

mkBucket :: Int -> Int -> String -> String
mkBucket bucketSize x suffix
  | lower == upper = show lower <> suffix
  | otherwise = show lower <> "-" <> show upper <> suffix
 where
  lower = (x `div` bucketSize) * bucketSize
  upper = lower + bucketSize

tabulateEncodedSize :: LazyByteString.ByteString -> Property -> Property
tabulateEncodedSize bytes =
  tabulate
    "encoded size "
    [mkBucket 1000 (fromIntegral (LazyByteString.length bytes)) " bytes"]

tabulatePerasCert :: PerasCert TestBlock -> Property -> Property
tabulatePerasCert cert =
  foldr (flip (.)) id $
    [ tabulate
        "number of voters"
        [mkBucket 100 numVoters " voters"]
    , tabulate
        "proportion of persistent voters"
        [mkBucket 10 persistentVotersRatio "%"]
    ]
 where
  numVoters =
    length
      . unPerasCertVoters
      . pcVoters
      $ cert
  numPersistentVoters =
    length
      . filter (== PersistentPerasVoteElegibilityProof)
      . NonEmpty.toList
      . NEMap.elems
      . unPerasCertVoters
      . pcVoters
      $ cert

  persistentVotersRatio
    | numVoters == 0 = 0
    | otherwise = numPersistentVoters * 100 `div` numVoters

tabulatePerasVote :: PerasVote TestBlock -> Property -> Property
tabulatePerasVote vote =
  foldr (flip (.)) id $
    [ tabulate
        "voter type"
        [voterType]
    ]
 where
  voterType =
    case pvElegibilityProof vote of
      PersistentPerasVoteElegibilityProof -> "persistent"
      NonPersistentPerasVoteElegibilityProof _ -> "non-persistent"
