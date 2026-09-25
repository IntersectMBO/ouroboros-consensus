{-# LANGUAGE ScopedTypeVariables #-}

-- | Property-based tests for 'Bitmap'
module Test.Consensus.Util.Bitmap (tests) where

import Cardano.Binary (decodeFull, serialize)
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Util.Bitmap as Bitmap
import Test.QuickCheck (Testable (..), counterexample, vectorOf)
import Test.Tasty
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , choose
  , forAll
  , testProperty
  , (===)
  )
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "Bitmap"
      [ testProperty
          "prop_roundtrip_toIndices"
          prop_roundtrip_toIndices
      , testProperty
          "prop_roundtrip_serialisation"
          prop_roundtrip_serialisation
      ]

-- * Properties

-- | Converting from indices to bitmap and back preserves the indices.
prop_roundtrip_toIndices :: Property
prop_roundtrip_toIndices =
  forAll genMaxIndex $ \maxIndex ->
    forAll genNumIndices $ \numIndices -> do
      forAll (genIndices numIndices maxIndex) $ \indices -> do
        let bitmap = Bitmap.fromIndices maxIndex indices
        let indices' = Bitmap.toIndices bitmap
        Set.fromList indices === Set.fromList indices'

-- | Serialisation roundtrip preserves the bitmap.
prop_roundtrip_serialisation :: Property
prop_roundtrip_serialisation =
  forAll genMaxIndex $ \maxIndex ->
    forAll genNumIndices $ \numIndices -> do
      forAll (genIndices numIndices maxIndex) $ \indices -> do
        let bitmap = Bitmap.fromIndices maxIndex indices
        let encoded = serialize bitmap
        case decodeFull encoded of
          Left err ->
            counterexample ("Deserialization failed: " <> show err) $
              property False
          Right bitmap' ->
            bitmap === bitmap'

-- * Generators

genMaxIndex :: Gen Int
genMaxIndex =
  choose (0, 10000)

genNumIndices :: Gen Int
genNumIndices =
  choose (0, 100)

genIndices :: Int -> Int -> Gen [Int]
genIndices numIndices maxIndex =
  vectorOf numIndices (choose (0, maxIndex))
