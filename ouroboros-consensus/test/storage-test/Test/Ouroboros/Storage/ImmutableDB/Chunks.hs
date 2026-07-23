-- | Immutable DB chunks tests.
module Test.Ouroboros.Storage.ImmutableDB.Chunks (tests) where

import Data.List (sort)
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (..), chunksBetween)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.Arbitrary ()

property_chunksBetween_is_never_empty :: ChunkNo -> ChunkNo -> Bool
property_chunksBetween_is_never_empty cn1 cn2 =
  not (null (chunksBetween cn1 cn2))

property_chunksBetween_is_singleton_when_bounds_are_equal :: ChunkNo -> Property
property_chunksBetween_is_singleton_when_bounds_are_equal cn =
  chunksBetween cn cn === [cn]

property_chunksBetween_is_invariant_to_argument_order :: ChunkNo -> ChunkNo -> Property
property_chunksBetween_is_invariant_to_argument_order cn1 cn2 =
  chunksBetween cn1 cn2 === chunksBetween cn2 cn1

property_chunksBetween_result_is_sorted :: ChunkNo -> ChunkNo -> Property
property_chunksBetween_result_is_sorted cn1 cn2 =
  let result = chunksBetween cn1 cn2
   in result === sort result

property_chunksBetween_result_length_indicates_inclusive_bounds :: ChunkNo -> ChunkNo -> Property
property_chunksBetween_result_length_indicates_inclusive_bounds cn1@(ChunkNo a) cn2@(ChunkNo b) =
  let expected = 1 + abs (toInteger a - toInteger b)
      observed = toInteger $ length (chunksBetween cn1 cn2)
   in observed === expected

tests :: TestTree
tests =
  testGroup
    "ImmutableDB chunking"
    [ testProperty "chunksBetween is never empty" property_chunksBetween_is_never_empty
    , testProperty
        "chunksBetween is singleton when bounds are equal"
        property_chunksBetween_is_singleton_when_bounds_are_equal
    , testProperty
        "chunksBetween is invariant to argument order"
        property_chunksBetween_is_invariant_to_argument_order
    , testProperty
        "chunksBetween result is sorted"
        property_chunksBetween_result_is_sorted
    , testProperty
        "chunksBetween result length indicates inclusive bounds"
        property_chunksBetween_result_length_indicates_inclusive_bounds
    ]
