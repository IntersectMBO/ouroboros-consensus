-- | Utility functions for the WFALS model tests.
module Test.Consensus.Committee.WFALS.Model.Utils
  ( -- * Generators
    genStake
  , genUniqueVoterIds
  , genStakeDistr

    -- * Property helpers
  , forAllValidStakeDistrAndNumSeats
  , forAllPossiblyInvalidStakeDistrAndNumSeats
  , tabulateTargetNumSeats
  , tabulateStakeDistrSize
  , tabulateMoreSeatsThanNodes
  , tabulatePersistentToNonPersistentRatio
  ) where

import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Word (Word64)
import Test.Consensus.Committee.WFALS.Model
  ( IsStake (..)
  , NumSeats
  , Stake (..)
  , StakeDistr
  , StakeRole (..)
  , StakeType (..)
  , VoterId
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , choose
  , forAll
  , sized
  , tabulate
  , vectorOf
  )

-- * Generators

-- | Generate a random stake as a Rational number
genStake :: Gen (Stake Ledger Global)
genStake = do
  Positive num <- arbitrary
  Positive den <- arbitrary
  pure (StakeLedgerGlobal (num % den))

-- | Generate a non-empty list of unique voter IDs of a given size.
genUniqueVoterIds :: Int -> Gen [VoterId]
genUniqueVoterIds size =
  fmap show <$> go size Set.empty
 where
  go 0 acc =
    return (Set.toList acc)
  go k acc = do
    voterId <- choose (0, maxBound :: Word64)
    if voterId `Set.member` acc
      then
        go k acc
      else do
        rest <- go (k - 1) (Set.insert voterId acc)
        pure (voterId : rest)

-- | Generate a non-empty random stake distribution of a given size
--
-- NOTE: the size is shifted up by one to ensure non-emptiness.
genStakeDistr :: Int -> Gen (StakeDistr Ledger Global)
genStakeDistr size = do
  ids <- genUniqueVoterIds (succ size)
  stakes <- vectorOf (succ size) genStake
  pure (Map.fromList (zip ids stakes))

-- * Property helpers

-- | Helper to generate stake distributions along with a number of seats that
-- lies within the acceptable range [1, #{nodes with positive stake}]
forAllValidStakeDistrAndNumSeats ::
  (StakeDistr Ledger Global -> NumSeats Global -> Property) ->
  Property
forAllValidStakeDistrAndNumSeats p =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let numPositiveStakeNodes =
          fromIntegral
            . length
            . filter ((> 0) . stakeToRational)
            . Map.elems
            $ stakeDistr
    let genNumSeats =
          fromInteger <$> choose (1, numPositiveStakeNodes)
    forAll genNumSeats $ \numSeats ->
      p stakeDistr numSeats

-- | Helper to generate stake distributions along with a number of seats that
-- could possibly be invalid in different ways, e.g., too many expected seats
-- or an empty stake distribution.
forAllPossiblyInvalidStakeDistrAndNumSeats ::
  (StakeDistr Ledger Global -> NumSeats Global -> Property) ->
  Property
forAllPossiblyInvalidStakeDistrAndNumSeats p =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let numNodes =
          fromIntegral (Map.size stakeDistr)
    let genNumSeats =
          fromInteger <$> choose (1, numNodes + 1)
    forAll genNumSeats $ \numSeats ->
      p stakeDistr numSeats

-- | Tabulate the target number of seats
tabulateTargetNumSeats :: NumSeats Global -> Property -> Property
tabulateTargetNumSeats numSeats =
  tabulate "Target number of seats" [bucket]
 where
  -- Divide in buckets of 10
  bucket
    | lower == upper = show lower
    | otherwise = show lower <> "-" <> show upper
  lower = (numSeats `div` 10) * 10
  upper = lower + 9

-- | Tabulate the size of a stake distribution
tabulateStakeDistrSize :: StakeDistr Ledger Global -> Property -> Property
tabulateStakeDistrSize stakeDistr =
  tabulate "Stake distribution size" [bucket]
 where
  -- Divide in buckets of 10
  bucket
    | lower == upper = show lower
    | otherwise = show lower <> "-" <> show upper
  lower = (size `div` 10) * 10
  upper = lower + 9
  size = Map.size stakeDistr

-- | Tabulate whether we are asking for more seats than nodes in the stake
-- distribution or not
tabulateMoreSeatsThanNodes ::
  StakeDistr Ledger Global ->
  NumSeats Global ->
  Property ->
  Property
tabulateMoreSeatsThanNodes stakeDistr numSeats =
  tabulate
    "More target seats than nodes"
    [ show (numSeats > fromIntegral (Map.size stakeDistr))
    ]

-- | Tabulate the ratio of persistent to non-persistent seats as the percentage
-- of persistent seats out of the total number of seats.
tabulatePersistentToNonPersistentRatio ::
  NumSeats Global ->
  NumSeats Global ->
  Property ->
  Property
tabulatePersistentToNonPersistentRatio
  numPersistentSeats
  numNonPersistentSeats =
    tabulate "Persistent to non-persistent seat ratio" [bucket]
   where
    -- Divide buckets in 10% increments
    bucket
      | lower == upper = show lower <> "%"
      | otherwise = show lower <> "%-" <> show upper <> "%"
    lower = (numPersistentSeats * 100) `div` totalSeats `div` 10 * 10
    upper = min 100 (lower + 10)
    totalSeats = numPersistentSeats + numNonPersistentSeats
