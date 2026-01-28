-- | Model implementing committee selection based on the Fait Accompli scheme
module Test.Consensus.Peras.Committee.Model
  ( -- * Auxiliary types
    VoterId (..)
  , NumSeats
  , SeatIndex

    -- * Stake Distribution
  , StakeDistr (..)
  , mkStakeDistr
  , stakeDistrToNonIncreasingStakeList
  , stakeDistrSize
  , stakeDistrTotalStake
  , normalizeStakeDistr

    -- * Fait Accompli Committee Selection
  , weightedFaitAccompliWith
  , weightedFaitAccompliThreshold
  , localSortition
  , nullFallback
  , idFallback

    -- ** Random sampling helpers
  , poisson

    -- * Property tests
  , genStake
  , genUniqueVoterIds
  , genStakeDistr
  , prop_stakeDistrNonIncreasingStakeList
  , prop_weightedFaitAccompliWith_persistentSeatsRespectNumSeats
  , prop_weightedFaitAccompliWith_persistentSeatsHaveLargerStake
  , prop_weightedFaitAccompliWith_totalStakePreserved
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Traversable (mapAccumR)
import GHC.Word (Word64)
import Numeric.Natural (Natural, minusNaturalMaybe)
import System.Random (RandomGen, randomR)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , choose
  , counterexample
  , forAll
  , sized
  , vectorOf
  , (==>)
  )

-- * Auxiliary types

-- | Voter identifier
newtype VoterId = VoterId {unVoterId :: Word64}
  deriving (Eq, Ord)

instance Show VoterId where
  show (VoterId i) = "#" <> show i

-- | Voter stake
type VoterStake = Rational

-- | Number of seats in the committee
type NumSeats = Natural

-- | Seat index (0-based)
type SeatIndex = Natural

-- * Stake Distribution

-- | Stake distribution mapping voter IDs to their stakes
newtype StakeDistr = StakeDistr {unStakeDistr :: Map VoterId VoterStake}
  deriving (Eq, Show)

-- | Create a stake distribution from a list of (voter ID, stake) pairs
mkStakeDistr :: [(VoterId, VoterStake)] -> StakeDistr
mkStakeDistr pairs =
  StakeDistr (Map.fromList pairs)

-- | Normalized a stake distribution so that the total stake adds up to 1
normalizeStakeDistr :: StakeDistr -> StakeDistr
normalizeStakeDistr (StakeDistr distr) =
  StakeDistr (Map.map (/ totalStake) distr)
 where
  totalStake = sum (Map.elems distr)

-- | Get the size of the stake distribution
stakeDistrSize :: StakeDistr -> Int
stakeDistrSize (StakeDistr distr) =
  Map.size distr

-- | Get the total stake in a stake distribution
stakeDistrTotalStake :: StakeDistr -> VoterStake
stakeDistrTotalStake (StakeDistr distr) =
  sum (Map.elems distr)

-- | View a stake distribution as a non-increasing list of stakes
stakeDistrToNonIncreasingStakeList :: StakeDistr -> [(VoterId, VoterStake)]
stakeDistrToNonIncreasingStakeList (StakeDistr distr) =
  sortBy (comparing (Down . snd)) (Map.toList distr)

-- * Fait Accompli Committee Selection

-- | Weighted Fait Accompli committee selection with a fallback mechanism
weightedFaitAccompliWith ::
  -- | Fallback function for non-persistent seats
  (NumSeats -> StakeDistr -> StakeDistr) ->
  -- | Total number of seats
  NumSeats ->
  -- | Stake distribution
  StakeDistr ->
  -- | Selected persistent and non-persistent seats with their corresponding voting stakes
  (StakeDistr, StakeDistr)
weightedFaitAccompliWith fallback numSeats stakeDistr
  | numSeats <= 0 =
      error "Number of seats must be positive"
  | stakeDistrSize stakeDistr == 0 =
      error "Stake distribution cannot be empty"
  | otherwise =
      (persistentSeats, nonPersistentSeats)
 where
  -- Persistent seats selected directly based on stake
  persistentSeats =
    mkStakeDistr $
      toVoterIdAndStake $
        persistentSeatsWithStake

  -- Non-persistent seats selected via fallback
  nonPersistentSeats =
    fallback numNonPersistentSeats $
      mkStakeDistr $
        toVoterIdAndStake $
          nonPersistentSeatsWithStake

  -- Number of persistent and non-persistent seats
  numPersistentSeats =
    fromIntegral (length persistentSeatsWithStake)
  numNonPersistentSeats =
    case minusNaturalMaybe numSeats numPersistentSeats of
      Just n -> n
      Nothing -> error "Number of persistent seats exceeds total seats"

  -- Split the cumulative stakes into persistent and non-persistent seats
  (persistentSeatsWithStake, nonPersistentSeatsWithStake) =
    span
      ( \(_, voterStake, seatIndex, stakeAccum) ->
          weightedFaitAccompliThreshold numSeats seatIndex voterStake stakeAccum
      )
      $ rightCumulativeNonIncreasingStakes

  -- Stake distribution in non-increasing order, including voter seat index and
  -- accumulated stakes from the right.
  rightCumulativeNonIncreasingStakes =
    snd $
      mapAccumR accumStake 0 $
        addSeatIndex $
          stakeDistrToNonIncreasingStakeList $
            stakeDistr
   where
    addSeatIndex =
      zipWith
        (\seatIndex (voterId, voterStake) -> (voterId, voterStake, seatIndex))
        [0 ..]
    accumStake stakeAccum (voterId, voterStake, seatIndex) =
      ( voterStake + stakeAccum
      , (voterId, voterStake, seatIndex, voterStake + stakeAccum)
      )

  -- Extract only voter ID and stake from the full tuple used for calculations
  toVoterIdAndStake =
    fmap $ \(voterId, voterStake, _, _) -> (voterId, voterStake)

-- | Weighted Fait Accompli threshold for persistent seats
weightedFaitAccompliThreshold ::
  -- | Total number of seats
  NumSeats ->
  -- | Voter seat index (0-based)
  SeatIndex ->
  -- | Voter stake
  VoterStake ->
  -- | Accumulated stake from the right (including voter stake)
  Rational ->
  -- | Whether the voter qualifies for a persistent seat
  Bool
weightedFaitAccompliThreshold numSeats voterSeat voterStake stakeAccR
  | stakeAccR <= 0 =
      False
  | otherwise =
      ( (1 - (voterStake / stakeAccR))
          ^ (2 :: Integer)
      )
        < ( toRational (numSeats - voterSeat)
              / toRational (numSeats - voterSeat + 1)
          )

-- | Local sortition fallback
localSortition ::
  RandomGen rng =>
  -- | Random number generator
  rng ->
  -- | Expected number of non-persistent seats
  NumSeats ->
  -- | Non-persistent stake distribution candidated (not necessarily normalized)
  StakeDistr ->
  -- | Selected non-persistent seats with their correspoinding voting stakes
  StakeDistr
localSortition rng0 numSeats stakeDistr =
  mkStakeDistr $
    sampleNonPersistentVoters $
      stakeDistrToNonIncreasingStakeList $
        normalizeStakeDistr $
          stakeDistr
 where
  -- The total stake of the non-persistent candidate stake distribution
  totalStake = stakeDistrTotalStake stakeDistr
  -- All non-persistent voters share the same voting "weight"
  nonPersistentVoterStake = totalStake / fromIntegral numSeats
  -- Sample non-persistent voter seats independently to emulate local sortition
  sampleNonPersistentVoters = snd . foldr sampleVoterStake (rng0, [])
  -- Sample voter seats for each voter independently to emulate local sortition
  sampleVoterStake (voterId, voterStake) (rng, acc) =
    let
      -- Expected number of seats for this voter
      lambda = fromIntegral numSeats * fromRational voterStake
      -- Sample number of seats using Poisson distribution
      (rng', seats) = poisson lambda rng
      -- Total voting stake scaled by the number of seats this voter was granted
      voterStake' = nonPersistentVoterStake * fromIntegral seats
     in
      ( -- Thread the updated random generator
        rng'
      , -- Accumulate voter if they won any seats
        if seats > 0 then (voterId, voterStake') : acc else acc
      )

-- Convert the map of (voter stake, voter seats) to a stake distribution

-- | Null fallback (for testing purposes).
--
-- Always returns an empty stake distribution.
nullFallback ::
  NumSeats ->
  StakeDistr ->
  StakeDistr
nullFallback _ _ =
  mkStakeDistr []

-- | Identity fallback (for testing purposes)
--
-- Returns the original stake distribution unchanged.
idFallback ::
  NumSeats ->
  StakeDistr ->
  StakeDistr
idFallback _ stakeDistr =
  stakeDistr

-- ** Random sampling helpers

-- | Poisson sampling using Knuth's algorithm
poisson :: RandomGen rng => Double -> rng -> (rng, Int)
poisson lambda rng0 = go 0 1 rng0
 where
  go k p rng
    | p <= exp (-lambda) =
        (rng, k - 1)
    | otherwise =
        let (u, rng') = randomR (0, 1) rng
         in go (k + 1) (p * u) rng'

-- * Property tests

-- ** Generators

-- | Generate a random stake as a Rational number
genStake :: Gen VoterStake
genStake = do
  Positive num <- arbitrary
  Positive den <- arbitrary
  pure (num % den)

-- | Generate a non-empty list of unique voter IDs of a given size
genUniqueVoterIds :: Int -> Gen [VoterId]
genUniqueVoterIds size =
  go size Set.empty
 where
  go 0 acc =
    return (Set.toList acc)
  go k acc = do
    voterId <- VoterId <$> choose (0, maxBound :: Word64)
    if voterId `Set.member` acc
      then
        go k acc
      else do
        rest <- go (k - 1) (Set.insert voterId acc)
        pure (voterId : rest)

-- | Generate a non-empty random stake distribution of a given size
genStakeDistr :: Int -> Gen StakeDistr
genStakeDistr size = do
  ids <- genUniqueVoterIds (succ size)
  stakes <- vectorOf (succ size) genStake
  pure (mkStakeDistr (zip ids stakes))

-- ** Properties

-- | Stakes returned by 'stakeDistrToNonIncreasingStakeList' are non-increasing
prop_stakeDistrNonIncreasingStakeList :: Property
prop_stakeDistrNonIncreasingStakeList =
  forAll (sized genStakeDistr) $ \stakeDistr ->
    let stakes = fmap snd (stakeDistrToNonIncreasingStakeList stakeDistr)
     in and (zipWith (>=) stakes (tail stakes))

-- | Persistent seats returned by 'weightedFaitAccompliWith' never exceed the
-- requested number of seats.
--
-- NOTE: uses 'nullFallback' to avoid complicating the test.
prop_weightedFaitAccompliWith_persistentSeatsRespectNumSeats :: Property
prop_weightedFaitAccompliWith_persistentSeatsRespectNumSeats =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let maxSeats = fromIntegral (stakeDistrSize stakeDistr)
    forAll (fromInteger <$> choose (1, maxSeats)) $ \numSeats -> do
      let (persistentSeats, _) =
            weightedFaitAccompliWith nullFallback numSeats stakeDistr
      counterexample
        ("Persistent seats: " <> show persistentSeats)
        (stakeDistrSize persistentSeats <= fromIntegral numSeats)

-- | The stake of persistent seats returned by 'weightedFaitAccompliWith' is
-- never smaller than the stake of non-persistent seats before invoking the
-- given fallback scheme.
--
-- NOTE: uses 'idFallback' to compare against the actual (residual)
-- non-persistent stake distribution.
prop_weightedFaitAccompliWith_persistentSeatsHaveLargerStake :: Property
prop_weightedFaitAccompliWith_persistentSeatsHaveLargerStake =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let maxSeats = fromIntegral (stakeDistrSize stakeDistr)
    forAll (fromInteger <$> choose (1, maxSeats)) $ \numSeats -> do
      let (persistentSeats, nonPersistentSeats) =
            weightedFaitAccompliWith idFallback numSeats stakeDistr
      let persistentStakes = Map.elems (unStakeDistr persistentSeats)
      let nonPersistentStakes = Map.elems (unStakeDistr nonPersistentSeats)
      counterexample
        ( unlines
            [ "Persistent seats: " <> show persistentSeats
            , "Non-persistent seats: " <> show nonPersistentSeats
            , "Min persistent stake: " <> show (minimum persistentStakes)
            , "Max non-persistent stake: " <> show (maximum nonPersistentStakes)
            ]
        )
        $ not (null persistentStakes)
          && not (null nonPersistentStakes)
            ==> (minimum persistentStakes >= maximum nonPersistentStakes)

-- | The total stake for both persistent and non-persistent voters adds up to
-- the original total stake.
--
-- NOTE: since the fallback method might be non-deterministic, we use the
-- 'nullFallback' and instead compute its expected total stake directly.
prop_weightedFaitAccompliWith_totalStakePreserved :: Property
prop_weightedFaitAccompliWith_totalStakePreserved =
  undefined

-- forAll (sized genStakeDistr) $ \stakeDistr -> do
--   let maxSeats = fromIntegral (stakeDistrSize stakeDistr)
--   forAll (fromInteger <$> choose (1, maxSeats)) $ \numSeats -> do
--     let (persistentSeats, _) =
--           weightedFaitAccompliWith nullFallback numSeats stakeDistr
--     let totalOriginalStake = stakeDistrTotalStake stakeDistr
--     let totalPersistentStake = stakeDistrTotalStake persistentSeats
--     let totalNonPersistentStake =
--           totalOriginalStake - totalPersistentStake
--     counterexample
--       ( unlines
--           [ "Total original stake: " <> show totalOriginalStake
--           , "Total persistent stake: " <> show totalPersistentStake
--           , "Total non-persistent stake: " <> show totalNonPersistentStake
--           , "Sum of persistent and non-persistent stakes: "
--               <> show (totalPersistentStake + totalNonPersistentStake)
--           ]
--       )
--       ( totalOriginalStake
--           == totalPersistentStake + totalNonPersistentStake
--       )
