{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Model implementing committee selection based on the Fait Accompli scheme.
--
-- This is based on the paper:
--
-- Peter Gaži, Aggelos Kiayias, and Alexander Russell. 2023. Fait Accompli
-- Committee Selection: Improving the Size-Security Tradeoff of Stake-Based
-- Committees. In Proceedings of the 2023 ACM SIGSAC Conference on Computer and
-- Communications Security (CCS '23). Association for Computing Machinery, New
-- York, NY, USA, 845–858. https://doi.org/10.1145/3576915.3623194
--
-- PDF: https://eprint.iacr.org/2023/1273.pdf
module Test.Consensus.Peras.Committee.Model
  ( -- * Auxiliary types
    StakeRole (..)
  , StakeType (..)
  , SeatIndex
  , VoterId
  , NumSeats
  , IsStake (..)

    -- * Stake Distribution
  , StakeDistr
  , stakeDistrToDecreasingStakes
  , stakeDistrTotalStake

    -- * Fait Accompli Committee Selection
  , weightedFaitAccompliWith
  , weightedFaitAccompliPersistentSeats
  , weightedFaitAccompliThreshold
  , localSortition
  , makeStakeDistrVotingGlobal

    -- ** Random sampling helpers
  , poisson

    -- * Property tests
  , genStake
  , genUniqueVoterIds
  , genStakeDistr
  , prop_stakeDistrDecreasingStakes
  , prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats
  , prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveHaveLargeStake
  , prop_weightedFaitAccompliPersistentSeats_totalStakePreserved
  , prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect
  , prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify
  , tests
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Traversable (mapAccumR)
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Numeric.Natural (Natural, minusNaturalMaybe)
import System.Random (RandomGen)
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Stateful (runStateGen)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , Testable (..)
  , choose
  , counterexample
  , forAll
  , sized
  , tabulate
  , vectorOf
  , (===)
  , (==>)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-------------------------------------------------------------------------------

-- * Auxiliary types

-- | The role of a given stake.
--
-- NOTE: used mostly to better document type signatures in this module.
data StakeRole
  = -- | Stake as recorded on the ledger.
    -- Used for inputs to the committee selection algorithm.
    Ledger
  | -- | Stake used when casting a new vote.
    -- This may be derived from the ledger stake via some transformation.
    -- Used for outputs of the committee selection algorithm.
    Voting
  deriving Show

-- | Extra type information about a given stake.
--
-- NOTE: used mostly to better document type signatures in this module.
data StakeType = Global | Persistent | Residual | Cumulated
  deriving Show

-- | Number of seats in the committee
type NumSeats :: StakeType -> Type
type family NumSeats st

-- | Seat index (0-based)
type SeatIndex = Natural

-- | Voter identifier
type VoterId = Word64

-- ** Ledger and voting stakes

-- | Stake types annotated with their role and type information
data family Stake :: StakeRole -> StakeType -> Type

-- | Type class with operations needed over stakes
class (Num stake, Ord stake) => IsStake stake where
  stakeToRational :: stake -> Rational
  rationalToStake :: Rational -> stake

-- | DerivingVia helper to implement 'IsStake' instances for different stake
-- types via a common underlying representation (Rational).
newtype RationalStake (sr :: StakeRole) (st :: StakeType)
  = RationalStake Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

instance IsStake (RationalStake sr st) where
  stakeToRational (RationalStake r) = r
  rationalToStake = RationalStake

-- | Adjust a stake via a function on rationals
adjustStake ::
  forall stake' stake.
  ( IsStake stake
  , IsStake stake'
  ) =>
  (Rational -> Rational) ->
  stake ->
  stake'
adjustStake f stake =
  rationalToStake (f (stakeToRational stake))

-- | Retag a stake with a different origin and type.
--
-- NOTE: type parameters are arranged so that one can use type applications
-- to specify the parameters of the resulting type first.
coerceStake ::
  forall stake' stake.
  ( IsStake stake
  , IsStake stake'
  ) =>
  stake ->
  stake'
coerceStake =
  adjustStake id

-- ** Stake distributions

-- | Stake distribution mapping voters to their stakes
type StakeDistr (sr :: StakeRole) (st :: StakeType) = Map VoterId (Stake sr st)

-- | Get the total stake in a stake distribution
stakeDistrTotalStake ::
  forall sr st.
  IsStake (Stake sr st) =>
  StakeDistr sr st ->
  Stake sr st
stakeDistrTotalStake distr =
  sum (Map.elems distr)

-- | View a stake distribution as a non-increasing list of stakes
stakeDistrToDecreasingStakes ::
  IsStake (Stake sr st) =>
  StakeDistr sr st ->
  [(VoterId, Stake sr st)]
stakeDistrToDecreasingStakes distr =
  sortBy (comparing (Down . snd)) (Map.toList distr)

-------------------------------------------------------------------------------

-- * Fait Accompli Committee Selection

-- | Weighted Fait Accompli committee selection with a fallback mechanism.
--
-- This is mostly just plumbing to connect the result of
-- 'weightedFaitAccompliPersistentSeats' with a given fallback scheme.
weightedFaitAccompliWith ::
  -- | Fallback function for non-persistent seats
  ( NumSeats 'Residual ->
    StakeDistr 'Ledger 'Residual ->
    StakeDistr 'Voting 'Residual
  ) ->
  -- | Total number of seats
  NumSeats 'Global ->
  -- | Stake distribution
  StakeDistr 'Ledger 'Global ->
  -- | Selected persistent and non-persistent seats with their corresponding
  -- voting stakes
  ( StakeDistr 'Voting 'Persistent
  , StakeDistr 'Voting 'Residual
  )
weightedFaitAccompliWith fallback globalNumSeats stakeDistr =
  (persistentSeats, nonPersistentSeats)
 where
  (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
    weightedFaitAccompliPersistentSeats
      globalNumSeats
      stakeDistr

  nonPersistentSeats =
    fallback
      numNonPersistentSeats
      residualStakeDistr

-- | First step of the weighted Fait Accompli committee selection.
--
-- This step selects the persistent seats deterministically via the weighted
-- Fait Accompli threshold, and returns the remaining stake distribution for the
-- non-persistent seats selection.
weightedFaitAccompliPersistentSeats ::
  NumSeats 'Global ->
  StakeDistr 'Ledger 'Global ->
  ( StakeDistr 'Voting 'Persistent
  , NumSeats 'Residual
  , StakeDistr 'Ledger 'Residual
  )
weightedFaitAccompliPersistentSeats globalNumSeats stakeDistr
  | globalNumSeats <= 0 =
      error "Number of seats must be positive"
  | Map.size stakeDistr == 0 =
      error "Stake distribution cannot be empty"
  | sum stakeDistr == 0 =
      error "Total stake must be positive"
  | otherwise =
      (persistentSeats, numNonPersistentSeats, residualStakeDistr)
 where
  -- Persistent seats selected deterministically based on their ledger stake.
  -- NOTE: their voting stake is *exactly* their ledger stake.
  persistentSeats =
    voterListToStakeDistr persistentVotersWithStake

  -- Residual stake distribution for the non-persistent seats selection. This
  -- includes all the voters that did not qualify for a persistent seat, with
  -- their original ledger stake.
  residualStakeDistr =
    voterListToStakeDistr residualVotersWithStake

  -- Number of non-persistent seats to be selected via a fallback scheme
  numNonPersistentSeats =
    case globalNumSeats `minusNaturalMaybe` numPersistentSeats of
      Just n -> n
      Nothing -> error "Number of persistent seats exceeds total seats"
   where
    numPersistentSeats = fromIntegral (length persistentVotersWithStake)

  -- Split the cumulative stakes into persistent and non-persistent seats
  (persistentVotersWithStake, residualVotersWithStake) =
    bimap
      (mapStakes (coerceStake @(Stake Voting Persistent)))
      (mapStakes (coerceStake @(Stake Ledger Residual)))
      $ span
        ( \(_, voterStake, seatIndex, stakeAccum) ->
            weightedFaitAccompliThreshold
              globalNumSeats
              seatIndex
              voterStake
              stakeAccum
        )
        rightCumulativeDecreasingStakes
   where
    mapStakes f = fmap (\(x, s, y, z) -> (x, f s, y, z))

  -- Stake distribution in non-increasing order, including voter seat index and
  -- accumulated stakes from the right, i.e., from all the voters with smaller
  -- stake than the current one.
  rightCumulativeDecreasingStakes =
    accumStakeR $
      addSeatIndex $
        stakeDistrToDecreasingStakes $
          stakeDistr
   where
    -- Add an increasing seat index to each candidate
    addSeatIndex =
      zipWith
        (\seatIndex (vId, vStake) -> (vId, vStake, seatIndex))
        [0 ..]
    -- Accumulated stake of every candidate beyond (and including) this one
    accumStakeR xs =
      snd $
        mapAccumR
          ( \stakeAccR (voterId, voterStake, seatIndex) ->
              let stakeAccR' =
                    stakeAccR
                      -- We coerce from global to cumulated stake here
                      + coerceStake voterStake
               in ( stakeAccR'
                  , (voterId, voterStake, seatIndex, stakeAccR')
                  )
          )
          0
          xs

  -- Drop the accumulated information to get back to a stake distribution
  voterListToStakeDistr =
    Map.fromList
      . fmap (\(voterId, stake, _, _) -> (voterId, stake))

-- | Weighted Fait Accompli threshold for persistent seats
--
-- For more details, see (Section 4, Figure 7) of the paper:
-- https://eprint.iacr.org/2023/1273.pdf
weightedFaitAccompliThreshold ::
  -- | Total number of seats
  NumSeats 'Global ->
  -- | Voter seat index (0-based)
  SeatIndex ->
  -- | Voter ledger stake
  Stake 'Ledger 'Global ->
  -- | Cumulated ledger stake from the right (including the current voter stake)
  Stake 'Ledger 'Cumulated ->
  -- | Whether the voter qualifies for a persistent seat
  Bool
weightedFaitAccompliThreshold numSeats voterSeat voterStake stakeAccR
  | stakeToRational stakeAccR <= 0 =
      False
  | otherwise =
      ( 1
          - ( stakeToRational voterStake
                / stakeToRational stakeAccR
            )
      )
        ^ (2 :: Integer)
        < toRational (numSeats - voterSeat)
          / toRational (numSeats - voterSeat + 1)

-- | Local sortition fallback
localSortition ::
  ( IsStake (Stake 'Ledger st)
  , IsStake (Stake 'Voting st)
  , Integral (NumSeats st)
  ) =>
  RandomGen rng =>
  -- | Random number generator
  rng ->
  -- | Expected number of non-persistent seats
  NumSeats st ->
  -- | Non-persistent stake distribution candidated (not necessarily normalized)
  StakeDistr 'Ledger st ->
  -- | Selected non-persistent seats with their correspoinding voting stakes
  StakeDistr 'Voting st
localSortition rng0 numSeats stakeDistr =
  Map.fromList $
    sampleNonPersistentVoters $
      Map.toList $
        stakeDistr
 where
  -- The total stake of the non-persistent candidate stake distribution
  totalStake = stakeDistrTotalStake stakeDistr
  -- All non-persistent voters share the same voting "weight"
  seatStake = rationalToStake (stakeToRational totalStake / fromIntegral numSeats)
  -- Sample non-persistent voter seats independently to emulate local sortition
  sampleNonPersistentVoters = snd . foldr sampleVoterStake (rng0, [])
  -- Sample voter seats for each voter independently to emulate local sortition
  sampleVoterStake (voterId, stake) (rng, acc) =
    let
      normalizedStake = stakeToRational stake / stakeToRational totalStake
      -- Expected number of seats for this voter
      lambda = fromIntegral numSeats * fromRational normalizedStake
      -- Sample number of seats using Poisson distribution
      (numSeatsForVoter, rng') = poisson lambda rng
      -- Voting stake of this voter equally divided
      --
      -- NOTE: if desired we could also scale this by the number of seats this
      -- voter was granted via the Poisson sampling.
      votingStake = fromIntegral numSeatsForVoter * seatStake
     in
      ( -- Thread the updated random generator
        rng'
      , -- Accumulate voter if they won any seats
        if numSeatsForVoter > 0 then (voterId, votingStake) : acc else acc
      )

-- | Combine the resulting persistent and non-persistent stake distributions
-- into a single global one
makeStakeDistrVotingGlobal ::
  StakeDistr 'Voting 'Persistent ->
  StakeDistr 'Voting 'Residual ->
  StakeDistr 'Voting 'Global
makeStakeDistrVotingGlobal persistent residual =
  Map.unionWith
    (+)
    (coerceStake @(Stake Voting Global) <$> persistent)
    (coerceStake @(Stake Voting Global) <$> residual)

-- ** Random sampling helpers

-- | Wrapper over 'mwc-random' to sample from a Poisson distribution using an
-- explicit seed.
poisson :: RandomGen rng => Double -> rng -> (Natural, rng)
poisson lambda rng0
  | lambda < 0 =
      error "Poisson's lambda parameter must be non-negative"
  | otherwise =
      first fromIntegral $
        runStateGen rng0 $
          MWC.poisson lambda

-------------------------------------------------------------------------------

-- * Boilerplate instances

type instance NumSeats 'Global = Natural

type instance NumSeats 'Persistent = Natural

type instance NumSeats 'Residual = Natural

newtype instance Stake 'Ledger 'Global = StakeLedgerGlobal Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Ledger 'Persistent = StakeLedgerPersistent Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Ledger 'Residual = StakeLedgerResidual Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Ledger 'Cumulated = StakeLedgerCumulated Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Voting 'Global = StakeVotingGlobal Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Voting 'Persistent = StakeVotingPersistent Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake 'Voting 'Residual = StakeVotingResidual Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

deriving via (RationalStake 'Ledger 'Global) instance IsStake (Stake 'Ledger 'Global)
deriving via (RationalStake 'Ledger 'Persistent) instance IsStake (Stake 'Ledger 'Persistent)
deriving via (RationalStake 'Ledger 'Residual) instance IsStake (Stake 'Ledger 'Residual)
deriving via (RationalStake 'Ledger 'Cumulated) instance IsStake (Stake 'Ledger 'Cumulated)
deriving via (RationalStake 'Voting 'Global) instance IsStake (Stake 'Voting 'Global)
deriving via (RationalStake 'Voting 'Persistent) instance IsStake (Stake 'Voting 'Persistent)
deriving via (RationalStake 'Voting 'Residual) instance IsStake (Stake 'Voting 'Residual)

-------------------------------------------------------------------------------

-- * Property tests

-- ** Generators

-- | Generate a random stake as a Rational number
genStake :: Gen (Stake 'Ledger 'Global)
genStake = do
  Positive num <- arbitrary
  Positive den <- arbitrary
  pure (StakeLedgerGlobal (num % den))

-- | Generate a non-empty list of unique voter IDs of a given size.
genUniqueVoterIds :: Int -> Gen [VoterId]
genUniqueVoterIds size =
  go size Set.empty
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
genStakeDistr :: Int -> Gen (StakeDistr 'Ledger 'Global)
genStakeDistr size = do
  ids <- genUniqueVoterIds (succ size)
  stakes <- vectorOf (succ size) genStake
  pure (Map.fromList (zip ids stakes))

-- * Property helpers

-- | Helper to generate stake distributions along with a number of seats that
-- could possibly exceed the number of nodes in the stake distribution.
forAllStakeDistrAndNumSeats ::
  (StakeDistr 'Ledger 'Global -> NumSeats 'Global -> Property) ->
  Property
forAllStakeDistrAndNumSeats p =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let numNodes = fromIntegral (Map.size stakeDistr)
    -- generate a number of seats that could possibly exceed the number of nodes
    let genNumSeats = fromInteger <$> choose (1, numNodes * 2)
    forAll genNumSeats $ \numSeats -> p stakeDistr numSeats

-- | Tabulate the ratio of persistent to non-persistent seats as the percentage
-- of persistent seats out of the total number of seats.
tabulatePersistentToNonPersistentRatio ::
  NumSeats 'Global ->
  NumSeats 'Global ->
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

-- ** Properties

-- | Stakes returned by 'stakeDistrToDecreasingStakes' are non-increasing
prop_stakeDistrDecreasingStakes :: Property
prop_stakeDistrDecreasingStakes =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let stakes = fmap snd (stakeDistrToDecreasingStakes stakeDistr)
    case stakes of
      [] -> True -- vacuously true for empty stake distributions
      (_ : rest) -> and (zipWith (>=) stakes rest)

-- | Persistent seats returned by 'weightedFaitAccompliPersistentSeats' never
-- exceed the requested number of seats.
prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats :: Property
prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let (persistentSeats, numNonPersistentSeats, _) =
          weightedFaitAccompliPersistentSeats numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    tabulatePersistentToNonPersistentRatio
      numPersistentSeats
      numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total requested seats: " <> show numSeats
            , "Number of persistent seats selected: " <> show numPersistentSeats
            , "Persistent seats selected: " <> show persistentSeats
            ]
        )
      $ numPersistentSeats <= numSeats

-- | The stake of a persistent seat returned by
-- 'weightedFaitAccompliPersistentSeats' is never smaller than the stake of any
-- of the remaining the nodes in the residual stake distribution.
prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveHaveLargeStake :: Property
prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveHaveLargeStake =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let persistentStakes = Map.elems persistentSeats
    let residualStakes = Map.elems residualStakeDistr
    tabulatePersistentToNonPersistentRatio
      numPersistentSeats
      numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Persistent seats: " <> show persistentSeats
            , "Residual stake distribution: " <> show residualStakeDistr
            , "Min persistent stake: " <> show (minimum persistentStakes)
            , "Max residual stake: " <> show (maximum residualStakes)
            ]
        )
      $ not (null persistentStakes)
        && not (null residualStakes)
      ==> minimum (stakeToRational <$> persistentStakes)
        >= maximum (stakeToRational <$> residualStakes)

-- | The total stake for the persistent are residual stake distributions
-- returned by 'weightedFaitAccompliPersistentSeats' adds up to the original
-- total stake.
prop_weightedFaitAccompliPersistentSeats_totalStakePreserved :: Property
prop_weightedFaitAccompliPersistentSeats_totalStakePreserved =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let totalOriginalStake = stakeDistrTotalStake stakeDistr
    let totalPersistentStake = stakeDistrTotalStake persistentSeats
    let totalResidualStake = stakeDistrTotalStake residualStakeDistr
    let totalCombinedStake =
          stakeToRational totalPersistentStake
            + stakeToRational totalResidualStake
    tabulatePersistentToNonPersistentRatio
      numPersistentSeats
      numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total original stake: " <> show totalOriginalStake
            , "Total persistent stake: " <> show totalPersistentStake
            , "Total residual stake: " <> show totalResidualStake
            , "Total combined stake: " <> show totalCombinedStake
            ]
        )
      $ stakeToRational totalOriginalStake === totalCombinedStake

-- | The number of non-persistent seats to be selected from the residual stake
-- distribution returned by 'weightedFaitAccompliPersistentSeats' adds up to the
-- original number of seats minus the number of persistent seats.
prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect :: Property
prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let (persistentSeats, numNonPersistentSeats, _) =
          weightedFaitAccompliPersistentSeats numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    tabulatePersistentToNonPersistentRatio
      numPersistentSeats
      numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total requested seats: " <> show numSeats
            , "Persistent seats selected: " <> show numPersistentSeats
            , "Non-persistent seats to select: " <> show numNonPersistentSeats
            ]
        )
      $ case minusNaturalMaybe numSeats numPersistentSeats of
        Just n -> n === numNonPersistentSeats
        Nothing -> property False

-- | None of the stakes in the residual stake distribution returned by
-- 'weightedFaitAccompliPersistentSeats' should qualify for the next persistent
-- seat according to the weighted Fait Accompli threshold.
prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify :: Property
prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats numSeats stakeDistr
    let numPersistentSeats =
          fromIntegral (Map.size persistentSeats)
    let addSeatIndex =
          flip zipWith [numPersistentSeats ..] $
            \seatIndex (voterId, voterStake) ->
              (voterId, voterStake, seatIndex)
    let accumStakeR =
          fmap snd $
            flip mapAccumR 0 $
              \stakeAccR (voterId, voterStake, seatIndex) ->
                let
                  stakeAccR' =
                    adjustStake (+ stakeToRational voterStake) stakeAccR
                 in
                  ( stakeAccR'
                  ,
                    ( voterId
                    , voterStake
                    , seatIndex
                    , stakeAccR'
                    )
                  )
    let rightCumulativeDecreasingResidualStakes =
          accumStakeR $
            addSeatIndex $
              stakeDistrToDecreasingStakes residualStakeDistr

    let judgments =
          fmap
            ( \(voterIndex, voterStake, seatIndex, stakeAccum) ->
                ( voterIndex
                , voterStake
                , seatIndex
                , stakeAccum
                , -- We still have remaining seats to allocate
                  (seatIndex < numSeats)
                    -- and this voter would qualify for a persistent seat
                    && weightedFaitAccompliThreshold
                      numSeats
                      seatIndex
                      -- Here we cast the stake from Residual to Global since
                      -- the Fait-Accompli threshold is supposed to be computed
                      -- on global stakes:
                      (coerceStake @(Stake Ledger Global) voterStake)
                      stakeAccum
                )
            )
            rightCumulativeDecreasingResidualStakes
    tabulatePersistentToNonPersistentRatio
      numPersistentSeats
      numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Persistent seats selected: "
                <> show persistentSeats
            , "Next persistent seat index: "
                <> show numPersistentSeats
            , "Residual stakes with index, cumulative stake, and threshold judgments: "
                <> show judgments
            ]
        )
      $ all
        (\(_, _, _, _, aboveThreshold) -> not aboveThreshold)
        judgments

-- | Test suite
tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "Peras committee selection model property tests"
      [ testProperty
          "prop_stakeDistrDecreasingStakes"
          prop_stakeDistrDecreasingStakes
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats"
          prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveHaveLargeStake"
          prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveHaveLargeStake
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_totalStakePreserved"
          prop_weightedFaitAccompliPersistentSeats_totalStakePreserved
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect"
          prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify"
          prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify
      ]
