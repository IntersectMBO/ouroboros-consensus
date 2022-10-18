{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}

-- | Utilities to schedule actions per 'Tick'.
module Test.Util.Schedule (
    Schedule (..)
  , SchedulingStrategy (..)
  , genSchedule
  , joinSchedule
  , lastTick
  , shrinkSchedule
  ) where

import           Data.List (intercalate, unfoldr)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import           Test.QuickCheck

import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Test.Util.LogicalClock (Tick (..))

-- | A schedule plans actions on certain times.
--
-- TODO Note that a schedule can't express delays between the actions
-- within a single tick. Generating such delays may expose more (most
-- likely concurrency-related) bugs.
newtype Schedule a = Schedule { getSchedule :: Map Tick [a] }
  deriving stock (Show, Eq)

instance Condense a => Condense (Schedule a) where
  condense =
        unlines
      . map (uncurry showEntry)
      . filter (not . null . snd)
      . Map.toAscList
      . getSchedule
    where
      showEntry (Tick tick) as = show tick <> ": " <>
        intercalate ", " (map condense as)

-- | Return the last tick at which an update is planned, if no updates
-- are planned, return 0.
lastTick :: Schedule a -> Tick
lastTick = fromMaybe (Tick 0) . maxKey . getSchedule
  where
    maxKey :: forall k v. Map k v -> Maybe k
    maxKey = fmap (fst . fst) . Map.maxViewWithKey

-- | A scheduling strategy specifies how to distribute elements across a
-- sequence of ticks.
data SchedulingStrategy =
     -- | The default strategy. Most ticks will have no associated elements, but
     -- if they do, there can be multiple.
     DefaultSchedulingStrategy
   | -- | Like 'DefaultSchedulingStrategy', but with at most one element per
     -- tick.
     SingleItemPerTickStrategy
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Spread out elements over a schedule, i.e. schedule a number of
-- elements to be processed on each tick. Most ticks will have no
-- associated elements.
genSchedule :: SchedulingStrategy -> [a] -> Gen (Schedule a)
genSchedule strat = fmap Schedule . go Map.empty 1
  where
    go :: Map Tick [a]
       -> Tick
       -> [a]
       -> Gen (Map Tick [a])
    go !schedule tick as
      | null as = return schedule
      | otherwise    = do
        nbAs <- genNumElemsPerTick
        let (this, rest) = splitAt nbAs as
        go (Map.insert tick this schedule) (succ tick) rest

    genNumElemsPerTick = case strat of
        DefaultSchedulingStrategy ->
          frequency [ (2, return 0), (1, choose (1, 5)) ]
        SingleItemPerTickStrategy -> elements [0, 1]

-- | Repeatedly remove the last entry (highest 'Tick')
shrinkSchedule :: Schedule a -> [Schedule a]
shrinkSchedule =
      unfoldr (fmap (\(_, m) -> (Schedule m, m)) . Map.maxView)
    . getSchedule

-- | Inverse of 'genSchedule'
joinSchedule :: Schedule a -> [a]
joinSchedule = concatMap snd . Map.toAscList . getSchedule
