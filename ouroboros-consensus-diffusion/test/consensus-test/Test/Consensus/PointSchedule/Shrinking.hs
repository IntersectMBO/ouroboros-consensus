{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains all kind of utilities related to shrinking point
-- schedules. Some function allow shrinking generically while other require the
-- properties to be written in a specific way. Be sure to read the documentation
-- of the shrinking function that you use.

module Test.Consensus.PointSchedule.Shrinking (
    shrinkFullTableExceptLast
  , shrinkViaFullTable
  ) where

import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Consensus.PointSchedule (PointSchedule (..))
import           Test.Consensus.PointSchedule.FullTable
                     (FullTablePointSchedule (..),
                     activeFromFullTablePointSchedule,
                     fullTableFromActivePointSchedule)
import           Test.QuickCheck (shrinkList)

-- | Shrink a 'PointSchedule' by transforming it into a
-- 'FullTablePointSchedule', shrinking that full-table point schedule, and
-- transforming it back.
shrinkViaFullTable ::
  (FullTablePointSchedule -> [FullTablePointSchedule]) ->
  PointSchedule ->
  [PointSchedule]
shrinkViaFullTable shrinkFullTable ps@PointSchedule{ticks} =
    ps
      & fullTableFromActivePointSchedule
      & shrinkFullTable
      & map activeFromFullTablePointSchedule
      & filter (\PointSchedule{ticks=ticks'} -> length ticks' < length ticks)

-- | Shrink a full-table point schedule randomly except for the last line that
-- has to be constant. Keeping the last line guarantees the same final state of
-- the peers, which makes it a safe “blind” shrinker.
shrinkFullTableExceptLast :: FullTablePointSchedule -> [FullTablePointSchedule]
shrinkFullTableExceptLast FullTablePointSchedule{ftpsRows, ftpsPeerIds} =
    let (rows, lastRow) = extractLast ftpsRows
     in map (flip FullTablePointSchedule ftpsPeerIds . NonEmpty.fromList . (++ [lastRow])) (shrinkList (const []) rows)
  where
    extractLast :: NonEmpty a -> ([a], a)
    extractLast = go . NonEmpty.toList
      where
        go :: [a] -> ([a], a)
        go []       = error "extractLast"
        go [x]      = ([], x)
        go (x : xs) = first (x :) (go xs)
