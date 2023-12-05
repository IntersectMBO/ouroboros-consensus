{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrink (shrinkPointSchedule) where

import           Data.Function ((&))
import           Data.List.NonEmpty (toList)
import           Data.Maybe (mapMaybe)
import           Test.Consensus.PointSchedule (pointSchedule, PointSchedule (..))
import           Test.QuickCheck (shrinkList)

-- | Shrink a 'PointSchedule' in a generic way.
shrinkPointSchedule :: PointSchedule -> [PointSchedule]
shrinkPointSchedule PointSchedule{ticks, peerIds} =
  shrinkList (const []) (toList ticks)
    & mapMaybe (flip pointSchedule peerIds)
