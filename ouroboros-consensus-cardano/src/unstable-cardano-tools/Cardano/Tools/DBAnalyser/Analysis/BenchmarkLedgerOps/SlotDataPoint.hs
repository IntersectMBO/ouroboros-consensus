{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint (
    BlockStats (BlockStats, unBlockStats)
  , SlotDataPoint (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo)
import           Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import           Data.Int (Int64)
import           Data.Word (Word32, Word64)
import           GHC.Generics (Generic)
import qualified Text.Builder as Builder
import           Text.Builder (Builder)

-- | Information about the time spent processing the block corresponding to
-- 'slot', divided into the five major operations:
--
--  0. Forecast.
--  1. Header tick.
--  2. Header application.
--  3. Block tick.
--  4. Block application.
--
-- It is up to the user of a slot data point to decide which units the data
-- represent (eg milliseconds, nanoseconds, etc)
data SlotDataPoint =
    SlotDataPoint
      { -- | Slot in which the 5 ledger operations were applied.
        slot            :: !SlotNo
        -- | Gap to the previous slot.
      , slotGap         :: !Word64
        -- | Total time spent in the 5 ledger operations at 'slot'.
      , totalTime       :: !Int64
        -- | Time spent by the mutator while performing the 5 ledger operations
        -- at 'slot'.
      , mut             :: !Int64
        -- | Time spent in garbage collection while performing the 5 ledger
        -- operations at 'slot'.
      , gc              :: !Int64
        -- | Total number of __major__ garbage collections that took place while
        -- performing the 5 ledger operations at 'slot'.
      , majGcCount      :: !Word32
        -- | Total number of __minor__ garbage collections that took place while
        -- performing the 5 ledger operations at 'slot'.
      , minGcCount      :: !Word32
        -- | Allocated bytes while performing the 5 ledger operations
        -- at 'slot'.
      , allocatedBytes  :: !Word64
        -- | Difference of the GC.mutator_elapsed_ns field when computing the
        -- forecast.
      , mut_forecast    :: !Int64
      , mut_headerTick  :: !Int64
      , mut_headerApply :: !Int64
      , mut_blockTick   :: !Int64
      , mut_blockApply  :: !Int64
      -- | Free-form information about the block.
      , blockStats      :: !BlockStats
      } deriving (Generic, Show)

newtype BlockStats = BlockStats { unBlockStats :: [Builder] }
  deriving (Generic, Show)

instance ToJSON BlockStats where
  -- We convert the blocks stats to a 'Vector Text'.
  toJSON = toJSON . fmap Builder.run . unBlockStats

  toEncoding = Aeson.Encoding.list (Aeson.Encoding.text . Builder.run) . unBlockStats

instance ToJSON SlotDataPoint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
