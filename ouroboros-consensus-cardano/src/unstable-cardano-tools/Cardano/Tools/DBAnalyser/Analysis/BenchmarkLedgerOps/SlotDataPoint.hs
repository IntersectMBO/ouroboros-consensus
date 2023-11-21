{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint (
    BlockStats (BlockStats, unBlockStats)
  , SlotDataPoint (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo)
import           Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import           Data.Aeson.Types as Aeson.Types
import           Data.Int (Int64)
import           Data.Word (Word32, Word64)
import           GHC.Exts (toList)
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
        -- | Total number of major garbage collections that took place while
        -- performing the 5 ledger operations at 'slot'.
      , majGcCount      :: !Word32
        -- | Difference of the GC.mutator_elapsed_ns field when computing the
        -- forecast.
      , mut_forecast    :: !Int64
      , mut_headerTick  :: !Int64
      , mut_headerApply :: !Int64
      , mut_blockTick   :: !Int64
      , mut_blockApply  :: !Int64
      -- | Free-form information about the block.
      , blockStats      :: !BlockStats
      } deriving (Generic, Show, Eq)

newtype BlockStats = BlockStats { unBlockStats :: [Builder] }
  deriving (Generic, Show)

instance Eq BlockStats where
  BlockStats st0 == BlockStats st1 = fmap Builder.run st0 == fmap Builder.run st1

instance ToJSON BlockStats where
  -- We convert the blocks stats to a 'Vector Text'.
  toJSON = toJSON . fmap Builder.run . unBlockStats

  toEncoding = Aeson.Encoding.list (Aeson.Encoding.text . Builder.run) . unBlockStats

instance FromJSON BlockStats where
  parseJSON (Aeson.Array v) =
    BlockStats . toList . fmap Builder.text  <$> traverse parseJSON v
  parseJSON value           =
    Aeson.Types.prependFailure
      "Parsing Block stats failed: " (Aeson.Types.typeMismatch "Array" value)

instance ToJSON SlotDataPoint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON SlotDataPoint
