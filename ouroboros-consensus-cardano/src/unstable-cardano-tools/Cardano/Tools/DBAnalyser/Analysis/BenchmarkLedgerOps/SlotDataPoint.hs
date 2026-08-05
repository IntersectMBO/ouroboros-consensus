{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint
  ( BlockStats (BlockStats, unBlockStats)
  , SlotDataPoint (..)
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import Data.Int (Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import TextBuilder (TextBuilder)
import qualified TextBuilder as Builder

-- | Information about the time spent processing the block corresponding to
-- 'slot', divided into the five major operations:
--
--  0. Forecast.
--  1. Header tick.
--  2. Header application.
--  3. Block tick.
--  4. Block application.
--
-- Before those five operations run, the node reads the ledger tables
-- that the block needs (e.g. the on-disk backend's UTxO-table, if the
-- on-disk backend is used). This is the tables read. 'totalTime',
-- 'mut', 'gc', 'majGcCount', 'minGcCount' and 'allocatedBytes' cover
-- the tables read and the five operations together. 'tableReadTime'
-- and 'mut_tableRead' report the costs of table read on its own.
--
-- It is up to the user of a slot data point to decide which units the data
-- represent (eg milliseconds, nanoseconds, etc)
data SlotDataPoint
  = SlotDataPoint
  { slot :: !SlotNo
  -- ^ Slot in which the 5 ledger operations were applied.
  , slotGap :: !Word64
  -- ^ Gap to the previous slot.
  , totalTime :: !Int64
  -- ^ Elapsed time spent on the tables read and the 5 ledger
  -- operations at 'slot'. Taken from GC.elapsed_ns.
  , mut :: !Int64
  -- ^ Time the mutator ran during the tables read and the 5 ledger
  -- operations at 'slot'. Taken from GC.mutator_elapsed_ns.
  , gc :: !Int64
  -- ^ Time spent in garbage collection during the tables read and the
  -- 5 ledger operations at 'slot'. Taken from GC.gc_elapsed_ns.
  , tableReadTime :: !Int64
  -- ^ Elapsed time spent on the tables read. 'totalTime' already
  -- counts this time. To get the time the 5 operations took on their
  -- own, subtract this from 'totalTime'.
  , mut_tableRead :: !Int64
  -- ^ Time the mutator ran during the tables read. Taken from
  -- GC.mutator_elapsed_ns, which is elapsed time minus GC time.
  -- 'tableReadTime' minus this value is the GC time inside the table
  -- read.
  , majGcCount :: !Word32
  -- ^ Total number of __major__ garbage collections that took place
  -- during the tables read and the 5 ledger operations at 'slot'.
  , minGcCount :: !Word32
  -- ^ Total number of __minor__ garbage collections that took place
  -- during the tables read and the 5 ledger operations at 'slot'.
  , allocatedBytes :: !Word64
  -- ^ Allocated bytes during the tables read and the 5 ledger
  -- operations at 'slot'.
  , mut_forecast :: !Int64
  -- ^ Difference of the GC.mutator_elapsed_ns field when computing the
  -- forecast.
  , mut_headerTick :: !Int64
  , mut_headerApply :: !Int64
  , mut_blockTick :: !Int64
  , mut_blockApply :: !Int64
  , blockByteSize :: !Word32
  , blockStats :: !BlockStats
  -- ^ Free-form information about the block.
  }
  deriving (Generic, Show)

newtype BlockStats = BlockStats {unBlockStats :: [TextBuilder]}
  deriving (Generic, Show)

instance ToJSON BlockStats where
  -- We convert the blocks stats to a 'Vector Text'.
  toJSON = toJSON . fmap Builder.toText . unBlockStats

  toEncoding = Aeson.Encoding.list (Aeson.Encoding.text . Builder.toText) . unBlockStats

instance ToJSON SlotDataPoint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
