{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint (
    SlotDataPoint (..)
  , showData
  , showHeaders
    -- * Write data points to file
  , DataPointOutputFormat (CSV, JSON)
  , writeDataPoint
  , writeHeader
  ) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo))
import           Data.Int (Int64)
import qualified Data.Text.IO as Text.IO
import           Data.Word (Word32, Word64)
import qualified System.IO as IO
import qualified Text.Builder as Builder
import           Text.Builder (Builder, decimal, intercalate)

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
      , blockStats      :: ![Builder]
      }

-- | Return the headers that correspond to the fields of 'SlotDataPoint'.
--
-- The position of each header matches the position in which the corresponding
-- field value is returned in 'showData'. Eg, if show headers returns:
--
-- > "slot slotGap totalTime" ...
--
-- then the third value returned by 'showData' will correspond to 'totalTime'.
showHeaders :: Builder -> Builder
showHeaders sep = intercalate sep $ fmap fst           showHeadersAndData

showData :: SlotDataPoint -> Builder -> Builder
showData dp sep = intercalate sep $ fmap (($ dp) . snd) showHeadersAndData

showHeadersAndData :: [(Builder, SlotDataPoint -> Builder)]
showHeadersAndData =
    [ ("slot"                  , decimal . unSlotNo . slot)
    , ("slotGap"               , decimal . slotGap)
    , ("totalTime"             , decimal . totalTime)
    , ("mut"                   , decimal . mut)
    , ("gc"                    , decimal . gc)
    , ("majGcCount"            , decimal . majGcCount)
    , ("mut_forecast"          , decimal . mut_forecast)
    , ("mut_headerTick"        , decimal . mut_headerTick)
    , ("mut_headerApply"       , decimal . mut_headerApply)
    , ("mut_blockTick"         , decimal . mut_blockTick)
    , ("mut_blockApply"        , decimal . mut_blockApply)
    , ("...era-specific stats" , Builder.intercalate separator . blockStats)
    ]

{-------------------------------------------------------------------------------
  Write data points to file
-------------------------------------------------------------------------------}

data DataPointOutputFormat = CSV | JSON
  deriving (Show, Eq)

-- | Separator used for CSV output.
separator :: Builder
separator = "\t"

-- | Write a header for the data points.
--
-- This is only needed for the CSV output format.
writeHeader :: IO.Handle -> DataPointOutputFormat -> IO ()
writeHeader outFileHandle CSV  =
     Text.IO.hPutStrLn outFileHandle
   $ Builder.run
   $ showHeaders separator
writeHeader _             JSON = pure ()

writeDataPoint ::
     IO.Handle
  -> DataPointOutputFormat
  -> SlotDataPoint
  -> IO ()
writeDataPoint outFileHandle CSV  slotDataPoint =
      Text.IO.hPutStrLn outFileHandle
    $ Builder.run
    $ showData slotDataPoint separator
writeDataPoint outFileHandle JSON slotDataPoint = undefined
