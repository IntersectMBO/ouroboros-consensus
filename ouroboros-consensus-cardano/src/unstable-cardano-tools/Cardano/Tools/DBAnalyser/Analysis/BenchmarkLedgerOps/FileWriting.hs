{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.FileWriting (
    -- * Output format
    OutputFormat
  , getOutputFormat
    -- * File writing functions
  , writeDataPoint
  , writeHeader
  , writeMetadata
  ) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo))
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.Metadata as BenchmarkLedgerOps.Metadata
import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint
                     (SlotDataPoint)
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as DP
import qualified Cardano.Tools.DBAnalyser.CSV as CSV
import           Cardano.Tools.DBAnalyser.Types (LedgerApplicationMode)
import           Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath.Posix (takeExtension)
import qualified System.IO as IO
import qualified Text.Builder as Builder
import           Text.Builder (Builder, decimal)

{-------------------------------------------------------------------------------
  Output format
-------------------------------------------------------------------------------}

data OutputFormat = CSV | JSON
  deriving (Show, Eq)

-- | Use the provided 'Maybe FilePath' to determine the output format.
--
-- The output format is based on the extension (see 'OutputFormat').
--
-- If the extension is not supported the output format defaults to
-- CSV, and this function prints a warning informing the user of this
-- choice.
getOutputFormat :: Maybe FilePath -> IO OutputFormat
getOutputFormat (Just filePath) =
    case takeExtension filePath of
    ".csv"  -> pure CSV
    ".json" -> pure JSON
    ext     -> do
      IO.hPutStr IO.stderr $ "Unsupported extension '" <> ext <> "'. Defaulting to CSV."
      pure CSV
getOutputFormat Nothing         = pure CSV


{-------------------------------------------------------------------------------
  File writing functions
-------------------------------------------------------------------------------}

-- | Separator used for CSV output.
csvSeparator :: Builder
csvSeparator = "\t"

-- | Write a header for the data points.
--
-- This is only needed for the CSV output format.
--
-- The position of each header matches the position in which the corresponding
-- field value is written in 'writeDatapoint'. Eg, if 'writeHeader' writes:
--
-- > "slot slotGap totalTime" ...
--
-- then the third value written by 'writeDataPoint' will correspond to 'totalTime'.
--
writeHeader :: IO.Handle -> OutputFormat -> IO ()
writeHeader outFileHandle CSV  =
    CSV.writeHeaderLine outFileHandle (CSV.Separator csvSeparator) dataPointCsvBuilder
writeHeader _             JSON = pure ()

-- | NOTE: This function is not thread safe.
writeDataPoint ::
     IO.Handle
  -> OutputFormat
  -> SlotDataPoint
  -> IO ()
writeDataPoint outFileHandle CSV  slotDataPoint =
    CSV.computeAndWriteLinePure
        outFileHandle (CSV.Separator csvSeparator) dataPointCsvBuilder slotDataPoint
writeDataPoint outFileHandle JSON slotDataPoint =
    BSL.hPut outFileHandle $ Aeson.encode slotDataPoint

-- | Write metadata to a JSON file if this is the selected
-- format. Perform a no-op otherwise.
writeMetadata :: IO.Handle -> OutputFormat -> LedgerApplicationMode -> IO ()
writeMetadata _outFileHandle CSV _lgrAppMode = pure ()
writeMetadata  outFileHandle JSON lgrAppMode =
  BenchmarkLedgerOps.Metadata.getMetadata lgrAppMode
  >>= BSL.hPut outFileHandle . Aeson.encode

{-------------------------------------------------------------------------------
  Operations to assist CSV printing
-------------------------------------------------------------------------------}

dataPointCsvBuilder :: [(Builder, SlotDataPoint -> Builder)]
dataPointCsvBuilder =
    [ ("slot"                  , decimal . unSlotNo . DP.slot)
    , ("slotGap"               , decimal . DP.slotGap)
    , ("totalTime"             , decimal . DP.totalTime)
    , ("mut"                   , decimal . DP.mut)
    , ("gc"                    , decimal . DP.gc)
    , ("majGcCount"            , decimal . DP.majGcCount)
    , ("minGcCount"            , decimal . DP.minGcCount)
    , ("allocatedBytes"        , decimal . DP.allocatedBytes)
    , ("mut_forecast"          , decimal . DP.mut_forecast)
    , ("mut_headerTick"        , decimal . DP.mut_headerTick)
    , ("mut_headerApply"       , decimal . DP.mut_headerApply)
    , ("mut_blockTick"         , decimal . DP.mut_blockTick)
    , ("mut_blockApply"        , decimal . DP.mut_blockApply)
    , ("blockBytes"            , decimal . DP.blockByteSize)
    , ("...era-specific stats" , Builder.intercalate csvSeparator . DP.unBlockStats . DP.blockStats)
    ]
