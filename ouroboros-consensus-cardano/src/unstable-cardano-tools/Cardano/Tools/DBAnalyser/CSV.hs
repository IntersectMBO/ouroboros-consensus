{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | This module provides functionality for helping writing data values as CSV entries.
--
-- A couple of 'db-analyzer` analysis produce CSV files, which contain
-- the analysis' results. A way to populate these files is to write
-- the headers first, and then, line by line, write the rows that
-- contain the data. Each column in a row containing data must
-- correspond to a given header. To make it easier to maintain this
-- correspondence between headers and data, we usually specify a CSV
-- builder as:
--
-- > [(Builder, a -> IO Builder)]
--
-- where each first component of each tuple in the list represents a
-- header, and each second component determines how the value that
-- corresponds to that header is computed, given a certain value that
-- is needed to compute a row in the resulting CSV.
--
-- We use 'Text.Builder' to efficiently intercalate values with the CSV 'Separator'.
--
module Cardano.Tools.DBAnalyser.CSV (
    Separator (Separator, unSeparator)
  , computeAndWriteLine
  , computeAndWriteLinePure
  , computeColumns
  , computeColumnsPure
  , writeHeaderLine
  , writeLine
  ) where

import           Data.String (IsString)
import qualified Data.Text.IO as Text.IO
import qualified System.IO as IO
import qualified Text.Builder as Builder
import           Text.Builder (Builder)

newtype Separator = Separator { unSeparator :: Builder }
  deriving (Show, IsString, Monoid, Semigroup)

writeHeaderLine :: IO.Handle -> Separator -> [(Builder, a)] -> IO ()
writeHeaderLine handle (Separator separator) =
      Text.IO.hPutStrLn handle
    . Builder.run
    . Builder.intercalate separator
    . fmap fst

writeLine :: IO.Handle -> Separator -> [Builder] -> IO ()
writeLine handle (Separator separator) =
      Text.IO.hPutStrLn handle
    . Builder.run
    . Builder.intercalate separator

computeAndWriteLine :: IO.Handle -> Separator -> [(a, b -> IO Builder)] -> b -> IO ()
computeAndWriteLine handle separator csvBuilder b = do
  computeColumns (fmap snd csvBuilder) b >>= writeLine handle separator

computeAndWriteLinePure :: IO.Handle -> Separator -> [(a, b -> Builder)] -> b -> IO ()
computeAndWriteLinePure handle separator csvBuilder b =
    writeLine handle separator $ computeColumnsPure (fmap snd csvBuilder) b

computeColumns :: [a -> IO Builder] -> a -> IO [Builder]
computeColumns fBuilders a =
  sequence $ fmap ($ a) fBuilders

computeColumnsPure :: [a -> Builder] -> a -> [Builder]
computeColumnsPure fBuilders a =
  fmap ($ a) fBuilders
