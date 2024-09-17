{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Bench.Consensus.ChainSyncClient.Driver (mainWith) where

import           Control.Monad (when)
import qualified Data.Array.IO as UA
import           Data.Int (Int64)
import           Data.Time.Clock (diffTimeToPicoseconds)
import qualified Data.Time.Clock.System as SysTime
import qualified Data.Time.Clock.TAI as TAI
import           Data.Word (Word32)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

-- | The argument to the function under test
--
-- This is comparable to the "size" argument in QuickCheck.
newtype X = X Int64

{-# INLINE mainWith #-}
mainWith :: (Int64 -> IO ()) -> IO ()
mainWith fut = do
    xx <- getArgs >>= \case
        [loStr, hiStr]
          | Just lo <- readMaybe loStr
          , Just hi <- readMaybe hiStr
          -> pure (X lo, X hi)
        _ -> fail "Pass min and max index as arguments $1 and $2"

    let zz@(!lo, !hi) = mkMeasurementIndexInterval xx

    -- all the extraneous allocation happens up front
    --
    -- TODO except the getSystemTime FFI overhead. I haven't find a
    -- platform-agnostic package for reading the clock into a pre-allocated
    -- buffer (eg via Storable)
    varStarts <- newTimeArray_ zz
    varStops  <- newTimeArray_ zz

    let go !z = do
            recordTimeArray varStarts z
            let X i = fst $ coords z in fut i
            recordTimeArray varStops  z
            when (z < hi) $ go (incrMeasurementIndex z)
    go lo

    -- and all the rendering overhead happens after the fact
    render varStarts varStops zz

-----

-- | 0-based index into the samples for a specific 'X' value
newtype RepetitionIndex = RepetitionIndex Int64

-- | 0-based index into the cross product @'X' * 'RepetitionIndex'@
newtype MeasurementIndex = MeasurementIndex Int64
  deriving (Eq, Ord, UA.Ix)

-- | Reduces variance by the sqrt of this, if the distribution is normal
samplesPerX :: Int64
samplesPerX = 1000

-- | The interval of 'MeasurentIndexes' necessary to represent 'samplesPerX'
-- samples of each 'X' in the given interval
mkMeasurementIndexInterval :: (X, X) -> (MeasurementIndex, MeasurementIndex)
mkMeasurementIndexInterval (X lo, X hi) =
    ( MeasurementIndex (firstSample lo)
    , MeasurementIndex (firstSample (hi + 1) - 1)
    )
  where
    firstSample x = samplesPerX * x

-- | The inverse of the mapping that underlies 'mkMeasurementIndexInterval'
coords :: MeasurementIndex -> (X, RepetitionIndex)
coords (MeasurementIndex z) =
    (X q, RepetitionIndex r)
  where
    (q, r) = z `quotRem` samplesPerX

-- | Increment
incrMeasurementIndex :: MeasurementIndex -> MeasurementIndex
incrMeasurementIndex (MeasurementIndex z) = MeasurementIndex $ z + 1

-----

-- | Dump all measurements to the screen, along with their coordinates
render :: TimeArray -> TimeArray -> (MeasurementIndex, MeasurementIndex) -> IO ()
render varStarts varStops zz = do
    putStrLn "# Uuid RelativeSample Index Nanoseconds"
    mapM_ each (UA.range zz)
  where
    each z = do
        let (X x, RepetitionIndex y) = coords z
        start <- readTimeArray varStarts z
        stop  <- readTimeArray varStops  z
        let dur = stop `diffPico` start
        putStrLn $ unwords [let MeasurementIndex i = z in show i, show y, show x, show dur]

-----

-- | An array of wall clock measurements
--
-- Unboxed arrays to minimize the resource usage of the benchmark
-- driver/harness.
data TimeArray = TimeArray !(UA.IOUArray MeasurementIndex Int64) !(UA.IOUArray MeasurementIndex Word32)

newTimeArray_ :: (MeasurementIndex, MeasurementIndex) -> IO TimeArray
newTimeArray_ zz = do
    seconds <- UA.newArray_ zz
    nanos   <- UA.newArray_ zz
    pure $ TimeArray seconds nanos

{-# INLINE recordTimeArray #-}
recordTimeArray :: TimeArray -> MeasurementIndex -> IO ()
recordTimeArray tarr = \z -> do
    tm <- SysTime.getSystemTime
    let SysTime.MkSystemTime {
            SysTime.systemSeconds     = a
          , SysTime.systemNanoseconds = b
          } = tm
    UA.writeArray seconds z a
    UA.writeArray nanos   z b
  where
    TimeArray seconds nanos = tarr

readTimeArray :: TimeArray -> MeasurementIndex -> IO SysTime.SystemTime
readTimeArray tarr = \z -> do
    a <- UA.readArray seconds z
    b <- UA.readArray nanos   z
    pure SysTime.MkSystemTime {
        SysTime.systemSeconds     = a
      , SysTime.systemNanoseconds = b
      }
  where
    TimeArray seconds nanos = tarr

diffPico :: SysTime.SystemTime -> SysTime.SystemTime -> Integer
diffPico stop start =
    diffTimeToPicoseconds $ stop' `TAI.diffAbsoluteTime` start'
  where
    stop'  = SysTime.systemToTAITime stop
    start' = SysTime.systemToTAITime start
