{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark for the sqlite backend of 'LeiosDemoDb' mark-as-immutable and
-- garbage collection.
--
-- By default the benchmark populates a synthetic database with deterministic
-- data (as in @leios-db-bench@); given a @FIXTURE.db@ argument it runs
-- against a copy of that production 'leios.db' sqlite database file instead.
--
-- Each cycle advances a GC frontier by @--slot-step@ slots (default 30);
-- then calls 'leiosDbMarkAsImmutable' and 'leiosDbGarbageCollect'. In
-- synthetic mode @--orphan-fraction@ of the EBs is never marked immutable,
-- so garbage collection evicts them.
--
-- Reported per cycle, as one CSV row on stdout (everything else goes to
-- stderr).
--
-- Usage:
--
-- @
-- cabal bench leios-gc-bench
-- cabal bench leios-gc-bench --benchmark-options='leios.db'
-- @
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, forever, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (Tracer (..), emit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List as List (foldl', intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import qualified Database.SQLite3 as SQL
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , TraceLeiosDb (..)
  , newLeiosDBSQLite
  , sqlSampleLeiosDBStats
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  )
import Options.Applicative hiding (action)
import System.Directory (copyFile)
import System.Exit (die)
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser optsInfo
  withSystemTempDirectory "leios-gc-bench" $ \tmpDir -> do
    let benchDb = tmpDir <> "/bench.db"
    (tracer, drain) <- mkCollectingTracer
    -- get the series of (slot, ebHash)
    (db, schedule) <- case optDbPath opts of
      Just path -> do
        hPutStrLn stderr $ "Copying the database " <> path <> " -> " <> benchDb
        copyFile path benchDb
        db <- newLeiosDBSQLite tracer benchDb
        close =<< open db
        schedule <- readEbSchedule benchDb
        pure (db, schedule)
      Nothing -> do
        validateSyntheticOpts opts
        hPutStrLn stderr $
          "Populating a synthetic database at "
            <> benchDb
            <> " ("
            <> show (syntheticEbCount opts)
            <> " EBs × "
            <> show (optTxsPerEb opts)
            <> " txs × "
            <> show (optTxBytes opts)
            <> " B, orphan fraction "
            <> show (optOrphanFraction opts)
            <> ")"
        db <- newLeiosDBSQLite tracer benchDb
        schedule <- populateDb opts db
        pure (db, schedule)
    (startSlot, endSlot) <- case schedule of
      [] -> die "empty mark-as-immutable schedule (no volatile ebs, or --orphan-fraction 1)"
      (s0, _) : rest -> pure (s0, maximum (map fst rest))
    let lastFrontier = startSlot + fromIntegral (optCycles opts) * optSlotStep opts
    hPutStr stderr $
      unlines
        [ ""
        , "Volatile ebs window : slots "
            <> show startSlot
            <> ".."
            <> show endSlot
            <> " ("
            <> show (length schedule)
            <> " distinct EB hashes)"
        , "Cycles              : "
            <> show (optCycles opts)
            <> " × slot step "
            <> show (optSlotStep opts)
            <> " (GC frontier "
            <> show (startSlot + optSlotStep opts)
            <> ".."
            <> show lastFrontier
            <> ")"
        , ""
        ]
    when (lastFrontier > endSlot) $
      hPutStrLn stderr $
        "NOTE: frontier passes the window top at cycle "
          <> show ((endSlot - startSlot) `div` optSlotStep opts)
          <> "; later cycles measure no-op GCs"
    before <- sqlSampleLeiosDBStats benchDb
    hPutStrLn stderr (renderStats "before" before)
    latRef <- newIORef 0
    putStrLn csvHeader
    cycleStats <-
      withAsync mutator $ \_ ->
        withAsync (tickProbe latRef) $ \_ ->
          runCycles opts db drain latRef schedule startSlot
    after <- sqlSampleLeiosDBStats benchDb
    hPutStrLn stderr (renderStats "after " after)
    hPutStr stderr (renderSummary cycleStats)

-- * Options

data Opts = Opts
  { optDbPath :: Maybe FilePath
  , optCycles :: Int
  , optSlotStep :: Word64
  , optEbsPerSlot :: Int
  , optTxsPerEb :: Int
  , optTxBytes :: Int
  , optOrphanFraction :: Double
  }

optsInfo :: ParserInfo Opts
optsInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Benchmark LeiosDemoDb mark-as-immutable and garbage collection \
          \against a synthetic database (default) or a production leios.db \
          \fixture; per-cycle results are written to stdout as CSV"
    )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> optional
      ( strArgument
          ( metavar "FIXTURE.db"
              <> help
                "Production leios.db sqlite file (benchmarked on a temp \
                \copy); when omitted, a synthetic database is generated"
          )
      )
    <*> option
      auto
      ( long "cycles"
          <> metavar "N"
          <> value 50
          <> showDefault
          <> help "Number of GC cycles to run"
      )
    <*> option
      auto
      ( long "slot-step"
          <> metavar "N"
          <> value 30
          <> showDefault
          <> help "Slots the frontier advances per cycle"
      )
    <*> option
      auto
      ( long "ebs-per-slot"
          <> metavar "N"
          <> value 1
          <> showDefault
          <> help "Synthetic mode: EBs announced per slot"
      )
    <*> option
      auto
      ( long "txs-per-eb"
          <> metavar "N"
          <> value 200
          <> showDefault
          <> help "Synthetic mode: transactions per EB"
      )
    <*> option
      auto
      ( long "tx-bytes"
          <> metavar "N"
          <> value 1536
          <> showDefault
          <> help "Synthetic mode: bytes per transaction payload (min 32)"
      )
    <*> option
      auto
      ( long "orphan-fraction"
          <> metavar "F"
          <> value 0.5
          <> showDefault
          <> help
            "Synthetic mode: fraction of EBs never marked immutable \
            \(evicted by GC instead)"
      )

-- * Synthetic population

validateSyntheticOpts :: Opts -> IO ()
validateSyntheticOpts opts = do
  when (optCycles opts < 1 || optSlotStep opts < 1 || optEbsPerSlot opts < 1 || optTxsPerEb opts < 1) $
    die "--cycles, --slot-step, --ebs-per-slot and --txs-per-eb must be at least 1"
  when (optTxBytes opts < 32) $
    die "--tx-bytes must be at least 32 (a payload embeds the 32-byte tx hash)"
  when (optOrphanFraction opts < 0 || optOrphanFraction opts > 1) $
    die "--orphan-fraction must be in [0, 1]"

-- | EBs generated in synthetic mode: 'optEbsPerSlot' per slot over the slots
-- the GC frontier will sweep.
syntheticEbCount :: Opts -> Int
syntheticEbCount opts =
  optCycles opts * fromIntegral (optSlotStep opts) * optEbsPerSlot opts

-- | Insert deterministic volatile EBs (untimed) and return the
-- mark-as-immutable schedule, ascending in slot; orphaned EBs are inserted
-- but excluded from it, so GC evicts them.
populateDb :: Opts -> LeiosDbHandle IO -> IO [(Word64, BS.ByteString)]
populateDb opts db = do
  conn <- open db
  schedule <- forM [0 .. syntheticEbCount opts - 1] $ \ebIdx -> do
    let slot = fromIntegral (ebIdx `div` optEbsPerSlot opts) :: Word64
        MkEbHash hashBytes = genEbHash ebIdx
        point = MkLeiosPoint (SlotNo slot) (MkEbHash hashBytes)
        eb = genEb opts ebIdx
        txs =
          [ (h, genTx opts h)
          | txIdx <- [0 .. optTxsPerEb opts - 1]
          , let h = genTxHash ebIdx txIdx
          ]
    leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
    _ <- leiosDbInsertEbBody conn point eb
    _ <- leiosDbInsertTxs conn txs
    pure $ if orphaned ebIdx then Nothing else Just (slot, hashBytes)
  close conn
  pure (catMaybes schedule)
 where
  -- Bresenham-style even spread of the orphan fraction over the EB indices.
  orphaned i = orphansBefore (i + 1) > orphansBefore i
  orphansBefore i = floor (fromIntegral i * optOrphanFraction opts :: Double) :: Int

-- * Deterministic data generation (as in leios-db-bench)

-- | 'EbHash' from an index: \"ebHash:<index>\" padded to 32 bytes with zeros.
genEbHash :: Int -> EbHash
genEbHash i = MkEbHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("ebHash:" <> show i)

-- | 'LeiosEb' with 'optTxsPerEb' transactions of 'optTxBytes' each.
genEb :: Opts -> Int -> LeiosEb
genEb opts ebIdx =
  MkLeiosEb $
    V.fromList
      [ (genTxHash ebIdx txIdx, fromIntegral (optTxBytes opts) :: BytesSize)
      | txIdx <- [0 .. optTxsPerEb opts - 1]
      ]

-- | 'TxHash' from an EB index + TX offset: \"txHash:<ebIdx>:<txIdx>\" padded
-- to 32 bytes with zeros.
--
-- NOTE: This is taking an EB index as it always generates the worst case of
-- fully disjunct transaction closures between EBs.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx txIdx = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("txHash:" <> show ebIdx <> ":" <> show txIdx)

-- | Generate a TX payload: the TX hash bytes padded with zeros to 'optTxBytes'.
genTx :: Opts -> TxHash -> BS.ByteString
genTx opts (MkTxHash h) = h <> BS.replicate (optTxBytes opts - BS.length h) 0

-- * Cycles

-- | One benchmark cycle: what was measured while the frontier advanced once.
data CycleResult = CycleResult
  { crFrontier :: !Word64
  , crMarkAsImmutableWall :: !DiffTime
  , crMarkedAsImmutableHashes :: !Int
  , crGcWall :: !DiffTime
  , crStats :: !CycleStats
  , crTickLat :: !DiffTime
  }

runCycles ::
  Opts ->
  LeiosDbHandle IO ->
  IO [TraceLeiosDb] ->
  IORef DiffTime ->
  [(Word64, BS.ByteString)] ->
  Word64 ->
  IO [CycleResult]
runCycles opts db drain latRef schedule startSlot = do
  remainingRef <- newIORef schedule
  _ <- drain -- discard events from handle setup
  _ <- atomicModifyIORef' latRef (\m -> (0, m))
  mapM (oneCycle remainingRef) [1 .. optCycles opts]
 where
  oneCycle remainingRef i = do
    let frontier = startSlot + fromIntegral i * optSlotStep opts
    remaining <- readIORef remainingRef
    let (due, rest) = span (\(s, _) -> s < frontier) remaining
    writeIORef remainingRef rest
    (_, markAsImmutableWall) <- timed $
      forM_ due $ \(s, h) ->
        leiosDbMarkAsImmutable db (MkLeiosPoint (SlotNo s) (MkEbHash h))
    let marked = length due
    (_, gcWall) <- timed $ leiosDbGarbageCollect db (SlotNo frontier)
    tickLat <- atomicModifyIORef' latRef (\m -> (0, m))
    stats <- List.foldl' addEvent emptyCycleStats <$> drain
    let result =
          CycleResult
            { crFrontier = frontier
            , crMarkAsImmutableWall = markAsImmutableWall
            , crMarkedAsImmutableHashes = marked
            , crGcWall = gcWall
            , crStats = stats
            , crTickLat = tickLat
            }
    putStrLn (renderCycle i result)
    pure result

-- * Event collection

mkCollectingTracer :: IO (Tracer IO TraceLeiosDb, IO [TraceLeiosDb])
mkCollectingTracer = do
  ref <- newIORef []
  let tracer = Tracer $ emit $ \ev -> atomicModifyIORef' ref (\evs -> (ev : evs, ()))
      drain = atomicModifyIORef' ref (\evs -> ([], reverse evs))
  pure (tracer, drain)

data CycleStats = CycleStats
  { csMarkedAsImmutable :: !(Int, Int, Int)
  , csEvicted :: !(Int, Int, Int)
  , csCollisions :: !Int
  }

emptyCycleStats :: CycleStats
emptyCycleStats = CycleStats (0, 0, 0) (0, 0, 0) 0

addEvent :: CycleStats -> TraceLeiosDb -> CycleStats
addEvent cs = \case
  TraceLeiosDbCopiedToImmutable{copiedEbs, copiedEbTxs, copiedTxs} ->
    cs{csMarkedAsImmutable = csMarkedAsImmutable cs `add3` (copiedEbs, copiedEbTxs, copiedTxs)}
  TraceLeiosDbEvicted{evictedEbs, evictedEbTxs, evictedTxs} ->
    cs{csEvicted = csEvicted cs `add3` (evictedEbs, evictedEbTxs, evictedTxs)}
  TraceLeiosDbInsertCollision{} -> cs{csCollisions = csCollisions cs + 1}
  _ -> cs
 where
  add3 (a, b, c) (x, y, z) = (a + x, b + y, c + z)

-- * RTS health probe

-- | Steady allocator, so minor heap GCs happen constantly (as on a real
-- node). Together with 'tickProbe' this reproduces the production failure
-- mode: an unsafe FFI call in the maintenance path blocks the RTS GC sync
-- and every thread — including the ticker — stalls for the statement's
-- duration.
--
-- Must really allocate on every iteration: a fused non-allocating loop
-- (e.g. @sum [1 .. n]@ at -O1) never reaches a GC safe point and wedges the
-- process at the first GC sync. A fresh 'BS.ByteString' per iteration
-- cannot be fused away, and the 'threadDelay' keeps the allocation rate
-- bounded rather than saturating a core.
mutator :: IO ()
mutator = forever $ do
  _ <- evaluate (BS.length (BS.replicate 65_536 0))
  threadDelay 100

-- | Record the worst excess over a 1 ms sleep, i.e. how long the RTS
-- refused to schedule an always-runnable thread.
tickProbe :: IORef DiffTime -> IO ()
tickProbe latRef = forever $ do
  t0 <- getMonotonicTime
  threadDelay 1_000
  t1 <- getMonotonicTime
  let !excess = diffTime t1 t0 - 0.001
  atomicModifyIORef' latRef (\m -> (max m excess, ()))

-- * Fixture inspection

-- | Distinct volatile EB hashes with their newest announcement slot, ascending.
readEbSchedule :: FilePath -> IO [(Word64, BS.ByteString)]
readEbSchedule path = do
  db <- SQL.open (T.pack path)
  stmt <-
    SQL.prepare
      db
      "SELECT MAX(ebSlot) AS s, ebHashBytes FROM ebs WHERE immutable = 0 GROUP BY ebHashBytes ORDER BY s"
  let loop acc =
        SQL.step stmt >>= \case
          SQL.Row -> do
            slot <- SQL.columnInt64 stmt 0
            h <- SQL.columnBlob stmt 1
            loop ((fromIntegral slot, h) : acc)
          SQL.Done -> pure (reverse acc)
  ebs <- loop []
  SQL.finalize stmt
  SQL.close db
  pure ebs

-- * Rendering

csvHeader :: String
csvHeader =
  List.intercalate
    ","
    [ "cycle"
    , "gcSlot"
    , "markedEbs"
    , "markedEbRows"
    , "markedEbTxRows"
    , "markedTxRows"
    , "markAsImmutableSeconds"
    , "evictedEbRows"
    , "evictedEbTxRows"
    , "evictedTxRows"
    , "gcSeconds"
    , "insertCollisions"
    , "tickLatSeconds"
    ]

renderCycle :: Int -> CycleResult -> String
renderCycle
  i
  CycleResult
    { crFrontier
    , crMarkAsImmutableWall
    , crMarkedAsImmutableHashes
    , crGcWall
    , crStats = cs
    , crTickLat
    } =
    List.intercalate
      ","
      [ show i
      , show crFrontier
      , show crMarkedAsImmutableHashes
      , show mEbs
      , show mEbTxs
      , show mTxs
      , showSeconds crMarkAsImmutableWall
      , show eEbs
      , show eEbTxs
      , show eTxs
      , showSeconds crGcWall
      , show (csCollisions cs)
      , showSeconds crTickLat
      ]
   where
    (mEbs, mEbTxs, mTxs) = csMarkedAsImmutable cs
    (eEbs, eEbTxs, eTxs) = csEvicted cs

renderStats :: String -> (TraceLeiosDb, TraceLeiosDb) -> String
renderStats label = \case
  ( TraceLeiosDbVolatileStats{volatileEbs, volatileEbTxs, volatileTxs}
    , TraceLeiosDbImmutableStats{immutableEbs, immutableEbTxs, immutableTxs}
    ) ->
      unwords
        [ label <> ": volatile"
        , "ebs=" <> show volatileEbs
        , "ebTxs=" <> show volatileEbTxs
        , "txs=" <> show volatileTxs
        , "| immutable"
        , "ebs=" <> show immutableEbs
        , "ebTxs=" <> show immutableEbTxs
        , "txs=" <> show immutableTxs
        ]
  other -> label <> ": unexpected stats events " <> show other

renderSummary :: [CycleResult] -> String
renderSummary results =
  unlines
    [ ""
    , "Totals over " <> show (length results) <> " cycles:"
    , "  marked as immutable ebs/ebTxs/txs = "
        <> showT (sum3 (map (csMarkedAsImmutable . crStats) results))
    , "  evicted ebs/ebTxs/txs = " <> showT (sum3 (map (csEvicted . crStats) results))
    , "  insert collisions     = " <> show (sum (map (csCollisions . crStats) results))
    , stat "mark as immutable " (map crMarkAsImmutableWall results)
    , stat "gc                " (map crGcWall results)
    , stat "tick latency      " (map crTickLat results)
    ]
 where
  sum3 = List.foldl' (\(a, b, c) (x, y, z) -> (a + x, b + y, c + z)) (0, 0, 0)
  showT (a, b, c) = show a <> "/" <> show b <> "/" <> show c
  stat label ts =
    "  "
      <> label
      <> ": min="
      <> showTime (minimum ts)
      <> "  avg="
      <> showTime (sum ts / fromIntegral (length ts))
      <> "  max="
      <> showTime (maximum ts)

-- * Timing helpers (as in leios-db-bench)

timed :: IO a -> IO (a, DiffTime)
timed action = do
  t0 <- getMonotonicTime
  !result <- action
  t1 <- getMonotonicTime
  pure (result, diffTime t1 t0)

showSeconds :: DiffTime -> String
showSeconds t = printf "%.6f" (realToFrac t :: Double)

showTime :: DiffTime -> String
showTime t
  | t < 1e-6 = show (round (s * 1_000_000_000 :: Double) :: Int) <> " ns"
  | t < 1e-3 = show (round (s * 1_000_000 :: Double) :: Int) <> " μs"
  | t < 1 = show (round (s * 1_000 :: Double) :: Int) <> " ms"
  | otherwise = show s <> " s"
 where
  s = realToFrac t :: Double
