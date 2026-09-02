{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark for the sqlite backend of 'LeiosDemoDb' promote-to-immutable
-- and garbage collection.
--
-- By default the benchmark populates a synthetic database with deterministic
-- data (as in @leios-db-bench@); given a @FIXTURE.vol.db@ argument it runs
-- against a copy of that production volatile partition instead (a sibling
-- @FIXTURE.imm.db@, if present, is copied along). Fixtures must already be in
-- the two-file split schema; see @convert-to-split-schema.py@.
--
-- Each cycle advances a GC frontier by @--slot-step@ slots (default 30), calls
-- 'leiosDbPromoteToImmutable' for the due EBs, waits for the background
-- copier to land them in the immutable partition (reported as
-- @copyWaitSeconds@), then calls 'leiosDbGarbageCollect' — which only MARKS
-- (@markSeconds@) — and waits for the background sweeper to sweep the marks
-- (@sweepSeconds@). In synthetic mode @--orphan-fraction@ of the EBs is
-- never promoted, so garbage collection evicts them.
--
-- Reported per cycle, as one CSV row on stdout (everything else goes to
-- stderr).
--
-- Usage:
--
-- @
-- cabal bench leios-gc-bench
-- cabal bench leios-gc-bench --benchmark-options='leios.vol.db'
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
import Data.List as List (foldl', intercalate, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import qualified Database.SQLite3 as SQL
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosDbStats (..)
  , TraceLeiosDb (..)
  , newLeiosDBSQLite
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
import System.Directory (copyFile, doesFileExist)
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
    -- Same naming convention as 'newLeiosDBSQLiteFromEnv'.
    let benchVol = tmpDir <> "/bench.db.vol"
        benchImm = tmpDir <> "/bench.db.imm"
    (tracer, flushEvents) <- mkCollectingTracer
    -- get the series of (slot, ebHash)
    (db, schedule) <- case optDbPath opts of
      Just path -> do
        hPutStrLn stderr $ "Copying the database " <> path <> " -> " <> benchVol
        copyFile path benchVol
        let immFixture = immSiblingOf path
        hasImm <- doesFileExist immFixture
        when hasImm $ do
          hPutStrLn stderr $ "Copying the database " <> immFixture <> " -> " <> benchImm
          copyFile immFixture benchImm
        db <- newLeiosDBSQLite tracer benchVol benchImm
        close =<< open db
        schedule <- readEbSchedule benchVol
        pure (db, schedule)
      Nothing -> do
        validateSyntheticOpts opts
        hPutStrLn stderr $
          "Populating a synthetic database at "
            <> benchVol
            <> " ("
            <> show (syntheticEbCount opts)
            <> " EBs × "
            <> show (optTxsPerEb opts)
            <> " txs × "
            <> show (optTxBytes opts)
            <> " B, orphan fraction "
            <> show (optOrphanFraction opts)
            <> ")"
        db <- newLeiosDBSQLite tracer benchVol benchImm
        schedule <- populateDb opts db
        pure (db, schedule)
    (startSlot, endSlot) <- case schedule of
      [] -> die "empty promote-to-immutable schedule (no volatile ebs, or --orphan-fraction 1)"
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
    -- Await the sweeper's startup self-heal (GC-candidates initialisation +
    -- resume of persisted marks), so it is not attributed to cycle 1.
    sweepBacklog <- mkBacklogProbe benchVol sqlSweepBacklog
    initialBacklog <- mkBacklogProbe benchVol sqlInitialBacklog
    (_, initialSweepWall) <- timed $ awaitZero "initial self-heal" initialBacklog
    hPutStrLn stderr ("initial sweep: " <> showTime initialSweepWall)
    before <- leiosDbSampleStats db
    hPutStrLn stderr (renderStats "before" before)
    latRef <- newIORef 0
    putStrLn csvHeader
    cycleStats <-
      withAsync mutator $ \_ ->
        withAsync (tickProbe latRef) $ \_ ->
          runCycles opts db flushEvents latRef sweepBacklog schedule startSlot (immutableEbs before)
    after <- leiosDbSampleStats db
    hPutStrLn stderr (renderStats "after " after)
    hPutStr stderr (renderSummary cycleStats)

-- | The imm-file sibling of a volatile fixture path, for either naming
-- convention (@FOO.vol@ as the node derives it, or the fixture converter's
-- @FOO.vol.db@).
immSiblingOf :: FilePath -> FilePath
immSiblingOf path
  | ".vol.db" `List.isSuffixOf` path =
      take (length path - length (".vol.db" :: String)) path <> ".imm.db"
  | ".vol" `List.isSuffixOf` path =
      take (length path - length (".vol" :: String)) path <> ".imm"
  | otherwise = path <> ".imm"

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
          "Benchmark LeiosDemoDb promote-to-immutable and garbage collection \
          \against a synthetic database (default) or a production leios.vol.db \
          \fixture; per-cycle results are written to stdout as CSV"
    )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> optional
      ( strArgument
          ( metavar "FIXTURE.vol.db"
              <> help
                "Production leios.vol.db sqlite file (benchmarked on a temp \
                \copy; a sibling .imm.db is copied along when present); when \
                \omitted, a synthetic database is generated"
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
            "Synthetic mode: fraction of EBs never promoted to immutable \
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
-- promote-to-immutable schedule, ascending in slot; orphaned EBs are inserted
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
  , crPromoteWall :: !DiffTime
  , crPromotedHashes :: !Int
  , crCopyWaitWall :: !DiffTime
  , crMarkWall :: !DiffTime
  , crSweepWall :: !DiffTime
  , crStats :: !CycleStats
  , crTickLat :: !DiffTime
  }

runCycles ::
  Opts ->
  LeiosDbHandle IO ->
  IO [TraceLeiosDb] ->
  IORef DiffTime ->
  IO Int ->
  [(Word64, BS.ByteString)] ->
  Word64 ->
  Int ->
  IO [CycleResult]
runCycles opts db flushEvents latRef sweepBacklog schedule startSlot immBefore = do
  remainingRef <- newIORef schedule
  promotedRef <- newIORef (0 :: Int)
  _ <- flushEvents -- discard events from handle setup
  _ <- atomicModifyIORef' latRef (\m -> (0, m))
  mapM (oneCycle remainingRef promotedRef) [1 .. optCycles opts]
 where
  oneCycle remainingRef promotedRef i = do
    let frontier = startSlot + fromIntegral i * optSlotStep opts
    remaining <- readIORef remainingRef
    let (due, rest) = span (\(s, _) -> s < frontier) remaining
    writeIORef remainingRef rest
    (_, promoteWall) <- timed $
      forM_ due $ \(s, h) ->
        leiosDbPromoteToImmutable db (MkLeiosPoint (SlotNo s) (MkEbHash h))
    let promoted = length due
    -- Wait until the background copier has landed the cycle's promotions
    -- in the immutable partition; every scheduled hash is distinct, so the
    -- immutable EB count is an exact progress meter.
    promotedTotal <- atomicModifyIORef' promotedRef (\n -> (n + promoted, n + promoted))
    (_, copyWaitWall) <- timed $ awaitCopier (immBefore + promotedTotal)
    -- The GC tick only marks; the sweeper evicts the marked rows in the
    -- background.
    (_, markWall) <- timed $ leiosDbGarbageCollect db (SlotNo frontier)
    (_, sweepWall) <- timed $ awaitZero "sweep" sweepBacklog
    tickLat <- atomicModifyIORef' latRef (\m -> (0, m))
    stats <- List.foldl' addEvent emptyCycleStats <$> flushEvents
    let result =
          CycleResult
            { crFrontier = frontier
            , crPromoteWall = promoteWall
            , crPromotedHashes = promoted
            , crCopyWaitWall = copyWaitWall
            , crMarkWall = markWall
            , crSweepWall = sweepWall
            , crStats = stats
            , crTickLat = tickLat
            }
    putStrLn (renderCycle i result)
    pure result

  awaitCopier target = go (0 :: Int)
   where
    go n = do
      stats <- leiosDbSampleStats db
      if immutableEbs stats >= target
        then pure ()
        else
          if n > 120_000
            then
              die $
                "copy wait timed out: immutableEbs = "
                  <> show (immutableEbs stats)
                  <> ", expected "
                  <> show target
            else do
              threadDelay 1_000
              go (n + 1)

-- * Sweep backlog probes

-- | What the sweeper still owes: GC-marked rows plus unresolved orphan hints.
sqlSweepBacklog :: T.Text
sqlSweepBacklog =
  "SELECT (SELECT COUNT(*) FROM ebs WHERE status = 3)\n\
  \     + (SELECT COUNT(*) FROM gcTxCandidates)"

-- | 'sqlSweepBacklog' plus whether any legacy orphan tx exists at all: only 0
-- once the sweeper's GC-candidates initialisation has both run and been swept
-- (counting staged candidates alone would race the initialisation scan).
sqlInitialBacklog :: T.Text
sqlInitialBacklog =
  "SELECT (SELECT COUNT(*) FROM ebs WHERE status = 3)\n\
  \     + (SELECT COUNT(*) FROM gcTxCandidates)\n\
  \     + (SELECT EXISTS (SELECT 1 FROM txs WHERE NOT EXISTS\n\
  \          (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)))"

-- | A reusable single-integer probe on its own connection (WAL readers do
-- not block the sweeper's writes; the reset after each poll releases the
-- read snapshot).
mkBacklogProbe :: FilePath -> T.Text -> IO (IO Int)
mkBacklogProbe path sql = do
  db <- SQL.open (T.pack path)
  stmt <- SQL.prepare db sql
  pure $ do
    _ <- SQL.step stmt
    n <- SQL.columnInt64 stmt 0
    SQL.reset stmt
    pure (fromIntegral n)

-- | Poll a backlog probe until it reaches zero.
awaitZero :: String -> IO Int -> IO ()
awaitZero what probe = go (0 :: Int)
 where
  go n =
    probe >>= \case
      0 -> pure ()
      k ->
        if n > 600_000
          then die (what <> " timed out; backlog = " <> show k)
          else do
            threadDelay 1_000
            go (n + 1)

-- * Event collection

mkCollectingTracer :: IO (Tracer IO TraceLeiosDb, IO [TraceLeiosDb])
mkCollectingTracer = do
  ref <- newIORef []
  let tracer = Tracer $ emit $ \ev -> atomicModifyIORef' ref (\evs -> (ev : evs, ()))
      flushEvents = atomicModifyIORef' ref (\evs -> ([], reverse evs))
  pure (tracer, flushEvents)

data CycleStats = CycleStats
  { csCopied :: !Int
  , csEvicted :: !Int
  , csCollisions :: !Int
  , csCopyErrors :: !Int
  , csSweepErrors :: !Int
  }

emptyCycleStats :: CycleStats
emptyCycleStats = CycleStats 0 0 0 0 0

addEvent :: CycleStats -> TraceLeiosDb -> CycleStats
addEvent cs = \case
  TraceLeiosDbCopiedToImmutable copiedEbs ->
    cs{csCopied = csCopied cs + copiedEbs}
  TraceLeiosDbEvicted evictedEbs ->
    cs{csEvicted = csEvicted cs + evictedEbs}
  TraceLeiosDbInsertCollision{} -> cs{csCollisions = csCollisions cs + 1}
  TraceLeiosDbCopyError{} -> cs{csCopyErrors = csCopyErrors cs + 1}
  TraceLeiosDbGCError{} -> cs{csSweepErrors = csSweepErrors cs + 1}
  _ -> cs

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
      "SELECT MAX(ebSlot) AS s, ebHashBytes FROM ebs WHERE status = 0 GROUP BY ebHashBytes ORDER BY s"
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
    , "promotedEbs"
    , "copiedEbRows"
    , "promoteSeconds"
    , "copyWaitSeconds"
    , "evictedEbRows"
    , "markSeconds"
    , "sweepSeconds"
    , "insertCollisions"
    , "copyErrors"
    , "sweepErrors"
    , "tickLatSeconds"
    ]

renderCycle :: Int -> CycleResult -> String
renderCycle
  i
  CycleResult
    { crFrontier
    , crPromoteWall
    , crPromotedHashes
    , crCopyWaitWall
    , crMarkWall
    , crSweepWall
    , crStats = cs
    , crTickLat
    } =
    List.intercalate
      ","
      [ show i
      , show crFrontier
      , show crPromotedHashes
      , show (csCopied cs)
      , showSeconds crPromoteWall
      , showSeconds crCopyWaitWall
      , show (csEvicted cs)
      , showSeconds crMarkWall
      , showSeconds crSweepWall
      , show (csCollisions cs)
      , show (csCopyErrors cs)
      , show (csSweepErrors cs)
      , showSeconds crTickLat
      ]

renderStats :: String -> LeiosDbStats -> String
renderStats label LeiosDbStats{volatileEbs, immutableEbs} =
  unwords
    [ label <> ":"
    , "volatile ebs=" <> show volatileEbs
    , "| immutable ebs=" <> show immutableEbs
    ]

renderSummary :: [CycleResult] -> String
renderSummary results =
  unlines
    [ ""
    , "Totals over " <> show (length results) <> " cycles:"
    , "  copied ebs            = " <> show (sum (map (csCopied . crStats) results))
    , "  evicted ebs           = " <> show (sum (map (csEvicted . crStats) results))
    , "  insert collisions     = " <> show (sum (map (csCollisions . crStats) results))
    , "  copy errors           = " <> show (sum (map (csCopyErrors . crStats) results))
    , "  sweep errors          = " <> show (sum (map (csSweepErrors . crStats) results))
    , stat "promote           " (map crPromoteWall results)
    , stat "copy wait         " (map crCopyWaitWall results)
    , stat "mark              " (map crMarkWall results)
    , stat "sweep             " (map crSweepWall results)
    , stat "tick latency      " (map crTickLat results)
    ]
 where
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
