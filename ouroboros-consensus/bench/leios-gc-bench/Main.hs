{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark for the sqlite backend of 'LeiosDemoDb': full-EB insertion,
-- promote-to-immutable and garbage collection, as a load matrix.
--
-- The volatile database is always populated with 2160 EBs (the k=2160 steady
-- state), one every 20 slots (the mainnet Praos block cadence, and each RB
-- announces at most one EB). The load level (@--load@) sets how much of each
-- EB's closure is shared with other EBs:
--
--   * @eb-with-sharing@ --- 25% of each EB's txs come from a pool shared
--     between EBs
--   * @eb-no-sharing@   --- no shared txs: every tx is unique to one EB, so
--     copies get no @OR IGNORE@ savings and every swept EB orphans its full
--     closure
--
-- (a production @FIXTURE.vol.db@ argument replaces the synthetic population
-- entirely).
--
-- Then the scenario selected by @--scenario@ runs, repeated for statistical
-- confidence:
--
--   * @steady@  --- 10 phases of 1 GC'able EB each: the per-tick norm of a
--     healthy node (one EB per RB, GC ticks every 10-25s, so 0-3 per tick
--     with 1 typical);
--   * @catchup@ --- 5 phases of 180 GC'able EBs each (the backlog of a ~1h
--     outage): the first GC tick after downtime or during sync, where
--     everything that aged out meanwhile is marked at once.
--
-- A phase takes the next N EBs of the schedule, so the GC frontier passed to
-- 'leiosDbGarbageCollect' makes exactly N EBs collectable. Per phase, in
-- order:
--
--   1. insert 5 fresh full EBs (announcement + body + txs) at the current
--      load, reporting the median wall time (@insertEbSeconds@);
--   2. promote half of the N EBs and wait for the background copier to land
--      them (@promoteSeconds@, @copyWaitSeconds@) --- the rest are evicted
--      un-copied, as un-certified EBs are on a real node;
--   3. call 'leiosDbGarbageCollect' --- which only MARKS (@markSeconds@) ---
--      and wait for the background sweeper to drain the marks
--      (@sweepSeconds@). The sweep is decomposed via its call-trace spans:
--      the one-off GC-candidates initialisation (first pass of the process
--      only), the EB-eviction loop, the orphan-tx loop (both loops include
--      the pacing sleeps between their batches) and the WAL checkpoint.
--
-- @--gc-pacing@ selects the sweeper pacing: @default@ is what the node runs
-- (4-EB eviction batches with 100ms pauses in between, trading drain time
-- for short write-lock holds); @zero@ removes both bounds (one unbounded
-- eviction transaction, no pauses), measuring the raw sweep work.
--
-- @--no-tx-index@ drops @idx_ebTxs_txHashBytes@ (the tx -> referencing-EB
-- index) from the volatile partition after population, to price the index:
-- insertion pays one scattered index write per body row with it, while the
-- sweeper's orphan probes degrade to full @ebTxs@ scans without it. NOTE:
-- sweeping without the index is extremely slow — pair it with
-- @--scenario steady@ (a catch-up sweep would hit the timeout) and read the
-- @insertEbSeconds@ column, sampled before each phase's sweep.
--
-- Reported per phase, as one CSV row on stdout (everything else goes to
-- stderr).
--
-- Usage (@cabal run@ passes arguments directly and keeps the CSV on stdout;
-- @cabal bench@ works too, via @--benchmark-options='...'@):
--
-- @
-- -- steady state at the default load (2160 EBs, 2700 txs\/EB, 25% shared)
-- cabal run bench:leios-gc-bench -- --scenario steady > steady.csv
--
-- -- catch-up with worst-case tx sharing: no tx is shared between EBs
-- cabal run bench:leios-gc-bench -- --scenario catchup --load eb-no-sharing
--
-- -- the 512 KB-body worst case (~40 GB volatile partition; TMPDIR must
-- -- point at a disk with ~100 GB free, not a tmpfs; runs for hours)
-- TMPDIR=\/scratch cabal run bench:leios-gc-bench -- \\
--   --scenario catchup --load eb-no-sharing --txs-per-eb 13000 --tx-bytes 923
--
-- -- raw sweep work and unprotected lock holds: no batching, no pauses
-- cabal run bench:leios-gc-bench -- --scenario catchup --gc-pacing zero
--
-- -- price idx_ebTxs_txHashBytes on the insert path
-- cabal run bench:leios-gc-bench -- --scenario steady --no-tx-index
--
-- -- a production volatile partition instead of the synthetic population
-- cabal run bench:leios-gc-bench -- --scenario steady leios.vol.db
-- @
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, forever, replicateM, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (Tracer (..), emit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List as List (foldl', group, intercalate, isSuffixOf, nub, sort)
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
  , newLeiosDBSQLiteWithGcPacing
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  )
import LeiosUtils.CallTrace
  ( CallEvent (..)
  , CallInfo (..)
  , CallMeasure (..)
  , CallTrace (..)
  , SomeJsonCallTrace (..)
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
  validateOpts opts
  withSystemTempDirectory "leios-gc-bench" $ \tmpDir -> do
    -- Same naming convention as 'newLeiosDBSQLiteFromEnv'.
    let benchVol = tmpDir <> "/bench.db.vol"
        benchImm = tmpDir <> "/bench.db.imm"
    (tracer, flushEvents) <- mkCollectingTracer
    let mkDb = case optGcPacing opts of
          GcPacingDefault -> newLeiosDBSQLite tracer benchVol benchImm
          GcPacingZero -> newLeiosDBSQLiteWithGcPacing tracer benchVol benchImm 0 0
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
        db <- mkDb
        close =<< open db
        schedule <- readEbSchedule benchVol
        pure (db, schedule)
      Nothing -> do
        hPutStrLn stderr $
          "Populating a synthetic database at "
            <> benchVol
            <> " ("
            <> show populationEbs
            <> " EBs × "
            <> show (optTxsPerEb opts)
            <> " txs × "
            <> show (optTxBytes opts)
            <> " B, dup fraction "
            <> show (effDup opts)
            <> ")"
        db <- mkDb
        schedule <- populateDb opts db
        pure (db, schedule)
    when (optNoTxIndex opts) $ do
      hPutStrLn stderr "Dropping idx_ebTxs_txHashBytes from the volatile partition"
      dropTxIndex benchVol
    when (null schedule) $
      die "empty EB schedule (no volatile ebs)"
    when (sum (map snd (phaseSeries (optScenario opts))) > length schedule) $
      hPutStrLn stderr $
        "NOTE: the phases want "
          <> show (sum (map snd (phaseSeries (optScenario opts))))
          <> " EBs but only "
          <> show (length schedule)
          <> " are scheduled; later phases will be skipped"
    let describeSeries =
          List.intercalate " + "
            [ scenario <> "×" <> show (length grp) <> " (" <> show n <> " EBs each)"
            | grp@((scenario, n) : _) <- List.group (phaseSeries (optScenario opts))
            ]
    hPutStr stderr $
      unlines
        [ ""
        , "Scheduled EBs       : " <> show (length schedule)
        , "Phases              : " <> describeSeries
        , "GC pacing           : " <> gcPacingName (optGcPacing opts)
        , "idx_ebTxs_txHashBytes: " <> (if optNoTxIndex opts then "DROPPED" else "present")
        , ""
        ]
    -- Await the sweeper's startup self-heal (GC-candidates initialisation +
    -- resume of persisted marks), so it is not attributed to phase 1.
    sweepBacklog <- mkBacklogProbe benchVol sqlSweepBacklog
    initialBacklog <- mkBacklogProbe benchVol sqlInitialBacklog
    (_, initialSweepWall) <- timed $ awaitZero "initial self-heal" initialBacklog
    hPutStrLn stderr ("initial sweep: " <> showTime initialSweepWall)
    before <- leiosDbSampleStats db
    hPutStrLn stderr (renderStats "before" before)
    latRef <- newIORef 0
    putStrLn csvHeader
    phaseStats <-
      withAsync mutator $ \_ ->
        withAsync (tickProbe latRef) $ \_ ->
          runPhases opts db flushEvents latRef sweepBacklog schedule (immutableEbs before)
    after <- leiosDbSampleStats db
    hPutStrLn stderr (renderStats "after " after)
    hPutStr stderr (renderSummary opts phaseStats)

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

data Load = LoadWithSharing | LoadNoSharing

loadName :: Load -> String
loadName = \case
  LoadWithSharing -> "eb-with-sharing"
  LoadNoSharing -> "eb-no-sharing"

-- | Preset fraction of each EB's txs drawn from a pool shared between EBs.
-- The no-sharing case is the worst: every tx is unique to one EB.
loadDupPreset :: Load -> Double
loadDupPreset = \case
  LoadWithSharing -> 0.25
  LoadNoSharing -> 0

data Scenario = Steady | Catchup

data GcPacing = GcPacingDefault | GcPacingZero

gcPacingName :: GcPacing -> String
gcPacingName = \case
  GcPacingDefault -> "default"
  GcPacingZero -> "zero"

readGcPacing :: String -> Either String GcPacing
readGcPacing = \case
  "default" -> Right GcPacingDefault
  "zero" -> Right GcPacingZero
  s -> Left ("expected default|zero, got: " <> s)

readScenario :: String -> Either String Scenario
readScenario = \case
  "steady" -> Right Steady
  "catchup" -> Right Catchup
  s -> Left ("expected steady|catchup, got: " <> s)

data Opts = Opts
  { optDbPath :: Maybe FilePath
  , optLoad :: Load
  , optScenario :: Scenario
  , optGcPacing :: GcPacing
  , optTxsPerEb :: Int
  , optTxBytes :: Int
  , optNoTxIndex :: Bool
  }

-- | Synthetic-mode volatile population: k EBs, the steady-state window.
populationEbs :: Int
populationEbs = 2160

-- | Synthetic-mode slots between consecutive EB announcements: the mainnet
-- Praos block cadence, and each RB announces at most one EB.
slotsPerEb :: Int
slotsPerEb = 20

-- | Timed full-EB insertions per phase; the median is reported.
insertSamples :: Int
insertSamples = 5

-- | Fraction of each phase's GC'able EBs promoted to immutable before the
-- GC call; the rest are evicted un-copied.
promoteFraction :: Double
promoteFraction = 0.5

-- | Shared-tx fraction of the @--load@ level.
effDup :: Opts -> Double
effDup = loadDupPreset . optLoad

-- | The labeled measurement phases of a scenario: steady is 25 phases of 1
-- GC'able EB (the per-tick norm of a healthy node); catch-up is 10 phases of
-- 180 GC'able EBs (the backlog of a ~1h outage at one EB per 20s).
phaseSeries :: Scenario -> [(String, Int)]
phaseSeries = \case
  Steady -> replicate 25 ("steady", 1)
  Catchup -> replicate 10 ("catchup", 180)

optsInfo :: ParserInfo Opts
optsInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Benchmark LeiosDemoDb full-EB insertion, promote-to-immutable and \
          \garbage collection at a chosen volatile load level, varying how \
          \many EBs each GC call may collect; per-phase results are written \
          \to stdout as CSV"
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
      (eitherReader readLoad)
      ( long "load"
          <> metavar "eb-with-sharing|eb-no-sharing"
          <> value LoadWithSharing
          <> showDefaultWith loadName
          <> help
            "Synthetic load level: whether part of each EB's closure is \
            \shared with other EBs (eb-no-sharing is the worst case)"
      )
    <*> option
      (eitherReader readScenario)
      ( long "scenario"
          <> metavar "steady|catchup"
          <> help
            "Which GC scenario to run: steady (10 phases of 1 GC'able EB, \
            \the per-tick norm of a healthy node) or catchup (5 phases of \
            \180 EBs, the backlog of a ~1h outage)"
      )
    <*> option
      (eitherReader readGcPacing)
      ( long "gc-pacing"
          <> metavar "default|zero"
          <> value GcPacingDefault
          <> showDefaultWith gcPacingName
          <> help
            "Sweeper pacing: default (4-EB eviction batches, 100ms pauses \
            \in between) or zero (one unbounded eviction transaction, no \
            \pauses) --- zero measures raw sweep work and the unprotected \
            \worst-case write-lock hold"
      )
    <*> option
      auto
      ( long "txs-per-eb"
          <> metavar "N"
          <> value 2700
          <> showDefault
          <> help
            "Synthetic mode: transactions per EB (2700 = observed on the \
            \proto-devnet; 13000 = the 512 KB body worst case)"
      )
    <*> option
      auto
      ( long "tx-bytes"
          <> metavar "N"
          <> value 1536
          <> showDefault
          <> help "Synthetic mode: bytes per transaction payload (min 32)"
      )
    <*> switch
      ( long "no-tx-index"
          <> help
            "Drop idx_ebTxs_txHashBytes after population, to price the \
            \index on the insert and sweep paths"
      )

readLoad :: String -> Either String Load
readLoad = \case
  "eb-with-sharing" -> Right LoadWithSharing
  "eb-no-sharing" -> Right LoadNoSharing
  s -> Left ("expected eb-with-sharing|eb-no-sharing, got: " <> s)

validateOpts :: Opts -> IO ()
validateOpts opts = do
  when (optTxsPerEb opts < 1) $
    die "--txs-per-eb must be at least 1"
  when (optTxBytes opts < 32) $
    die "--tx-bytes must be at least 32 (a payload embeds the 32-byte tx hash)"

-- * Synthetic population

-- | Insert deterministic volatile EBs (untimed), one every 'slotsPerEb'
-- slots, and return the (slot, hash) schedule ascending in slot.
populateDb :: Opts -> LeiosDbHandle IO -> IO [(Word64, BS.ByteString)]
populateDb opts db = do
  conn <- open db
  schedule <- forM [0 .. populationEbs - 1] $ \ebIdx -> do
    let slot = fromIntegral (ebIdx * slotsPerEb) :: Word64
        MkEbHash hashBytes = genEbHash ebIdx
        point = MkLeiosPoint (SlotNo slot) (MkEbHash hashBytes)
        eb = genEb opts ebIdx
        txs = [(h, genTx opts h) | h <- ebTxHashesFor opts ebIdx]
    leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
    _ <- leiosDbInsertEbBody conn point eb
    _ <- leiosDbInsertTxs conn txs
    pure (slot, hashBytes)
  close conn
  pure schedule

-- | Drop the tx -> referencing-EB index from the volatile partition
-- ('sql_schema' only runs at file creation, so the drop persists).
dropTxIndex :: FilePath -> IO ()
dropTxIndex path = do
  db <- SQL.open (T.pack path)
  SQL.exec db "pragma busy_timeout = 30000;"
  SQL.exec db "DROP INDEX IF EXISTS idx_ebTxs_txHashBytes"
  SQL.close db

-- * Deterministic data generation (as in leios-db-bench)

-- | 'EbHash' from an index: \"ebHash:<index>\" padded to 32 bytes with zeros.
genEbHash :: Int -> EbHash
genEbHash i = MkEbHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("ebHash:" <> show i)

-- | The txs of a synthetic EB: the first @dup-fraction@ of them come from a
-- pool shared between EBs (a rotating slice, so consecutive EBs overlap),
-- the rest are unique to this EB. With dup fraction 0 every tx is unique ---
-- the worst case for copy (@OR IGNORE@ never saves work) and sweep (every
-- swept EB orphans its whole closure).
ebTxHashesFor :: Opts -> Int -> [TxHash]
ebTxHashesFor opts ebIdx =
  [genSharedTxHash ((ebIdx * nShared + k) `mod` poolSize) | k <- [0 .. nShared - 1]]
    ++ [genTxHash ebIdx txIdx | txIdx <- [nShared .. optTxsPerEb opts - 1]]
 where
  nShared = floor (effDup opts * fromIntegral (optTxsPerEb opts) :: Double)
  poolSize = max 1 (4 * optTxsPerEb opts)

-- | 'LeiosEb' over 'ebTxHashesFor' with 'optTxBytes' per transaction.
genEb :: Opts -> Int -> LeiosEb
genEb opts ebIdx =
  MkLeiosEb $
    V.fromList
      [ (h, fromIntegral (optTxBytes opts) :: BytesSize)
      | h <- ebTxHashesFor opts ebIdx
      ]

-- | Unique 'TxHash': \"txHash:<ebIdx>:<txIdx>\" padded to 32 bytes with zeros.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx txIdx = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("txHash:" <> show ebIdx <> ":" <> show txIdx)

-- | Shared-pool 'TxHash': \"sharedTx:<poolIdx>\" padded to 32 bytes with zeros.
genSharedTxHash :: Int -> TxHash
genSharedTxHash j = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("sharedTx:" <> show j)

-- | Generate a TX payload: the TX hash bytes padded with zeros to 'optTxBytes'.
genTx :: Opts -> TxHash -> BS.ByteString
genTx opts (MkTxHash h) = h <> BS.replicate (optTxBytes opts - BS.length h) 0

-- * Measurement phases

-- | One measurement phase: what was measured with exactly 'prGcable' EBs
-- below the GC frontier.
data PhaseResult = PhaseResult
  { prScenario :: !String
  , prPacing :: !String
  , prGcable :: !Int
  , prResident :: !Int
  , prPromoted :: !Int
  , prInsertEbWall :: !DiffTime
  -- ^ median over the phase's timed full-EB insertions
  , prPromoteWall :: !DiffTime
  , prCopyWaitWall :: !DiffTime
  , prCopyEbWall :: !DiffTime
  -- ^ median duration of the copier's per-EB @copyToImmutable@ spans
  , prProbeWall :: !DiffTime
  -- ^ median duration of the copier's @completenessProbe@ sub-spans
  , prMarkWall :: !DiffTime
  , prSweepWall :: !DiffTime
  , prReinitWall :: !DiffTime
  -- ^ the sweeper's one-off GC-candidates initialisation, when it ran in
  -- this phase (0 otherwise)
  , prEbLoopWall :: !DiffTime
  -- ^ total EB-eviction loop time (batches + pacing sleeps)
  , prOrphanLoopWall :: !DiffTime
  -- ^ total orphan-tx loop time (batches + pacing sleeps)
  , prCheckpointWall :: !DiffTime
  -- ^ total WAL checkpoint time across the phase's sweep passes
  , prStats :: !CycleStats
  , prTickLat :: !DiffTime
  }

runPhases ::
  Opts ->
  LeiosDbHandle IO ->
  IO [TraceLeiosDb] ->
  IORef DiffTime ->
  IO Int ->
  [(Word64, BS.ByteString)] ->
  Int ->
  IO [PhaseResult]
runPhases opts db flushEvents latRef sweepBacklog schedule immBefore = do
  conn <- open db
  remainingRef <- newIORef schedule
  promotedRef <- newIORef (0 :: Int)
  freshRef <- newIORef (0 :: Int)
  -- Time one fresh full-EB insertion (announcement + body + txs) through the
  -- given connection; the fresh EBs live at slots no frontier ever reaches.
  let timedInsertEbWith c = do
        k <- atomicModifyIORef' freshRef (\cnt -> (cnt + 1, cnt))
        let ebIdx = 10_000_000 + k
            point = MkLeiosPoint (SlotNo (2_000_000_000 + fromIntegral k)) (genEbHash ebIdx)
            eb = genEb opts ebIdx
            txs = [(h, genTx opts h) | h <- ebTxHashesFor opts ebIdx]
        snd
          <$> timed
            ( do
                leiosDbInsertEbPoint c point (leiosEbBytesSize eb)
                _ <- leiosDbInsertEbBody c point eb
                _ <- leiosDbInsertTxs c txs
                pure ()
            )
  _ <- flushEvents -- discard events from handle setup
  _ <- atomicModifyIORef' latRef (\m -> (0, m))
  results <- forM (zip [1 :: Int ..] (phaseSeries (optScenario opts))) $ \(i, (scenario, n)) -> do
    remaining <- readIORef remainingRef
    let (due0, rest0) = splitAt n remaining
    if length due0 < n
      then do
        hPutStrLn stderr $
          "phase " <> show i <> ": skipped (wants " <> show n <> " EBs, schedule exhausted)"
        pure Nothing
      else do
        -- The frontier that makes exactly the next N EBs collectable; EBs
        -- sharing the last covered slot (fixture mode) are pulled in too.
        let frontier = case due0 of
              [] -> case rest0 of
                (s, _) : _ -> s
                [] -> 0
              _ -> fst (last due0) + 1
            (dueExtra, rest) = span ((< frontier) . fst) rest0
            due = due0 ++ dueExtra
        writeIORef remainingRef rest
        -- 1. quiescent full-EB insertion latency at the current load
        insertWalls <- replicateM insertSamples (timedInsertEbWith conn)
        resident <- volatileEbs <$> leiosDbSampleStats db
        -- 2. promote a fraction, wait for the copier to land them
        let nPromote =
              -- ceiling, so the 1-EB steady phases promote their EB and
              -- exercise the copier (floor would promote none)
              ceiling (promoteFraction * fromIntegral (length due) :: Double)
        (_, promoteWall) <- timed $
          forM_ (take nPromote due) $ \(s, h) ->
            leiosDbPromoteToImmutable db (MkLeiosPoint (SlotNo s) (MkEbHash h))
        promotedTotal <- atomicModifyIORef' promotedRef (\c -> (c + nPromote, c + nPromote))
        (_, copyWaitWall) <- timed $ awaitCopier (immBefore + promotedTotal)
        -- 3. GC: mark, then wait for the sweeper to drain
        (_, markWall) <- timed $ leiosDbGarbageCollect db (SlotNo frontier)
        (_, sweepWall) <- timed $ awaitZero "sweep" sweepBacklog
        tickLat <- atomicModifyIORef' latRef (\m -> (0, m))
        evs <- flushEvents
        let stats = List.foldl' addEvent emptyCycleStats evs
            result =
              PhaseResult
                { prScenario = scenario
                , prPacing = gcPacingName (optGcPacing opts)
                , prGcable = length due
                , prResident = resident
                , prPromoted = nPromote
                , prInsertEbWall = medianTime insertWalls
                , prPromoteWall = promoteWall
                , prCopyWaitWall = copyWaitWall
                , prCopyEbWall = medianTime (spanDurations "copyToImmutable" evs)
                , prProbeWall = medianTime (spanDurations "completenessProbe" evs)
                , prMarkWall = markWall
                , prSweepWall = sweepWall
                , prReinitWall = sum (spanDurations "reinitialiseGcTxCandidates" evs)
                , prEbLoopWall = sum (spanDurations "sweepEbBatch" evs)
                , prOrphanLoopWall = sum (spanDurations "sweepOrphanBatch" evs)
                , prCheckpointWall = sum (spanDurations "walCheckpoint" evs)
                , prStats = stats
                , prTickLat = tickLat
                }
        putStrLn (renderPhase i result)
        pure (Just result)
  close conn
  pure (catMaybes results)
 where
  awaitCopier target = go (0 :: Int)
   where
    go n = do
      stats <- leiosDbSampleStats db
      if immutableEbs stats >= target
        then pure ()
        else
          if n > 3_600_000
            then
              die $
                "copy wait timed out: immutableEbs = "
                  <> show (immutableEbs stats)
                  <> ", expected "
                  <> show target
            else do
              threadDelay 1_000
              go (n + 1)

medianTime :: [DiffTime] -> DiffTime
medianTime [] = 0
medianTime ts = List.sort ts !! (length ts `div` 2)

-- | Durations of the named call-trace spans among the collected events.
spanDurations :: String -> [TraceLeiosDb] -> [DiffTime]
spanDurations name evs =
  [ cmDuration m
  | TraceLeiosDbCall (SomeJsonCallTrace ct) <- evs
  , ciCallName (ctCallInfo ct) == name
  , CallEnd _ m <- [ctEvent ct]
  ]

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
        if n > 3_600_000
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
    [ "phase"
    , "scenario"
    , "gcPacing"
    , "gcableEbs"
    , "residentEbs"
    , "promotedEbs"
    , "copiedEbRows"
    , "insertEbSeconds"
    , "promoteSeconds"
    , "copyWaitSeconds"
    , "copyEbSeconds"
    , "evictedEbRows"
    , "markSeconds"
    , "sweepSeconds"
    , "reinitGcCandidatesSeconds"
    , "ebLoopSeconds"
    , "orphanLoopSeconds"
    , "walCheckpointSeconds"
    ]

renderPhase :: Int -> PhaseResult -> String
renderPhase
  i
  PhaseResult
    { prScenario
    , prPacing
    , prGcable
    , prResident
    , prPromoted
    , prInsertEbWall
    , prPromoteWall
    , prCopyWaitWall
    , prCopyEbWall
    , prMarkWall
    , prSweepWall
    , prReinitWall
    , prEbLoopWall
    , prOrphanLoopWall
    , prCheckpointWall
    , prStats = cs
    } =
    List.intercalate
      ","
      [ show i
      , prScenario
      , prPacing
      , show prGcable
      , show prResident
      , show prPromoted
      , show (csCopied cs)
      , showSeconds prInsertEbWall
      , showSeconds prPromoteWall
      , showSeconds prCopyWaitWall
      , showSeconds prCopyEbWall
      , show (csEvicted cs)
      , showSeconds prMarkWall
      , showSeconds prSweepWall
      , showSeconds prReinitWall
      , showSeconds prEbLoopWall
      , showSeconds prOrphanLoopWall
      , showSeconds prCheckpointWall
      ]

renderStats :: String -> LeiosDbStats -> String
renderStats label LeiosDbStats{volatileEbs, immutableEbs} =
  unwords
    [ label <> ":"
    , "volatile ebs=" <> show volatileEbs
    , "| immutable ebs=" <> show immutableEbs
    ]

renderSummary :: Opts -> [PhaseResult] -> String
renderSummary opts results =
  unlines $
    [""]
      <> ebGeometry
      <> [ "Totals over " <> show (length results) <> " phases:"
         , "  copied ebs            = " <> show (sum (map (csCopied . prStats) results))
         , "  evicted ebs           = " <> show (sum (map (csEvicted . prStats) results))
         , "  insert collisions     = " <> show (sum (map (csCollisions . prStats) results))
         , "  copy errors           = " <> show (sum (map (csCopyErrors . prStats) results))
         , "  sweep errors          = " <> show (sum (map (csSweepErrors . prStats) results))
         ]
      <> concatMap scenarioBlock (List.nub (map prScenario results))
 where
  scenarioBlock scenario =
    [ ""
    , scenario
        <> " ("
        <> show (length rs)
        <> " phases, "
        <> show (sum (map prGcable rs))
        <> " EBs GC'd):"
    , stat "insert eb (median)" (map prInsertEbWall rs)
    , stat "promote           " (map prPromoteWall rs)
    , stat "copy wait         " (map prCopyWaitWall rs)
    , stat "copy eb (median)  " (map prCopyEbWall rs)
    , stat "probe (median)    " (map prProbeWall rs)
    , stat "mark              " (map prMarkWall rs)
    , stat "sweep             " (map prSweepWall rs)
    , stat "gc-candidates init" (map prReinitWall rs)
    , stat "eb loop           " (map prEbLoopWall rs)
    , stat "orphan loop       " (map prOrphanLoopWall rs)
    , stat "wal checkpoint    " (map prCheckpointWall rs)
    , stat "tick latency      " (map prTickLat rs)
    ]
   where
    rs = [r | r <- results, prScenario r == scenario]

  -- Synthetic-mode EB geometry; a fixture's is whatever the file holds.
  ebGeometry = case optDbPath opts of
    Just _ -> []
    Nothing ->
      [ "  EB body               = " <> showKb (fromIntegral (leiosEbBytesSize (genEb opts 0)))
      , "  EB closure            = " <> showKb (optTxsPerEb opts * optTxBytes opts)
      , "  txs per EB            = " <> show (optTxsPerEb opts)
      , "  tx size               = " <> show (optTxBytes opts) <> " B"
      ]

  showKb :: Int -> String
  showKb n = printf "%.1f KB" (fromIntegral n / 1024 :: Double)

  stat label ts
    | null ts = "  " <> label <> ": (no phases)"
    | otherwise =
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
