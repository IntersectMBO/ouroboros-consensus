{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Concurrent benchmark of the __real__ mempool's shared-state access patterns
-- under a Leios-scale transaction load.
--
-- It opens the actual mempool ('openMempoolWithoutSyncThread') over a mocked
-- ledger interface whose forker reads inject a configurable latency to model
-- on-disk UTxO reads. Three roles run concurrently against it, as in a node
-- under tx-submission load:
--
-- * __Adders__ (tx-submission clients and local clients): each submits an
--   independent chain of transactions via the real 'addTx', rate-limited to a
--   target rate.
--
-- * __Readers__ (tx-submission servers / block forging): call the real
--   'getSnapshot' (@readTMVar istate@) on a configurable per-peer cadence,
--   measuring how long a read blocks.
--
-- * __Syncer__ (the mempool sync thread): periodically advances the ledger tip
--   and runs the real 'testSyncWithLedger', which revalidates the mempool
--   through the latency-injected forker.
--
-- With the mempool holding many transactions, this reproduces the contention
-- between revalidation, ingestion and serving that the mempool sync targets.
module Main (main) where

import Bench.Consensus.Mempool.TestBlock
  ( TestBlock
  , Token (Token)
  , advanceTip
  , mkInitialLedgerState
  , mkTx
  , sampleLedgerConfig
  )
import qualified Control.Concurrent as Conc
import Control.Concurrent.Async (async, wait)
import Control.Exception (evaluate)
import Control.Monad (forM, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (nullTracer)
import Data.IORef
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Options.Applicative
  ( Parser
  , auto
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , showDefault
  , value
  , (<**>)
  )
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (ByteSize32))
import Ouroboros.Consensus.Ledger.Tables
  ( KeysMK (KeysMK)
  , LedgerTables (LedgerTables)
  , ValuesMK
  , projectLedgerTables
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( emptyLedgerTables
  , forgetLedgerTables
  , restrictValues'
  )
import Ouroboros.Consensus.Mempool
  ( Mempool (addTx, getSnapshot, testSyncWithLedger)
  , MempoolCapacityBytesOverride (MempoolCapacityBytesOverride)
  , openMempoolWithoutSyncThread
  , snapshotTxs
  )
import Ouroboros.Consensus.Mempool.API
  ( AddTxOnBehalfOf (AddTxForLocalClient, AddTxForRemotePeer)
  , isMempoolTxAdded
  )
import Ouroboros.Consensus.Mempool.Impl.Common
  ( LedgerInterface (LedgerInterface, getCurrentLedgerState)
  , MempoolLedgerDBView (MempoolLedgerDBView)
  )
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ReadOnlyForker (..)
  , Statistics (Statistics)
  )
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Test.Util.Orphans.IOLike ()

-- * Configuration (command-line options, each with a default)

-- | Benchmark parameters, parsed from the command line by 'configParser'.
data Config = Config
  { cfgDurationSec :: !Double
  , cfgNumPeers :: !Int
  -- ^ Number of N2N peers. Each peer contributes one tx-submission __server__ (a
  -- reader serving txs to that peer) and one tx-submission __client__ (a remote
  -- adder feeding txs received from that peer).
  , cfgNumLocalClients :: !Int
  -- ^ Number of local (N2C) clients. They add on behalf of a local client
  -- (higher fifo priority).
  , cfgTargetTpsTotal :: !Double
  -- ^ Total target submission rate across all adders (tx/s). @0@ means unbounded
  -- (adders submit as fast as they can), to measure the mempool's max sustained
  -- rate.
  , cfgSyncPeriodSec :: !Double
  -- ^ How often the syncer advances the tip + revalidates. A chain adopts a
  -- block roughly every ~20 s; a shorter period here exercises the sync
  -- contention more often. This is strictly periodic, unlike real block
  -- adoption.
  , cfgReadBaseMicros :: !Int
  -- ^ Fixed cost of a forker table read (models one on-disk round-trip),
  -- microseconds.
  , cfgReadPerKeyMicros :: !Int
  -- ^ Additional per-key cost of a forker table read (models per-UTxO on-disk
  -- lookup), microseconds. This is what makes a full-mempool sync read scale
  -- with occupancy.
  , cfgReadPeriodMicros :: !Int
  -- ^ Pause between successive 'getSnapshot's per reader, microseconds. A reader
  -- models the tx-submission /server/ for one downstream peer, which reads the
  -- mempool on request rather than in a spin. On a Leios testnet each downstream
  -- peer pulled ~3–4 tx-body requests/s from a relay, and with the txid requests
  -- on top a server reads roughly 5–8×/s per peer — a read every ~125–200ms. The
  -- default models ~7 reads/s/peer; set @0@ for a tight loop (only sensible for a
  -- handful of readers, else hundreds of spinning O(occupancy) readers just
  -- measure CPU saturation).
  }

configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      ( long "duration"
          <> metavar "SECONDS"
          <> value 20
          <> showDefault
          <> help "Benchmark duration"
      )
    <*> option
      auto
      ( long "peers"
          <> metavar "N"
          <> value 2
          <> showDefault
          <> help "Number of N2N peers (each => one tx-submission server/reader and one remote adder)"
      )
    <*> option
      auto
      ( long "local-clients"
          <> metavar "N"
          <> value 1
          <> showDefault
          <> help "Number of local (N2C) clients (higher fifo priority)"
      )
    <*> option
      auto
      ( long "tps"
          <> metavar "TXS_PER_SEC"
          <> value 100
          <> showDefault
          <> help "Total target submission rate across all adders; 0 = unbounded"
      )
    <*> option
      auto
      ( long "sync-period"
          <> metavar "SECONDS"
          <> value 5
          <> showDefault
          <> help "How often the syncer advances the tip and revalidates"
      )
    <*> option
      auto
      ( long "read-base-us"
          <> metavar "US"
          <> value 0
          <> showDefault
          <> help
            "Fixed per-read cost. 0 by default: the measured LSM read cost scales \
            \linearly with the number of txs (no batching — a round trip per tx), \
            \so it is charged per key below, not as a fixed component."
      )
    <*> option
      auto
      ( long "read-per-key-us"
          <> metavar "US"
          <> value 60
          <> showDefault
          <> help
            "Per-key cost of a forker table read. Default 60us, midpoint of the \
            \~40-80us/tx measured on the LSM backend (see \
            \input-output-hk/ouroboros-leios#553); this bench's txs have ~1 input \
            \each, so per-key ~= per-tx here."
      )
    <*> option
      auto
      ( long "read-period-us"
          <> metavar "US"
          <> value 150000
          <> showDefault
          <> help "Pause between successive getSnapshots per reader; 0 = tight loop"
      )

-- | tx-submission servers = one per peer.
numReaders :: Config -> Int
numReaders cfg = cfgNumPeers cfg

-- | tx-submission clients = one per peer (N2N) + the local clients (N2C).
numAdders :: Config -> Int
numAdders cfg = cfgNumPeers cfg + cfgNumLocalClients cfg

-- | Disjoint token namespace per adder so their chains never collide.
chainStride :: Int
chainStride = 1_000_000_000

capacityOverride :: MempoolCapacityBytesOverride
capacityOverride = MempoolCapacityBytesOverride (ByteSize32 100_000_000)

-- * Main

main :: IO ()
main = do
  cfg <-
    execParser $
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc
              "Concurrent benchmark of the real mempool's shared-state access under Leios-scale load"
        )
  putStr $
    unlines
      [ "Mempool shared-state concurrent benchmark (real mempool)"
      , "  duration      : " <> show (cfgDurationSec cfg) <> " s"
      , "  peers         : "
          <> show (cfgNumPeers cfg)
          <> " (=> "
          <> show (numReaders cfg)
          <> " servers/readers)"
      , "  clients       : "
          <> show (numAdders cfg)
          <> " ("
          <> show (cfgNumPeers cfg)
          <> " N2N + "
          <> show (cfgNumLocalClients cfg)
          <> " local, target "
          <> show (cfgTargetTpsTotal cfg)
          <> " tx/s total)"
      , "  sync period   : " <> show (cfgSyncPeriodSec cfg) <> " s"
      , "  forker read   : "
          <> show (cfgReadBaseMicros cfg)
          <> " us + "
          <> show (cfgReadPerKeyMicros cfg)
          <> " us/key"
      , ""
      ]
  let seeds = [Token (j * chainStride) | j <- [0 .. numAdders cfg - 1]]
      baseLedger = mkInitialLedgerState seeds
  ledgerVar <- newTVarIO baseLedger
  mempool <-
    openMempoolWithoutSyncThread
      (latencyLedgerInterface cfg ledgerVar)
      sampleLedgerConfig
      capacityOverride
      Nothing
      nullTracer

  addedRef <- newIORef (0 :: Int)
  readRef <- newIORef (0 :: Int)
  maxReadLatRef <- newIORef (0 :: DiffTime)
  syncDursRef <- newIORef ([] :: [DiffTime])

  start <- getMonotonicTime
  let expired = do
        now <- getMonotonicTime
        pure (realToFrac (diffTime now start) >= cfgDurationSec cfg)

  syncer <- async (runSyncer cfg expired mempool ledgerVar baseLedger syncDursRef)
  readers <-
    forM [1 .. numReaders cfg] $ \_ ->
      async (runReader cfg expired mempool readRef maxReadLatRef)
  adders <-
    forM [0 .. numAdders cfg - 1] $ \j -> do
      let onBehalf = if j < cfgNumPeers cfg then AddTxForRemotePeer else AddTxForLocalClient
      async (runAdder cfg expired mempool onBehalf j addedRef)

  mapM_ wait adders
  mapM_ wait readers
  wait syncer
  end <- getMonotonicTime

  finalOccupancy <- length . snapshotTxs <$> atomically (getSnapshot mempool)
  added <- readIORef addedRef
  reads' <- readIORef readRef
  maxReadLat <- readIORef maxReadLatRef
  syncDurs <- readIORef syncDursRef
  let elapsed = realToFrac (diffTime end start) :: Double
  putStr $
    unlines
      [ "Results:"
      , "  elapsed         : " <> showT (diffTime end start)
      , "  txs added       : " <> show added
      , "  final occupancy : " <> show finalOccupancy <> " txs in mempool"
      , "  throughput      : " <> show (round (fromIntegral added / elapsed) :: Int) <> " tx/s"
      , "  snapshot reads  : " <> show reads'
      , "  max read stall  : " <> showT maxReadLat
      , "  syncs           : " <> show (length syncDurs)
      , "  max sync time   : " <> showT (if null syncDurs then 0 else maximum syncDurs)
      , "  avg sync time   : "
          <> showT (if null syncDurs then 0 else sum syncDurs / fromIntegral (length syncDurs))
      ]

-- * Roles

-- | Adder @j@ submits its own chain: consume token @base+i@, produce @base+i+1@.
runAdder ::
  Config -> IO Bool -> Mempool IO TestBlock -> AddTxOnBehalfOf -> Int -> IORef Int -> IO ()
runAdder cfg expired mempool onBehalf j addedRef = go 0
 where
  base = j * chainStride
  intervalMicros =
    if cfgTargetTpsTotal cfg <= 0
      then 0
      else round (1_000_000 * fromIntegral (numAdders cfg) / cfgTargetTpsTotal cfg)
  go :: Int -> IO ()
  go i = do
    done <- expired
    if done
      then pure ()
      else do
        let tx = mkTx [Token (base + i)] [Token (base + i + 1)]
        r <- addTx mempool onBehalf tx
        when (isMempoolTxAdded r) $ atomicModifyIORef' addedRef (\c -> (c + 1, ()))
        when (intervalMicros > 0) $ Conc.threadDelay intervalMicros
        go (i + 1)

-- | Reader: real 'getSnapshot' on the configured per-reader cadence
-- ('cfgReadPeriodMicros'), recording max read latency.
runReader :: Config -> IO Bool -> Mempool IO TestBlock -> IORef Int -> IORef DiffTime -> IO ()
runReader cfg expired mempool readRef maxLatRef = go
 where
  go = do
    done <- expired
    if done
      then pure ()
      else do
        t0 <- getMonotonicTime
        snap <- atomically (getSnapshot mempool)
        _ <- evaluate (length (snapshotTxs snap))
        t1 <- getMonotonicTime
        let lat = diffTime t1 t0
        atomicModifyIORef' readRef (\c -> (c + 1, ()))
        atomicModifyIORef' maxLatRef (\m -> (max m lat, ()))
        when (cfgReadPeriodMicros cfg > 0) $ Conc.threadDelay (cfgReadPeriodMicros cfg)
        go

-- | Syncer: every 'cfgSyncPeriodSec', advance the tip and run the real sync.
runSyncer ::
  Config ->
  IO Bool ->
  Mempool IO TestBlock ->
  StrictTVar IO (LedgerState TestBlock ValuesMK) ->
  LedgerState TestBlock ValuesMK ->
  IORef [DiffTime] ->
  IO ()
runSyncer cfg expired mempool ledgerVar baseLedger syncDursRef = go 1
 where
  go :: Int -> IO ()
  go n = do
    Conc.threadDelay (round (1_000_000 * cfgSyncPeriodSec cfg))
    done <- expired
    if done
      then pure ()
      else do
        atomically $ writeTVar ledgerVar (advanceTip (fromIntegral n) baseLedger)
        t0 <- getMonotonicTime
        _ <- testSyncWithLedger mempool
        t1 <- getMonotonicTime
        atomicModifyIORef' syncDursRef (\ds -> (diffTime t1 t0 : ds, ()))
        go (n + 1)

-- * Latency-injecting ledger interface

latencyLedgerInterface ::
  Config ->
  StrictTVar IO (LedgerState TestBlock ValuesMK) ->
  LedgerInterface IO TestBlock
latencyLedgerInterface cfg ledgerVar =
  LedgerInterface
    { getCurrentLedgerState = do
        st <- readTVar ledgerVar
        pure $
          MempoolLedgerDBView
            (forgetLedgerTables st)
            ( pure $
                Right $
                  ReadOnlyForker
                    { roforkerClose = pure ()
                    , roforkerGetLedgerState = pure (forgetLedgerTables st)
                    , roforkerReadTables = \keys -> do
                        -- Busy-wait, not 'threadDelay': the read latency here is
                        -- often sub-millisecond, below 'threadDelay's timer
                        -- granularity, and it is on the critical path (held under
                        -- the mempool lock), so rounding it up to ~1ms would
                        -- badly distort the results.
                        spinMicros (cfgReadBaseMicros cfg + cfgReadPerKeyMicros cfg * keysCount keys)
                        pure (projectLedgerTables st `restrictValues'` keys)
                    , roforkerReadStatistics = pure (Statistics 0)
                    , roforkerRangeReadTables = \_ -> pure (emptyLedgerTables, Nothing)
                    }
            )
    }

keysCount :: LedgerTables (LedgerState TestBlock) KeysMK -> Int
keysCount (LedgerTables (KeysMK s)) = Set.size s

-- | Busy-wait for the given number of microseconds. Unlike 'Conc.threadDelay',
-- this honours sub-millisecond durations (which the RTS timer would round up),
-- at the cost of burning a core — appropriate for modelling a latency that sits
-- on the mempool's critical path.
spinMicros :: Int -> IO ()
spinMicros us
  | us <= 0 = pure ()
  | otherwise = do
      let target = fromIntegral us * 1_000 :: Word64
      start <- getMonotonicTimeNSec
      let go = do
            now <- getMonotonicTimeNSec
            when (now - start < target) go
      go

-- * Formatting

showT :: DiffTime -> String
showT t
  | s < 1e-3 = show (round (s * 1_000_000) :: Int) <> " us"
  | s < 1 = show (round (s * 1_000) :: Int) <> " ms"
  | otherwise = show (fromIntegral (round (s * 1000) :: Int) / 1000 :: Double) <> " s"
 where
  s = realToFrac t :: Double
