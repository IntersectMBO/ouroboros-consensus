{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Concurrent benchmark of the __real__ mempool's shared-state access
-- patterns, modelling the proto-devnet baseline.
--
-- We open the actual mempool ('openMempoolWithoutSyncThread') over a mocked
-- ledger interface whose forker reads inject a configurable latency to model
-- the on-disk (LSM) UTxO reads. Three roles run concurrently against it, as in
-- a node under tx-submission load:
--
-- * __Adders__ (tx-submission clients): each submits an independent chain of
--   transactions via the real 'addTx', rate-limited to a target TPS (like the
--   tx-firehose).
--
-- * __Readers__ (tx-submission servers / forging): tight loop of the real
--   'getSnapshot' (@readTMVar istate@), measuring how long a read blocks.
--
-- * __Syncer__ (the mempool sync thread): periodically advances the ledger tip
--   and runs the real 'testSyncWithLedger', which revalidates the whole mempool
--   through the latency-injected forker while holding the state lock.
--
-- The goal of this first version is to reproduce the baseline: the mempool
-- keeps up with the submission rate, with periodic reader/adder stalls whose
-- size grows with occupancy — matching what we measure on the devnet.
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
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Util.Orphans.IOLike ()
import Text.Read (readMaybe)

-- * Configuration (all overridable via environment variables)

envInt :: String -> Int -> Int
envInt k d = unsafePerformIO $ maybe d id . (>>= readMaybe) <$> lookupEnv k

envDouble :: String -> Double -> Double
envDouble k d = unsafePerformIO $ maybe d id . (>>= readMaybe) <$> lookupEnv k

{-# NOINLINE durationSec #-}
durationSec :: Double
durationSec = envDouble "DURATION" 20

-- | Number of N2N peers. Each peer contributes exactly one tx-submission
-- __server__ (a reader serving txs to that peer) and one tx-submission
-- __client__ (a remote adder feeding txs received from that peer). For the
-- frozen 3-node baseline, node1 has 2 peers (node2, node3).
{-# NOINLINE numPeers #-}
numPeers :: Int
numPeers = envInt "PEERS" 2

-- | Number of local (N2C) clients — the tx-firehose(s). They add on behalf of a
-- local client (higher fifo priority). Baseline runs 1.
{-# NOINLINE numLocalClients #-}
numLocalClients :: Int
numLocalClients = envInt "LOCAL_CLIENTS" 1

-- | tx-submission servers = one per peer.
numReaders :: Int
numReaders = numPeers

-- | tx-submission clients = one per peer (N2N) + the local firehose(s) (N2C).
numAdders :: Int
numAdders = numPeers + numLocalClients

-- | Total target submission rate across all adders (tx/s). Matches the
-- proto-devnet baseline firehose (TPS=100). @TPS=0@ means unbounded (adders
-- submit as fast as they can), to measure the mempool's max sustained rate.
{-# NOINLINE targetTpsTotal #-}
targetTpsTotal :: Double
targetTpsTotal = envDouble "TPS" 100

-- | How often the syncer advances the tip + revalidates. The devnet adopts a
-- block roughly every ~20 s; a shorter period here exercises the sync
-- contention more often.
{-# NOINLINE syncPeriodSec #-}
syncPeriodSec :: Double
syncPeriodSec = envDouble "SYNC_PERIOD" 5

-- | Fixed cost of a forker table read (models one LSM round-trip), microseconds.
{-# NOINLINE readBaseMicros #-}
readBaseMicros :: Int
readBaseMicros = envInt "READ_BASE_US" 500

-- | Additional per-key cost of a forker table read (models per-UTxO LSM
-- lookup), microseconds. This is what makes a full-mempool sync read scale with
-- occupancy.
{-# NOINLINE readPerKeyMicros #-}
readPerKeyMicros :: Int
readPerKeyMicros = envInt "READ_PERKEY_US" 200

-- | Pause between successive 'getSnapshot's per reader, microseconds. A reader
-- models the tx-submission /server/ for one downstream peer, which reads the
-- mempool on request rather than in a spin. In the proto-devnet each downstream
-- peer pulled ~3–4 tx-body requests/s from node1 (node2 2.7/s, node3 4.0/s over
-- multi-hour runs), and with the txid requests on top a server reads roughly
-- 5–8×/s per peer — a read every ~125–200ms. The default models ~7 reads/s/peer;
-- set @0@ for the old tight loop (only sensible for a handful of readers, else
-- hundreds of spinning O(occupancy) readers just measure CPU saturation).
{-# NOINLINE readPeriodMicros #-}
readPeriodMicros :: Int
readPeriodMicros = envInt "READ_PERIOD_US" 150000

-- | Disjoint token namespace per adder so their chains never collide.
chainStride :: Int
chainStride = 1_000_000_000

capacityOverride :: MempoolCapacityBytesOverride
capacityOverride = MempoolCapacityBytesOverride (ByteSize32 100_000_000)

-- * Main

main :: IO ()
main = do
  putStr $
    unlines
      [ "Mempool shared-state concurrent benchmark (real mempool)"
      , "  duration      : " <> show durationSec <> " s"
      , "  peers         : " <> show numPeers <> " (=> " <> show numReaders <> " servers/readers)"
      , "  clients       : "
          <> show numAdders
          <> " ("
          <> show numPeers
          <> " N2N + "
          <> show numLocalClients
          <> " local, target "
          <> show targetTpsTotal
          <> " tx/s total)"
      , "  sync period   : " <> show syncPeriodSec <> " s"
      , "  forker read   : " <> show readBaseMicros <> " us + " <> show readPerKeyMicros <> " us/key"
      , ""
      ]
  let seeds = [Token (j * chainStride) | j <- [0 .. numAdders - 1]]
      baseLedger = mkInitialLedgerState seeds
  ledgerVar <- newTVarIO baseLedger
  mempool <-
    openMempoolWithoutSyncThread
      (latencyLedgerInterface ledgerVar)
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
        pure (realToFrac (diffTime now start) >= durationSec)

  syncer <- async (runSyncer expired mempool ledgerVar baseLedger syncDursRef)
  readers <-
    forM [1 .. numReaders] $ \_ ->
      async (runReader expired mempool readRef maxReadLatRef)
  adders <-
    forM [0 .. numAdders - 1] $ \j -> do
      let onBehalf = if j < numPeers then AddTxForRemotePeer else AddTxForLocalClient
      async (runAdder expired mempool onBehalf j addedRef)

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
runAdder :: IO Bool -> Mempool IO TestBlock -> AddTxOnBehalfOf -> Int -> IORef Int -> IO ()
runAdder expired mempool onBehalf j addedRef = go 0
 where
  base = j * chainStride
  intervalMicros =
    if targetTpsTotal <= 0
      then 0
      else round (1_000_000 * fromIntegral numAdders / targetTpsTotal)
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

-- | Reader: real 'getSnapshot' in a tight loop, recording max read latency.
runReader :: IO Bool -> Mempool IO TestBlock -> IORef Int -> IORef DiffTime -> IO ()
runReader expired mempool readRef maxLatRef = go
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
        when (readPeriodMicros > 0) $ Conc.threadDelay readPeriodMicros
        go

-- | Syncer: every 'syncPeriodSec', advance the tip and run the real sync.
runSyncer ::
  IO Bool ->
  Mempool IO TestBlock ->
  StrictTVar IO (LedgerState TestBlock ValuesMK) ->
  LedgerState TestBlock ValuesMK ->
  IORef [DiffTime] ->
  IO ()
runSyncer expired mempool ledgerVar baseLedger syncDursRef = go 1
 where
  go :: Int -> IO ()
  go n = do
    Conc.threadDelay (round (1_000_000 * syncPeriodSec))
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
  StrictTVar IO (LedgerState TestBlock ValuesMK) ->
  LedgerInterface IO TestBlock
latencyLedgerInterface ledgerVar =
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
                        Conc.threadDelay (readBaseMicros + readPerKeyMicros * keysCount keys)
                        pure (projectLedgerTables st `restrictValues'` keys)
                    , roforkerReadStatistics = pure (Statistics 0)
                    , roforkerRangeReadTables = \_ -> pure (emptyLedgerTables, Nothing)
                    }
            )
    }

keysCount :: LedgerTables (LedgerState TestBlock) KeysMK -> Int
keysCount (LedgerTables (KeysMK s)) = Set.size s

-- * Formatting

showT :: DiffTime -> String
showT t
  | s < 1e-3 = show (round (s * 1_000_000) :: Int) <> " us"
  | s < 1 = show (round (s * 1_000) :: Int) <> " ms"
  | otherwise = show (fromIntegral (round (s * 1000) :: Int) / 1000 :: Double) <> " s"
 where
  s = realToFrac t :: Double
