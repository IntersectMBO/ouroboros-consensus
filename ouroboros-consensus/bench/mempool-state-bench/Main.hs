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

-- | Number of N2N peers. Each peer contributes one tx-submission __server__ (a
-- reader serving txs to that peer) and one tx-submission __client__ (a remote
-- adder feeding txs received from that peer).
{-# NOINLINE numPeers #-}
numPeers :: Int
numPeers = envInt "PEERS" 2

-- | Number of local (N2C) clients. They add on behalf of a local client (higher
-- fifo priority).
{-# NOINLINE numLocalClients #-}
numLocalClients :: Int
numLocalClients = envInt "LOCAL_CLIENTS" 1

-- | tx-submission servers = one per peer.
numReaders :: Int
numReaders = numPeers

-- | tx-submission clients = one per peer (N2N) + the local clients (N2C).
numAdders :: Int
numAdders = numPeers + numLocalClients

-- | Total target submission rate across all adders (tx/s). @TPS=0@ means
-- unbounded (adders submit as fast as they can), to measure the mempool's max
-- sustained rate.
{-# NOINLINE targetTpsTotal #-}
targetTpsTotal :: Double
targetTpsTotal = envDouble "TPS" 100

-- | How often the syncer advances the tip + revalidates. A chain adopts a block
-- roughly every ~20 s; a shorter period here exercises the sync contention more
-- often. This is strictly periodic, unlike real block adoption.
{-# NOINLINE syncPeriodSec #-}
syncPeriodSec :: Double
syncPeriodSec = envDouble "SYNC_PERIOD" 5

-- | Fixed cost of a forker table read (models one on-disk round-trip), microseconds.
{-# NOINLINE readBaseMicros #-}
readBaseMicros :: Int
readBaseMicros = envInt "READ_BASE_US" 500

-- | Additional per-key cost of a forker table read (models per-UTxO on-disk
-- lookup), microseconds. This is what makes a full-mempool sync read scale with
-- occupancy.
{-# NOINLINE readPerKeyMicros #-}
readPerKeyMicros :: Int
readPerKeyMicros = envInt "READ_PERKEY_US" 200

-- | Pause between successive 'getSnapshot's per reader, microseconds. A reader
-- models the tx-submission /server/ for one downstream peer, which reads the
-- mempool on request rather than in a spin. On a Leios testnet each downstream
-- peer pulled ~3–4 tx-body requests/s from a relay, and with the txid requests
-- on top a server reads roughly 5–8×/s per peer — a read every ~125–200ms. The
-- default models ~7 reads/s/peer; set @0@ for a tight loop (only sensible for a
-- handful of readers, else hundreds of spinning O(occupancy) readers just
-- measure CPU saturation).
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

-- | Reader: real 'getSnapshot' on the configured per-reader cadence
-- ('readPeriodMicros'), recording max read latency.
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
