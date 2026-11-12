{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Mempool.TryAddTx (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import Control.Monad (when)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer, mkTracer, nullTracer)
import Data.List.NonEmpty (NonEmpty (..))
import Ouroboros.Consensus.Block.SupportsPeras (pattern PerasEnabled)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.Mempool.Capacity (mkCapacityBytesOverride)
import Ouroboros.Consensus.Mempool.Impl.Common (MempoolLedgerDBView (..))
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util.IOLike
import Test.Consensus.Mempool.Fairness.TestBlock
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (testBlockLedgerConfigFrom, testInitLedgerWithState)

tests :: TestTree
tests =
  testGroup
    "tryAddLocalTx"
    [ testProperty "Full does not retain the transaction" prop_full
    , testProperty "Busy preserves a waiting local submission" $ prop_waiting AddTxForLocalClient
    , testProperty "Busy preserves a waiting peer submission" $ prop_waiting AddTxForRemotePeer
    , testProperty "Concurrent attempts cannot overfill the mempool" $ prop_inFlight False
    , testProperty "Cancellation releases admission and restores the state" $ prop_inFlight True
    , testProperty "Ledger I/O failure releases admission and restores the state" prop_exception
    ]

prop_full :: Property
prop_full = once $ runSimOrThrow $ do
  mempool <- openTestMempool nullTracer (pure ())
  let fullTx = mkGenTx 0 (ByteSize32 10)
      nextTx = mkGenTx 1 (ByteSize32 1)
  added <- tryAddLocalTx mempool fullTx
  full <- tryAddLocalTx mempool nextTx
  removeTxsEvenIfValid mempool (txId fullTx :| [])
  afterRemoval <- txsInPool mempool
  retried <- tryAddLocalTx mempool nextTx
  afterRetry <- txsInPool mempool
  pure $
    conjoin
      [ fmap isMempoolTxAdded added === Right True
      , fmap isMempoolTxAdded full === Left MempoolFull
      , afterRemoval === []
      , fmap isMempoolTxAdded retried === Right True
      , afterRetry === [nextTx]
      ]

-- A larger blocking submission owns the FIFO while a smaller one could fit.
-- Wait for the second attempt trace: the first attempt acquired admission and
-- found insufficient capacity, so it is now waiting for a size change.
prop_waiting :: AddTxOnBehalfOf -> Property
prop_waiting onbehalf = once $ runSimOrThrow $ do
  let initialTx = mkGenTx 0 (ByteSize32 9)
      waitingTx = mkGenTx 1 (ByteSize32 2)
      smallTx = mkGenTx 2 (ByteSize32 1)
  attempts <- uncheckedNewTVarM (0 :: Int)
  let tracer = mkTracer $ \case
        TraceMempoolAttemptingAdd tx
          | tx == waitingTx ->
              atomically $ modifyTVar attempts (+ 1)
        _ -> pure ()
  mempool <- openTestMempool tracer (pure ())
  initial <- addTx mempool AddTxForLocalClient initialTx
  withAsync (addTx mempool onbehalf waitingTx) $ \waiting -> do
    atomically $ readTVar attempts >>= check . (>= 2)
    busy <- tryAddLocalTx mempool smallTx
    removeTxsEvenIfValid mempool (txId initialTx :| [])
    admitted <- wait waiting
    afterWaiting <- txsInPool mempool
    retried <- tryAddLocalTx mempool smallTx
    afterRetry <- txsInPool mempool
    pure $
      conjoin
        [ isMempoolTxAdded initial === True
        , fmap isMempoolTxAdded busy === Left MempoolBusy
        , isMempoolTxAdded admitted === True
        , afterWaiting === [waitingTx]
        , fmap isMempoolTxAdded retried === Right True
        , afterRetry === [waitingTx, smallTx]
        ]

-- Hold an admission in ledger I/O, with both the FIFO and state lock acquired.
-- A competing try must return Busy without waiting to inspect that state.
prop_inFlight :: Bool -> Property
prop_inFlight cancelFirst = once $ runSimOrThrow $ do
  entered <- atomically newEmptyTMVar
  paused <- uncheckedNewTVarM True
  let readTables = do
        shouldPause <- atomically $ readTVar paused
        when shouldPause $ do
          atomically $ putTMVar entered ()
          atomically $ readTVar paused >>= check . not
      firstTx = mkGenTx 0 (ByteSize32 10)
      secondTx = mkGenTx 1 (ByteSize32 10)
  mempool <- openTestMempool nullTracer readTables
  withAsync (tryAddLocalTx mempool firstTx) $ \adding -> do
    atomically $ takeTMVar entered
    busy <- tryAddLocalTx mempool secondTx
    firstResult <-
      if cancelFirst
        then cancel adding >> pure Nothing
        else do
          atomically $ writeTVar paused False
          Just <$> wait adding
    atomically $ writeTVar paused False
    retried <- tryAddLocalTx mempool secondTx
    contents <- txsInPool mempool
    pure $
      conjoin
        [ fmap isMempoolTxAdded busy === Left MempoolBusy
        , fmap (fmap isMempoolTxAdded) firstResult
            === if cancelFirst then Nothing else Just (Right True)
        , fmap isMempoolTxAdded retried
            === if cancelFirst then Right True else Left MempoolFull
        , contents === [if cancelFirst then secondTx else firstTx]
        ]

data TestException = TestException deriving Show

instance Exception TestException

prop_exception :: Property
prop_exception = once $ runSimOrThrow $ do
  failing <- uncheckedNewTVarM True
  let readTables = do
        shouldFail <- atomically $ readTVar failing
        when shouldFail $ throwIO TestException
      tx = mkGenTx 0 (ByteSize32 10)
  mempool <- openTestMempool nullTracer readTables
  failed <- catch (tryAddLocalTx mempool tx >> pure False) (\TestException -> pure True)
  afterFailure <- txsInPool mempool
  atomically $ writeTVar failing False
  retried <- tryAddLocalTx mempool tx
  contents <- txsInPool mempool
  pure $
    conjoin
      [ failed === True
      , afterFailure === []
      , fmap isMempoolTxAdded retried === Right True
      , contents === [tx]
      ]

txsInPool :: IOLike m => Mempool m TestBlock -> m [GenTx TestBlock]
txsInPool mempool =
  map (\(tx, _, _) -> txForgetValidated tx) . snapshotTxs
    <$> atomically (getSnapshot mempool)

-- The fairness mock supplies independent, valid transactions of an exact byte
-- size. Its block capacity is one byte, so the override is exactly ten bytes.
openTestMempool ::
  (IOLike m, MonadTimer m) =>
  Tracer m (TraceEventMempool TestBlock) ->
  m () ->
  m (Mempool m TestBlock)
openTestMempool tracer readTables =
  openMempoolWithoutSyncThread
    ledgerInterface
    (testBlockLedgerConfigFrom eraParams)
    (mkCapacityBytesOverride $ ByteSize32 10)
    Nothing
    tracer
 where
  ledgerState = testInitLedgerWithState NoPayLoadDependentState
  ledgerInterface =
    LedgerInterface $
      pure $
        MempoolLedgerDBView ledgerState $
          pure $
            Right $
              ReadOnlyForker
                { roforkerClose = pure ()
                , roforkerReadTables = const $ readTables >> pure emptyLedgerTables
                , roforkerRangeReadTables = const $ pure (emptyLedgerTables, Nothing)
                , roforkerGetLedgerState = pure ledgerState
                , roforkerReadStatistics = pure $ Statistics 0
                }
  eraParams =
    HardFork.defaultEraParams
      (SecurityParam $ knownNonZeroBounded @10)
      (Time.slotLengthFromSec 2)
      (PerasEnabled ())
