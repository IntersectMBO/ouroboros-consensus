{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the time-capped snapshot reapplication
-- ('Ouroboros.Consensus.Mempool.Query').
module Test.Consensus.Mempool.Query (tests) where

import Cardano.Slotting.Slot (SlotNo, WithOrigin (At, Origin))
import Control.Monad (unless)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.Except (runExcept)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Foldable (foldlM)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.Word (Word64)
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , DiffMK
  , HasLedgerTables (projectLedgerTables)
  , KeysMK (KeysMK)
  , LedgerConfig
  , TickedLedgerState
  , ValuesMK (ValuesMK)
  , applyChainTick
  , ledgerTipSlot
  )
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (ByteSize32)
  , Invalidated (getInvalidated)
  , LedgerSupportsMempool (applyTx, txForgetValidated)
  , TxId
  , WhetherToIntervene (DoNotIntervene)
  , txId
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( applyDiffs
  , emptyLedgerTables
  , forgetLedgerTables
  , noNewTickingDiffs
  )
import Ouroboros.Consensus.Mempool
  ( Mempool (getSnapshotFor)
  , MempoolSnapshot (snapshotTxs)
  , addTxs
  , isMempoolTxAdded
  , openMempoolWithoutSyncThread
  )
import Ouroboros.Consensus.Mempool.API (MempoolMeasure)
import Ouroboros.Consensus.Mempool.Capacity
  ( MempoolCapacityBytesOverride
      ( MempoolCapacityBytesOverride
      , NoMempoolCapacityBytesOverride
      )
  )
import Ouroboros.Consensus.Mempool.Impl.Common
  ( InternalState (isLedgerState, isTxs)
  , LedgerInterface (..)
  , MempoolEnv (..)
  , MempoolLedgerDBView (..)
  , RevalidateTxsResult (..)
  , TraceEventMempool (..)
  , ValidatedTxWithDiffs (..)
  , initInternalState
  )
import Ouroboros.Consensus.Mempool.Query
  ( revalidateUntilTimeout
  , snapshotTxsPerStep
  )
import Ouroboros.Consensus.Mempool.TxSeq (TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Mock.Ledger hiding (TxId, TxIn, TxOut)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ReadOnlyForker (..)
  , Statistics (..)
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.TimeBoundedLoop (DoneType (..))
import Test.Consensus.Mempool.Util
  ( TestBlock
  , TestTx
  , applyTxToLedger
  , applyTxToTickedLedger
  , bumpTip
  , genConnectedTxLattice
  , genTickedLedgerState
  , genValidTxs
  , mustBeValid
  , testInitLedger
  , testLedgerConfigNoSizeLimits
  )
import Test.Crypto.Hash ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests =
  testGroup
    "Mempool.Query"
    [ testProperty
        "a dependent tx lattice reapplies fully, in order, for any step size"
        prop_latticeAllValid
    , testProperty
        "txs not included by the new tip survive reapplication, for any step size"
        prop_txChainSurvivesNewTip
    , testProperty
        "getSnapshotFor at the cached state reads nothing and returns everything"
        prop_getSnapshotForCachedPaths
    , testProperty
        "getSnapshotFor at a new tip containing some mempool txs drops exactly those txs"
        prop_getSnapshotForNewTip
    , testProperty
        "getSnapshotFor stops at the time limit, snapshotting a prefix"
        prop_getSnapshotForTimeout
    ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | Reapplying a connected lattice of dependent valid transactions at the
-- same tip keeps every transaction, in order, regardless of the step size,
-- and the resulting ledger state matches applying the transactions
-- sequentially.
prop_latticeAllValid :: Property
prop_latticeAllValid =
  forAll genTickedLedgerState $ \(slot, initSt) ->
    forAllBlind (genLatticeTxs (1, 5) slot initSt) $ \allTxs ->
      forAll (choose (1, length allTxs + 1)) $ \stepSize ->
        let tickets = buildTicketList slot allTxs initSt
            (RevalidateTxsResult is removed, done) =
              runRevalidate stepSize initSt slot tickets
            expectedSt =
              mustBeValid $
                foldlM
                  (flip (applyTxToTickedLedger testCfg DoNotIntervene slot))
                  initSt
                  allTxs
         in conjoin
              [ counterexample "all txs must reapply, in order" $
                  ticketIds (TxSeq.toList (isTxs is)) === ticketIds tickets
              , counterexample "no tx may be removed" $
                  map (txId . getInvalidated) removed === []
              , counterexample "the loop must stop because it ran out of txs" $
                  done === EndReached
              , counterexample "the reapplied ledger state must match sequential application" $
                  getTickedSimpleLedgerState (applyDiffs initSt (isLedgerState is))
                    === getTickedSimpleLedgerState expectedSt
              ]

-- | Reapply a connected lattice of dependent transactions at a tip whose
-- block contains a prefix of them (the lattice is flattened in dependency
-- order, so any prefix forms a valid block).
--
-- Exactly the included transactions must be removed — their inputs are
-- already spent on-chain — and every other transaction must survive, in
-- order: their inputs exist either at the new tip, as outputs of the
-- included transactions, or as outputs of earlier surviving transactions.
-- Checked for any step size, so survivors resolve their inputs across step
-- boundaries.
prop_txChainSurvivesNewTip :: Property
prop_txChainSurvivesNewTip =
  forAll genTickedLedgerState $ \(slot, initSt) ->
    forAllBlind (genLatticeTxs (1, 5) slot initSt) $ \allTxs ->
      forAll (choose (1, length allTxs)) $ \nOnChain ->
        forAll (choose (1, length allTxs + 1)) $ \stepSize ->
          let tickets = buildTicketList slot allTxs initSt
              (onChainTxs, offChainTxs) = splitAt nOnChain allTxs
              -- The new tip: a block containing the first 'nOnChain' txs.
              newTipSt =
                foldl
                  (\st tx -> mustBeValid (applyTxToTickedLedger testCfg DoNotIntervene slot tx st))
                  initSt
                  onChainTxs
              (RevalidateTxsResult is removed, done) =
                runRevalidate stepSize newTipSt slot tickets
           in conjoin
                [ counterexample "exactly the on-chain txs are removed" $
                    sort (map (txId . getInvalidated) removed) === sort (map txId onChainTxs)
                , counterexample "all off-chain txs survive, in order" $
                    ticketIds (TxSeq.toList (isTxs is)) === map txId offChainTxs
                , counterexample "the loop must stop because it ran out of txs" $
                    done === EndReached
                ]

-- | On the cached paths -- same tip and slot, and same tip at a later slot --
-- 'getSnapshotFor' must return the full mempool without a single LedgerDB
-- read.
prop_getSnapshotForCachedPaths :: Property
prop_getSnapshotForCachedPaths =
  forAll genBaseState $ \baseSt ->
    forAllBlind (genLatticeOn (1, 5) baseSt) $ \txs ->
      runSimOrThrow $
        withMempool baseSt txs $ \mempool getEvents -> do
          varReads <- uncheckedNewTVarM (0 :: Int)
          let snapSlot = tipSuccSlot baseSt
              countingReads keys = do
                atomically $ modifyTVar varReads (+ 1)
                resolverFromTables (projectLedgerTables baseSt) keys
          snapSame <- getSnapshotFor mempool snapSlot (tickTo snapSlot baseSt) countingReads
          snapNext <-
            getSnapshotFor mempool (succ snapSlot) (tickTo (succ snapSlot) baseSt) countingReads
          outcomes <- snapshotOutcomes <$> getEvents
          nReads <- atomically $ readTVar varReads
          pure $
            conjoin
              [ counterexample "same tip and slot: full snapshot" $
                  snapshotTxIds snapSame === map txId txs
              , counterexample "same tip, later slot: full snapshot" $
                  snapshotTxIds snapNext === map txId txs
              , counterexample "the cached paths must not read the LedgerDB" $
                  nReads === 0
              , counterexample "first snapshot cached, second computed to completion" $
                  case outcomes of
                    [Nothing, Just (_, EndReached)] -> property True
                    other -> counterexample (show other) False
              ]

-- | At a new tip whose block contains a prefix of the mempool's transactions,
-- 'getSnapshotFor' must drop exactly the included transactions and keep the
-- rest, in order (see 'prop_txChainSurvivesNewTip' for the seed-only scenario
-- against the bare loop).
--
-- The lattice is flattened in dependency order — every parent precedes all of
-- its descendants — so any prefix of it is closed under dependencies and
-- hence forms a valid block.
--
-- The lattice is at least @'snapshotTxsPerStep' + 1@ transactions deep, so the
-- reapplication always spans multiple steps, with surviving transactions
-- depending on on-chain and in-mempool outputs across step boundaries.
prop_getSnapshotForNewTip :: Property
prop_getSnapshotForNewTip =
  forAll genBaseState $ \baseSt ->
    forAllBlind (genLatticeOn (snapshotTxsPerStep, 2 * snapshotTxsPerStep) baseSt) $ \txs ->
      forAll (choose (1, length txs)) $ \nOnChain ->
        runSimOrThrow $
          withMempool baseSt txs $ \mempool getEvents -> do
            -- The new tip: a block containing the first 'nOnChain' of the
            -- mempool's transactions. Built with 'applyTxToLedger' (not
            -- 'applyTxToTickedLedger') so the tip point changes and
            -- 'getSnapshotFor' takes the different-tip path.
            let (onChainTxs, offChainTxs) = splitAt nOnChain txs
                newTipSt =
                  foldl
                    (\st tx -> mustBeValid (applyTxToLedger testCfg st tx))
                    baseSt
                    onChainTxs
                snapSlot = tipSuccSlot newTipSt
            snap <-
              getSnapshotFor
                mempool
                snapSlot
                (tickTo snapSlot newTipSt)
                (resolverFromTables (projectLedgerTables newTipSt))
            outcomes <- snapshotOutcomes <$> getEvents
            pure $ case outcomes of
              [Just (nSteps, EndReached)] ->
                tabulate "getSnapshotFor steps" [show nSteps] $
                  counterexample "the on-chain prefix is dropped, the rest survive in order" $
                    snapshotTxIds snap === map txId offChainTxs
              other ->
                counterexample
                  ("expected exactly one snapshot computed to completion, got: " <> show other)
                  False

-- | With a LedgerDB read that takes 0.06s (simulated) per step and the 0.1s
-- default time limit, 'getSnapshotFor' deterministically performs exactly two
-- steps and returns the corresponding prefix of the mempool.
prop_getSnapshotForTimeout :: Property
prop_getSnapshotForTimeout =
  forAll genBaseState $ \baseSt ->
    forAllBlind (genLongTxChain baseSt) $ \txs ->
      runSimOrThrow $
        withMempool baseSt txs $ \mempool getEvents -> do
          let newTipSt = bumpTip baseSt
              snapSlot = tipSuccSlot newTipSt
              slowReads keys = do
                threadDelay 0.06
                resolverFromTables (projectLedgerTables newTipSt) keys
          snap <- getSnapshotFor mempool snapSlot (tickTo snapSlot newTipSt) slowReads
          outcomes <- snapshotOutcomes <$> getEvents
          pure $
            conjoin
              [ counterexample "the snapshot is the prefix reached before the time limit" $
                  snapshotTxIds snap === map txId (take (2 * snapshotTxsPerStep) txs)
              , counterexample "the trace records two steps and TimeoutReached" $
                  case outcomes of
                    [Just (2, TimeoutReached)] -> property True
                    other -> counterexample (show other) False
              ]

-------------------------------------------------------------------------------
-- Infrastructure
-------------------------------------------------------------------------------

testCfg :: LedgerConfig TestBlock
testCfg = testLedgerConfigNoSizeLimits

-- | A budget large enough to never fire in these tests (IOSim time only
-- advances on explicit delays, so the loop is effectively unbounded).
unlimitedBudget :: DiffTime
unlimitedBudget = 1_000_000

-- | A connected lattice of dependent transactions (seed first), valid in
-- order against the given ticked state, with the depth drawn from the given
-- range. Every layer holds between 1 and 5 transactions, so the lattice has
-- between @depth + 1@ and @5 * depth + 1@ transactions — pick the lower bound
-- of the range to guarantee a transaction count.
genLatticeTxs ::
  (Int, Int) ->
  SlotNo ->
  TickedLedgerState TestBlock ValuesMK ->
  Gen [TestTx]
genLatticeTxs depthRange slot tickedSt = do
  depth <- choose depthRange
  (txSeed, innerTxs) <- genConnectedTxLattice slot depth tickedSt
  pure (txSeed : innerTxs)

-- | Apply one valid tx against a ticked-values state; return a ticket and the
-- next state.
buildTicket ::
  SlotNo ->
  TestTx ->
  TxSeq.TicketNo ->
  TickedLedgerState TestBlock ValuesMK ->
  ( TxTicket (MempoolMeasure TestBlock) (ValidatedTxWithDiffs TestBlock)
  , TickedLedgerState TestBlock ValuesMK
  )
buildTicket slot tx ticketNo st =
  case runExcept (applyTx testCfg DoNotIntervene slot tx st) of
    Left err -> error $ "buildTicket: " ++ show err
    Right (stDiff, vtx) ->
      ( TxTicket
          { txTicketTx = ValidatedTxWithDiffs vtx (projectLedgerTables stDiff)
          , txTicketNo = ticketNo
          , txTicketSize = Measure.zero
          }
      , applyDiffs st stDiff
      )

-- | Build tickets for a list of txs, threading the ticked state and assigning
-- ticket numbers starting at 1.
buildTicketList ::
  SlotNo ->
  [TestTx] ->
  TickedLedgerState TestBlock ValuesMK ->
  [TxTicket (MempoolMeasure TestBlock) (ValidatedTxWithDiffs TestBlock)]
buildTicketList slot txs st0 =
  fst $
    foldl
      ( \(acc, st) (n, tx) ->
          let (tkt, st') = buildTicket slot tx (TxSeq.TicketNo n) st
           in (acc ++ [tkt], st')
      )
      ([], st0)
      (zip [1 :: Word64 ..] txs)

-- | Resolve keys against the given UTxO table, like a LedgerDB read would.
resolverFromTables ::
  Monad m =>
  LedgerTables TestBlock ValuesMK ->
  LedgerTables TestBlock KeysMK ->
  m (LedgerTables TestBlock ValuesMK)
resolverFromTables (LedgerTables (ValuesMK utxos)) (LedgerTables (KeysMK keys)) =
  pure $ LedgerTables (ValuesMK (Map.restrictKeys utxos keys))

-- | Resolve keys against the UTxO set of the given ledger state, like a
-- LedgerDB read at that state would.
resolverFromState ::
  Monad m =>
  TickedLedgerState TestBlock ValuesMK ->
  LedgerTables TestBlock KeysMK ->
  m (LedgerTables TestBlock ValuesMK)
resolverFromState = resolverFromTables . projectLedgerTables

ticketIds ::
  [TxTicket (MempoolMeasure TestBlock) (ValidatedTxWithDiffs TestBlock)] ->
  [TxId TestTx]
ticketIds = map (txId . txForgetValidated . validatedTx . txTicketTx)

-- | A mempool environment carrying only what 'revalidateUntilTimeout' reads:
-- the ledger config and the capacity override. Every other field is an error
-- stub.
testMempoolEnv :: Monad m => MempoolEnv m TestBlock
testMempoolEnv =
  MempoolEnv
    { mpEnvLedger = error "testMempoolEnv: mpEnvLedger is not used"
    , mpEnvForker = error "testMempoolEnv: mpEnvForker is not used"
    , mpEnvLedgerCfg = testCfg
    , mpEnvStateVar = error "testMempoolEnv: mpEnvStateVar is not used"
    , mpEnvAddTxsRemoteFifo = error "testMempoolEnv: mpEnvAddTxsRemoteFifo is not used"
    , mpEnvAddTxsAllFifo = error "testMempoolEnv: mpEnvAddTxsAllFifo is not used"
    , mpEnvTracer = nullTracer
    , mpEnvCapacityOverride = NoMempoolCapacityBytesOverride
    , mpEnvTimeoutConfig = Nothing
    }

-- | Run 'revalidateUntilTimeout' in IOSim with an effectively unlimited
-- budget.
runRevalidate ::
  Int ->
  TickedLedgerState TestBlock ValuesMK ->
  SlotNo ->
  [TxTicket (MempoolMeasure TestBlock) (ValidatedTxWithDiffs TestBlock)] ->
  (RevalidateTxsResult TestBlock, DoneType)
runRevalidate stepSize baseSt slot tickets =
  runSimOrThrow $ do
    (result, _remaining, done) <-
      revalidateUntilTimeout
        testMempoolEnv
        unlimitedBudget
        stepSize
        (resolverFromState baseSt)
        seedCandidate
        tickets
    pure (result, done)
 where
  seedCandidate =
    RevalidateTxsResult
      ( initInternalState
          NoMempoolCapacityBytesOverride
          (TxSeq.TicketNo (fromIntegral (length tickets)))
          testCfg
          slot
          (noNewTickingDiffs baseSt)
      )
      []

-- | Base chain state for the integration properties: genesis evolved by a few
-- valid transactions, so the tip is not at 'Origin'.
genBaseState :: Gen (LedgerState TestBlock ValuesMK)
genBaseState = do
  n <- choose (1, 5)
  (_txs, st) <- genValidTxs n testInitLedger
  pure st

-- | The slot the mempool ticks the given state to ('ForgeInUnknownSlot'): the
-- successor of the tip slot.
tipSuccSlot :: LedgerState TestBlock ValuesMK -> SlotNo
tipSuccSlot st = case ledgerTipSlot st of
  Origin -> error "tipSuccSlot: the integration tests require a non-genesis tip"
  At s -> succ s

-- | Tick the given state to the given slot, keeping its tip.
tickTo :: SlotNo -> LedgerState TestBlock ValuesMK -> TickedLedgerState TestBlock DiffMK
tickTo slot st = applyChainTick OmitLedgerEvents testCfg slot (forgetLedgerTables st)

snapshotTxIds :: MempoolSnapshot TestBlock -> [TxId TestTx]
snapshotTxIds snap = [txId (txForgetValidated vtx) | (vtx, _, _) <- snapshotTxs snap]

-- | The @Maybe (StepCount, DoneType)@ of each 'TraceGetSnapshotFor' event, in
-- emission order.
snapshotOutcomes :: [TraceEventMempool TestBlock] -> [Maybe (Int, DoneType)]
snapshotOutcomes events = [mb | TraceGetSnapshotFor _ _ _ _ mb <- events]

-- | Open a real mempool over a mock LedgerDB fixed at the given base state,
-- add the given transactions (all of which must be accepted), and run the
-- continuation with the mempool and a reader for the traced events.
withMempool ::
  forall m.
  (IOLike m, MonadTimer m) =>
  LedgerState TestBlock ValuesMK ->
  [TestTx] ->
  (Mempool m TestBlock -> m [TraceEventMempool TestBlock] -> m Property) ->
  m Property
withMempool baseSt txs k = do
  (tracer, getEvents) <- recordingTracerTVar
  mempool <-
    openMempoolWithoutSyncThread
      ledgerInterface
      testCfg
      (MempoolCapacityBytesOverride (ByteSize32 100_000_000))
      Nothing
      tracer
  results <- addTxs mempool txs
  unless (all isMempoolTxAdded results) $
    error "withMempool: every generated transaction must be accepted"
  k mempool getEvents
 where
  ledgerInterface =
    LedgerInterface
      { getCurrentLedgerState =
          pure $
            MempoolLedgerDBView
              (forgetLedgerTables baseSt)
              ( pure $
                  Right $
                    ReadOnlyForker
                      { roforkerClose = pure ()
                      , roforkerReadTables = resolverFromTables (projectLedgerTables baseSt)
                      , roforkerRangeReadTables = const $ pure (emptyLedgerTables, Nothing)
                      , roforkerGetLedgerState = pure $ forgetLedgerTables baseSt
                      , roforkerReadStatistics = pure $ Statistics 0
                      }
              )
      }

-- | Like 'genLatticeTxs', but against an unticked base state (as the
-- integration properties hold it).
genLatticeOn :: (Int, Int) -> LedgerState TestBlock ValuesMK -> Gen [TestTx]
genLatticeOn depthRange baseSt =
  genLatticeTxs depthRange slot (applyDiffs baseSt (tickTo slot baseSt))
 where
  slot = tipSuccSlot baseSt

-- | A sequentially valid tx chain long enough to span more than two
-- reapplication steps.
genLongTxChain :: LedgerState TestBlock ValuesMK -> Gen [TestTx]
genLongTxChain baseSt = fst <$> genValidTxs (2 * snapshotTxsPerStep + 50) baseSt
