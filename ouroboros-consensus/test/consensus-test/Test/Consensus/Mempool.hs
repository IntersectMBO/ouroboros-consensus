{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Property tests for the mempool.
--
-- The mempool collects transactions from downstream nodes, makes them
-- available to upstream nodes, and of course provides the pool of transactions
-- that we use when forging blocks.
--
-- These tests for the mempool are not model based, but instead check various
-- simple properties and invariants, for instance:
--
-- * After adding valid transactions to the mempool, they can be retrieved.
-- * Adding invalid transactions from the mempool will report them as invalid,
--   and they are not added.
-- * Transactions cannot be retrieved after they are removed.
-- * The mempool capacity is not exceeded
--
-- NOTE: the test mempool's default capacity is set to a very large value in
-- module "Ouroboros.Consensus.Mock.Ledger.Block". This is why the generators do
-- not care about the mempool capacity when generating transactions for a
-- mempool with the 'NoMempoolCapacityBytesOverride' option set.
module Test.Consensus.Mempool (tests) where

import Cardano.Binary (toCBOR)
import Cardano.Crypto.Hash
import Control.Monad (foldM, forM, forM_, void)
import Control.Monad.Except (runExcept)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Monad.State (State, evalState, get, modify)
import Control.ResourceRegistry
import Control.Tracer (Tracer (..))
import Data.Bifunctor (first, second)
import Data.Either (isRight)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (stimes)
import qualified Data.Set as Set
import Data.Word
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.Mempool.Impl.Common (MempoolLedgerDBView (..))
import Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Mock.Ledger hiding (TxId)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util (repeatedly, repeatedlyM)
import Ouroboros.Consensus.Util.Condense (condense)
import Ouroboros.Consensus.Util.IOLike
import Test.Consensus.Mempool.Util
import Test.Crypto.Hash ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "Mempool"
    [ testGroup
        "TxSeq"
        [ testProperty "lookupByTicketNo complete" prop_TxSeq_lookupByTicketNo_complete
        , testProperty "lookupByTicketNo sound" prop_TxSeq_lookupByTicketNo_sound
        , testProperty "splitAfterTxSize" prop_TxSeq_splitAfterTxSize
        , testProperty "splitAfterTxSizeSpec" prop_TxSeq_splitAfterTxSizeSpec
        ]
    , testGroup
        "IOSim properties"
        [ testProperty
            "snapshotTxs == snapshotTxsAfter zeroTicketNo"
            prop_Mempool_snapshotTxs_snapshotTxsAfter
        , testProperty "valid added txs == getTxs" prop_Mempool_addTxs_getTxs
        , testProperty "addTxs [..] == forM [..] addTxs" prop_Mempool_semigroup_addTxs
        , testProperty "result of addTxs" prop_Mempool_addTxs_result
        , testProperty "Invalid transactions are never added" prop_Mempool_InvalidTxsNeverAdded
        , testProperty "removeTxs" prop_Mempool_removeTxs
        , testProperty "removeTxs [..] == forM [..] removeTxs" prop_Mempool_semigroup_removeTxs
        , testProperty "result of getCapacity" prop_Mempool_getCapacity
        , -- FIXME: we should add an issue to test this aspect somehow.
          -- , testProperty "Mempool capacity implementation"              prop_Mempool_Capacity
          testProperty "Added valid transactions are traced" prop_Mempool_TraceValidTxs
        , testProperty "Rejected invalid txs are traced" prop_Mempool_TraceRejectedTxs
        , testProperty "Removed invalid txs are traced" prop_Mempool_TraceRemovedTxs
        , testProperty "idx consistency" prop_Mempool_idx_consistency
        ]
    ]

{-------------------------------------------------------------------------------
  Mempool Implementation Properties
-------------------------------------------------------------------------------}

-- | Test that @snapshotTxs == snapshotTxsAfter zeroTicketNo@.
prop_Mempool_snapshotTxs_snapshotTxsAfter :: TestSetup -> Property
prop_Mempool_snapshotTxs_snapshotTxsAfter setup =
  withTestMempool setup $ \TestMempool{mempool} -> do
    let Mempool{getSnapshot} = mempool
    MempoolSnapshot{snapshotTxs, snapshotTxsAfter} <- atomically getSnapshot
    return $ snapshotTxs === snapshotTxsAfter zeroTicketNo

-- | Test that all valid transactions added to a 'Mempool' can be retrieved
-- afterward.
prop_Mempool_addTxs_getTxs :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_getTxs setup =
  withTestMempool (testSetup setup) $ \TestMempool{mempool} -> do
    _ <- addTxs mempool (allTxs setup)
    MempoolSnapshot{snapshotTxs} <- atomically $ getSnapshot mempool
    return $
      counterexample (ppTxs (txs setup)) $
        validTxs setup `List.isSuffixOf` map (txForgetValidated . prjTx) snapshotTxs

-- | Test that both adding the transactions one by one and adding them in one go
-- produce the same result.
prop_Mempool_semigroup_addTxs :: TestSetupWithTxs -> Property
prop_Mempool_semigroup_addTxs setup =
  withTestMempool (testSetup setup) $ \TestMempool{mempool = mempool1} -> do
    _ <- addTxs mempool1 (allTxs setup)
    snapshot1 <- atomically $ getSnapshot mempool1

    return $ withTestMempool (testSetup setup) $ \TestMempool{mempool = mempool2} -> do
      forM_ (allTxs setup) $ \tx -> addTxs mempool2 [tx]
      snapshot2 <- atomically $ getSnapshot mempool2

      return
        $ counterexample
          ( "Transactions after adding in one go: "
              <> show (snapshotTxs snapshot1)
              <> "\nTransactions after adding one by one: "
              <> show (snapshotTxs snapshot2)
          )
        $ snapshotTxs snapshot1 === snapshotTxs snapshot2
          .&&. snapshotMempoolSize snapshot1 === snapshotMempoolSize snapshot2
          .&&. snapshotSlotNo snapshot1 === snapshotSlotNo snapshot1

-- | Test that the result of adding transaction to a 'Mempool' matches our
-- expectation: invalid transactions have errors associated with them and
-- valid transactions don't.
prop_Mempool_addTxs_result :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_result setup =
  withTestMempool (testSetup setup) $ \TestMempool{mempool} -> do
    result <- addTxs mempool (allTxs setup)
    return $
      counterexample (ppTxs (txs setup)) $
        [ case res of
            MempoolTxAdded vtx -> (txForgetValidated vtx, True)
            MempoolTxRejected tx _err -> (tx, False)
        | res <- result
        ]
          === txs setup

-- | Test that invalid transactions are never added to the 'Mempool'.
prop_Mempool_InvalidTxsNeverAdded :: TestSetupWithTxs -> Property
prop_Mempool_InvalidTxsNeverAdded setup =
  withTestMempool (testSetup setup) $ \TestMempool{mempool} -> do
    txsInMempoolBefore <-
      map prjTx . snapshotTxs
        <$> atomically (getSnapshot mempool)
    _ <- addTxs mempool (allTxs setup)
    txsInMempoolAfter <-
      map prjTx . snapshotTxs
        <$> atomically (getSnapshot mempool)
    return $
      counterexample (ppTxs (txs setup)) $
        conjoin
          -- Check for each transaction in the mempool (ignoring those already
          -- in the mempool beforehand) that it was a valid transaction.
          --
          -- Note that we can't check that no invalid transactions are in the
          -- mempool because the same transaction could be added twice: the
          -- first time as a valid one and the second time as an invalid one.
          [ (txForgetValidated txInMempool `elem` validTxs setup) === True
          | txInMempool <- txsInMempoolAfter
          , txInMempool `notElem` txsInMempoolBefore
          ]

-- | After removing a transaction from the Mempool, it's actually gone.
prop_Mempool_removeTxs :: TestSetupWithTxInMempool -> Property
prop_Mempool_removeTxs (TestSetupWithTxInMempool testSetup txToRemove) =
  withTestMempool testSetup $ \TestMempool{mempool} -> do
    let Mempool{removeTxsEvenIfValid, getSnapshot} = mempool
    removeTxsEvenIfValid $ NE.fromList [txId txToRemove]
    txsInMempoolAfter <- map prjTx . snapshotTxs <$> atomically getSnapshot
    return $
      counterexample
        ( "Transactions in the mempool after removing ("
            <> show txToRemove
            <> "): "
            <> show txsInMempoolAfter
        )
        (txToRemove `notElem` map txForgetValidated txsInMempoolAfter)

-- | Test that both removing transactions one by one and removing them in one go
-- produce the same result.
prop_Mempool_semigroup_removeTxs :: TestSetupWithTxsInMempoolToRemove -> Property
prop_Mempool_semigroup_removeTxs (TestSetupWithTxsInMempoolToRemove testSetup txsToRemove) =
  withTestMempool testSetup $ \TestMempool{mempool = mempool1} -> do
    removeTxsEvenIfValid mempool1 $ NE.map txId txsToRemove
    snapshot1 <- atomically (getSnapshot mempool1)

    return $ withTestMempool testSetup $ \TestMempool{mempool = mempool2} -> do
      forM_ (NE.map txId txsToRemove) (removeTxsEvenIfValid mempool2 . (NE.:| []))
      snapshot2 <- atomically (getSnapshot mempool2)

      return
        $ counterexample
          ( "Transactions after removing in one go: "
              <> show (snapshotTxs snapshot1)
              <> "\nTransactions after removing one by one: "
              <> show (snapshotTxs snapshot2)
          )
        $ snapshotTxs snapshot1 === snapshotTxs snapshot2
          .&&. snapshotMempoolSize snapshot1 === snapshotMempoolSize snapshot2
          .&&. snapshotSlotNo snapshot1 === snapshotSlotNo snapshot1

-- | Test that 'getCapacity' returns the greatest multiple of the block
-- capacity that is not greater than the requested capacity.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'.
prop_Mempool_getCapacity :: MempoolCapTestSetup -> Property
prop_Mempool_getCapacity mcts =
  withTestMempool testSetup $ \TestMempool{mempool} -> do
    IgnoringOverflow actualCapacity <- atomically $ getCapacity mempool
    pure $ actualCapacity === expectedCapacity
 where
  MempoolCapacityBytesOverride testCapacity = testMempoolCapOverride testSetup
  MempoolCapTestSetup (TestSetupWithTxs testSetup _txsToAdd) = mcts

  ByteSize32 dnom = simpleBlockCapacity

  expectedCapacity =
    (\n -> stimes n simpleBlockCapacity) $
      max 1
      -- adding one less than the denom to the numer achieves rounding up
      $
        (unByteSize32 testCapacity + dnom - 1) `div` dnom

-- | Test that all valid transactions added to a 'Mempool' via 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceValidTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceValidTxs setup =
  withTestMempool (testSetup setup) $ \testMempool -> do
    let TestMempool{mempool, getTraceEvents} = testMempool
    _ <- addTxs mempool (allTxs setup)
    evs <- getTraceEvents
    return $
      counterexample (ppTxs (txs setup)) $
        let addedTxs = mapMaybe isAddedTxsEvent evs
         in validTxs setup === addedTxs
 where
  isAddedTxsEvent :: TraceEventMempool TestBlock -> Maybe (GenTx TestBlock)
  isAddedTxsEvent (TraceMempoolAddedTx tx _ _) = Just (txForgetValidated tx)
  isAddedTxsEvent _ = Nothing

-- | Test that all invalid rejected transactions returned from 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceRejectedTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceRejectedTxs setup =
  withTestMempool (testSetup setup) $ \testMempool -> do
    let TestMempool{mempool, getTraceEvents} = testMempool
    _ <- addTxs mempool (allTxs setup)
    evs <- getTraceEvents
    return $
      counterexample (ppTxs (txs setup)) $
        let rejectedTxs = mapMaybe isRejectedTxEvent evs
         in invalidTxs setup === rejectedTxs
 where
  isRejectedTxEvent :: TraceEventMempool blk -> Maybe (GenTx blk)
  isRejectedTxEvent (TraceMempoolRejectedTx tx _ _) = Just tx
  isRejectedTxEvent _ = Nothing

-- | Test that all transactions in the 'Mempool' that have become invalid
-- because of an update to the ledger are appropriately represented in the
-- trace of events.
prop_Mempool_TraceRemovedTxs :: TestSetup -> Property
prop_Mempool_TraceRemovedTxs setup =
  withTestMempool setup $ \testMempool -> do
    let TestMempool{mempool, getTraceEvents, addTxsToLedger, getCurrentLedger} = testMempool
    MempoolSnapshot{snapshotTxs} <- atomically $ getSnapshot mempool
    -- We add all the transactions in the mempool to the ledger. Some of
    -- them will become invalid because all inputs have been spent.
    let txsInMempool = map prjTx snapshotTxs
    errs <- atomically $ addTxsToLedger (map txForgetValidated txsInMempool)

    -- Sync the mempool with the ledger. Now some of the transactions in the
    -- mempool should have been removed.
    void $ testSyncWithLedger mempool

    -- Predict which transactions should have been removed
    curLedger <- atomically getCurrentLedger
    let expected = expectedToBeRemoved curLedger (map txForgetValidated txsInMempool)

    -- Look at the trace to see which transactions actually got removed
    evs <- getTraceEvents
    let removedTxs = concat $ mapMaybe isRemoveTxsEvent evs

    -- Also check that 'addTxsToLedger' never resulted in an error.
    return $
      classify (not (null removedTxs)) "Removed some transactions" $
        map (const (Right ())) errs === errs
          .&&. List.sortOn fst expected === List.sortOn fst removedTxs
 where
  cfg = testLedgerCfg setup

  isRemoveTxsEvent :: TraceEventMempool TestBlock -> Maybe [(TestTx, TestTxError)]
  isRemoveTxsEvent (TraceMempoolRemoveTxs txs _) = Just (map (first txForgetValidated) txs)
  isRemoveTxsEvent _ = Nothing

  expectedToBeRemoved :: LedgerState TestBlock ValuesMK -> [TestTx] -> [(TestTx, TestTxError)]
  expectedToBeRemoved ledgerState txsInMempool =
    [ (tx, err)
    | (tx, Left err) <- fst $ validateTxs cfg ledgerState txsInMempool
    ]

prjTx ::
  (Validated (GenTx TestBlock), TicketNo, TxMeasure TestBlock) ->
  Validated (GenTx TestBlock)
prjTx (a, _b, _c) = a

{-------------------------------------------------------------------------------
  TestSetup: how to set up a TestMempool
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { testLedgerCfg :: LedgerConfig TestBlock
  , testLedgerState :: LedgerState TestBlock ValuesMK
  , testInitialTxs :: [TestTx]
  -- ^ These are all valid and will be the initial contents of the Mempool.
  , testMempoolCapOverride :: MempoolCapacityBytesOverride
  }
  deriving Show

ppTestSetup :: TestSetup -> String
ppTestSetup
  TestSetup
    { testInitialTxs
    , testMempoolCapOverride
    } =
    unlines $
      ["Initial contents of the Mempool:"]
        <> (map ppTestTxWithHash testInitialTxs)
        <> ["Total size:"]
        <> [show $ foldMap genTxSize $ testInitialTxs]
        <> ["Mempool capacity override:"]
        <> [show testMempoolCapOverride]

ppTestTxWithHash :: TestTx -> String
ppTestTxWithHash x =
  condense
    (hashWithSerialiser toCBOR (simpleGenTx x) :: Hash SHA256 Tx, x)

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions.
--
-- The generated 'testMempoolCap' will be:
-- > foldMap 'genTxSize' 'testInitialTxs' + extraCapacity
genTestSetupWithExtraCapacity ::
  Int -> ByteSize32 -> Gen (TestSetup, LedgerState TestBlock ValuesMK)
genTestSetupWithExtraCapacity maxInitialTxs extraCapacity = do
  ledgerSize <- choose (0, maxInitialTxs)
  nbInitialTxs <- choose (0, maxInitialTxs)
  (_txs1, ledger1) <- genValidTxs ledgerSize testInitLedger
  (txs2, ledger2) <- genValidTxs nbInitialTxs ledger1
  let initTxsSizeInBytes = foldMap genTxSize txs2
      mpCap = initTxsSizeInBytes <> extraCapacity
      testSetup =
        TestSetup
          { testLedgerCfg = testLedgerConfigNoSizeLimits
          , testLedgerState = ledger1
          , testInitialTxs = txs2
          , testMempoolCapOverride = MempoolCapacityBytesOverride mpCap
          }
  return (testSetup, ledger2)

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions. Generates setups with a fixed
-- 'MempoolCapacityBytesOverride', no 'NoMempoolCapacityBytesOverride'.
genTestSetup :: Int -> Gen (TestSetup, LedgerState TestBlock ValuesMK)
genTestSetup maxInitialTxs =
  genTestSetupWithExtraCapacity maxInitialTxs (ByteSize32 0)

-- | Random 'MempoolCapacityBytesOverride'
instance Arbitrary TestSetup where
  arbitrary = sized $ \n -> do
    extraCapacity <- (ByteSize32 . fromIntegral) <$> choose (0, n)
    testSetup <- fst <$> genTestSetupWithExtraCapacity n extraCapacity
    noOverride <- arbitrary
    let initialSize = foldMap genTxSize $ testInitialTxs testSetup
        defaultCap = simpleBlockCapacity <> simpleBlockCapacity
    return $
      if noOverride && initialSize <= defaultCap
        then testSetup{testMempoolCapOverride = NoMempoolCapacityBytesOverride}
        else testSetup

  shrink
    TestSetup
      { testLedgerCfg
      , testLedgerState
      , testInitialTxs
      , testMempoolCapOverride =
        MempoolCapacityBytesOverride (ByteSize32 mpCap)
      } =
      -- TODO we could shrink @testLedgerState@ too
      [ TestSetup
          { testLedgerCfg
          , testLedgerState
          , testInitialTxs = testInitialTxs'
          , testMempoolCapOverride =
              MempoolCapacityBytesOverride mpCap'
          }
      | let ByteSize32 initial = foldMap genTxSize testInitialTxs
            extraCap = mpCap - initial
      , testInitialTxs' <- shrinkList (const []) testInitialTxs
      , isRight $ txsAreValid testLedgerCfg testLedgerState testInitialTxs'
      , let mpCap' = foldMap genTxSize testInitialTxs' <> ByteSize32 extraCap
      ]
  -- TODO shrink to an override, that's an easier test case
  shrink
    TestSetup
      { testLedgerCfg
      , testLedgerState
      , testInitialTxs
      , testMempoolCapOverride = NoMempoolCapacityBytesOverride
      } =
      -- TODO we could shrink @testLedgerState@ too
      [ TestSetup
          { testLedgerCfg
          , testLedgerState
          , testInitialTxs = testInitialTxs'
          , testMempoolCapOverride = NoMempoolCapacityBytesOverride
          }
      | testInitialTxs' <- shrinkList (const []) testInitialTxs
      , isRight $ txsAreValid testLedgerCfg testLedgerState testInitialTxs'
      ]

txsAreValid ::
  LedgerConfig TestBlock ->
  LedgerState TestBlock ValuesMK ->
  [TestTx] ->
  Either TestTxError (LedgerState TestBlock ValuesMK)
txsAreValid cfg ledgerState txs =
  runExcept $ repeatedlyM (flip (applyTxToLedger cfg)) txs ledgerState

validateTxs ::
  LedgerConfig TestBlock ->
  LedgerState TestBlock ValuesMK ->
  [TestTx] ->
  ([(TestTx, Either TestTxError ())], LedgerState TestBlock ValuesMK)
validateTxs cfg = go []
 where
  go revalidated ledgerState = \case
    [] -> (reverse revalidated, ledgerState)
    tx : txs' -> case runExcept (applyTxToLedger cfg ledgerState tx) of
      Left err -> go ((tx, Left err) : revalidated) ledgerState txs'
      Right ledgerState' -> go ((tx, Right ()) : revalidated) ledgerState' txs'

{-------------------------------------------------------------------------------
  TestSetupWithTxs
-------------------------------------------------------------------------------}

data TestSetupWithTxs = TestSetupWithTxs
  { testSetup :: TestSetup
  , txs :: [(TestTx, Bool)]
  -- ^ The 'Bool' indicates whether the transaction is valid
  }
  deriving Show

ppTxs :: [(TestTx, Bool)] -> String
ppTxs txs =
  unlines $
    ["Transactions:"]
      <> [ condense tx <> ": " <> if valid then "VALID" else "INVALID"
         | (tx, valid) <- txs
         ]

allTxs :: TestSetupWithTxs -> [GenTx TestBlock]
allTxs = map fst . txs

validTxs :: TestSetupWithTxs -> [GenTx TestBlock]
validTxs = map fst . filter snd . txs

invalidTxs :: TestSetupWithTxs -> [GenTx TestBlock]
invalidTxs = map fst . filter (not . snd) . txs

{-
Note [Transaction size limit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An important property of the mempool is that adding a transaction that can never
fit into the mempool must not block, also see
https://github.com/IntersectMBO/ouroboros-consensus/issues/1226. We test this
while generating a TestSetupWithTxs by always including a transaction that is
larger than the entire mempool, and setting the per-tx size limit such that just
this transaction is invalid due to its size, but not impacting the validity of
any other transactions. Therefore, we disable the size limit in e.g.
'genValidTx' to only capture UTxO-related validity for them by using an
appropriate ledger config ('testLedgerConfigNoSizeLimits').
-}

instance Arbitrary TestSetupWithTxs where
  arbitrary = sized $ \n -> do
    nbTxs <- choose (0, n)
    (testSetup, ledger) <- genTestSetup n
    (txs, _ledger') <- genTxs nbTxs ledger
    testSetup' <- case testMempoolCapOverride testSetup of
      NoMempoolCapacityBytesOverride -> return testSetup
      MempoolCapacityBytesOverride mpCap -> do
        noOverride <- arbitrary
        let initialSize = foldMap genTxSize $ testInitialTxs testSetup
            defaultCap = simpleBlockCapacity <> simpleBlockCapacity
            newSize =
              foldMap (genTxSize . fst) (filter snd txs)
                <> maximum (ByteSize32 0 : map (genTxSize . fst) (filter (not . snd) txs))
        return
          testSetup
            { testMempoolCapOverride =
                if noOverride && initialSize <> newSize <= defaultCap
                  then NoMempoolCapacityBytesOverride
                  else MempoolCapacityBytesOverride $ mpCap <> newSize
            }
    let mempoolCap :: TheMeasure
        mempoolCap =
          computeMempoolCapacity
            testLedgerConfigNoSizeLimits
            (TickedSimpleLedgerState ledger)
            (testMempoolCapOverride testSetup)

    largeInvalidTx <- genLargeInvalidTx mempoolCap
    let txs' = (largeInvalidTx, False) : txs
        -- Set the maximum tx size to the mempool capacity. This won't
        -- invalidate any valid tx in @txs@ as the capacity was chosen such that
        -- all @txs@ fit into the mempool. Also see Note [Transaction size
        -- limit].
        testSetup'' =
          testSetup'
            { testLedgerCfg =
                (testLedgerCfg testSetup')
                  { simpleLedgerMockConfig =
                      MockConfig
                        { mockCfgMaxTxSize = Just (unIgnoringOverflow mempoolCap)
                        }
                  }
            }

    return TestSetupWithTxs{testSetup = testSetup'', txs = txs'}

  shrink TestSetupWithTxs{testSetup, txs} =
    [ TestSetupWithTxs{testSetup = testSetup', txs}
    | testSetup' <- shrink testSetup
    ]
      <> [ TestSetupWithTxs{testSetup, txs = txs'}
         | txs' <-
             map (map (second isRight) . fst . revalidate testSetup)
               . shrinkList (const [])
               . map fst
               $ txs
         ]

revalidate ::
  TestSetup ->
  [TestTx] ->
  ([(TestTx, Either TestTxError ())], LedgerState TestBlock ValuesMK)
revalidate TestSetup{testLedgerCfg, testLedgerState, testInitialTxs} =
  validateTxs testLedgerCfg initLedgerState
 where
  -- The LedgerState after adding the transactions initially in the mempool
  initLedgerState =
    repeatedly
      (\tx l -> mustBeValid (applyTxToLedger testLedgerCfg l tx))
      testInitialTxs
      testLedgerState

{-------------------------------------------------------------------------------
  TestSetupWithTxInMempol: a mempool and a transaction that is in the mempool
-------------------------------------------------------------------------------}

-- | A 'TestSetup' along with a transaction that is in the Mempool.
--
-- > 'txInMempool' `elem` 'testInitialTxs' 'testSetup'
data TestSetupWithTxInMempool = TestSetupWithTxInMempool TestSetup TestTx
  deriving Show

instance Arbitrary TestSetupWithTxInMempool where
  arbitrary = do
    TestSetupWithTxs{testSetup} <-
      arbitrary `suchThat` (not . null . testInitialTxs . testSetup)
    tx <- elements (testInitialTxs testSetup)
    return $ TestSetupWithTxInMempool testSetup tx

  shrink (TestSetupWithTxInMempool testSetup _tx) =
    [ TestSetupWithTxInMempool testSetup' tx'
    | testSetup' <- shrink testSetup
    , not . null . testInitialTxs $ testSetup'
    , tx' <- testInitialTxs testSetup'
    ]

data TestSetupWithTxsInMempool = TestSetupWithTxsInMempool TestSetup [TestTx]
  deriving Show

instance Arbitrary TestSetupWithTxsInMempool where
  arbitrary = do
    TestSetupWithTxs{testSetup} <-
      arbitrary `suchThat` (not . null . testInitialTxs . testSetup)
    txs <- sublistOf (testInitialTxs testSetup)
    return $ TestSetupWithTxsInMempool testSetup txs

-- TODO shrink

data TestSetupWithTxsInMempoolToRemove
  = TestSetupWithTxsInMempoolToRemove TestSetup (NE.NonEmpty TestTx)
  deriving Show

instance Arbitrary TestSetupWithTxsInMempoolToRemove where
  arbitrary =
    fmap convertToRemove $
      arbitrary `suchThat` thereIsAtLeastOneTx

  shrink =
    fmap convertToRemove
      . filter thereIsAtLeastOneTx
      . shrink
      . revertToRemove

thereIsAtLeastOneTx :: TestSetupWithTxsInMempool -> Bool
thereIsAtLeastOneTx (TestSetupWithTxsInMempool _ txs) = not $ null txs

convertToRemove :: TestSetupWithTxsInMempool -> TestSetupWithTxsInMempoolToRemove
convertToRemove (TestSetupWithTxsInMempool ts txs) =
  TestSetupWithTxsInMempoolToRemove ts (NE.fromList txs)

revertToRemove :: TestSetupWithTxsInMempoolToRemove -> TestSetupWithTxsInMempool
revertToRemove (TestSetupWithTxsInMempoolToRemove ts txs) =
  TestSetupWithTxsInMempool ts (NE.toList txs)

{-------------------------------------------------------------------------------
  TestMempool: a mempool with random contents
-------------------------------------------------------------------------------}

data TestMempool m = TestMempool
  { mempool :: Mempool m TestBlock
  -- ^ A mempool with random contents.
  --
  -- Starts out synced with the ledger.
  , getTraceEvents :: m [TraceEventMempool TestBlock]
  -- ^ When called, obtains all events traced after opening the mempool at
  -- the given state from oldest-to-newest.
  --
  -- Events traced while setting up the mempool to contain random contents
  -- are not included.
  , eraseTraceEvents :: m ()
  -- ^ Erase the events traced so far. The return of 'getTraceEvents' will
  -- again be an empty list until another event is traced.
  , addTxsToLedger :: [TestTx] -> STM m [Either TestTxError ()]
  -- ^ This function can be used to add transactions to the ledger/chain.
  --
  -- Remember to synchronise the mempool afterwards.
  , getCurrentLedger :: STM m (LedgerState TestBlock ValuesMK)
  -- ^ Return the current ledger.
  }

-- NOTE: at the end of the test, this function also checks whether the Mempool
-- contents are valid w.r.t. the current ledger.
--
-- NOTE: the test mempool's default capacity is set to a very large value in
-- module "Ouroboros.Consensus.Mock.Ledger.Block". This is why the generators do
-- not care about the mempool capacity when generating transactions for a
-- mempool with the 'NoMempoolCapacityBytesOverride' option set.
withTestMempool ::
  forall prop.
  Testable prop =>
  TestSetup ->
  (forall m. IOLike m => TestMempool m -> m prop) ->
  Property
withTestMempool setup@TestSetup{..} prop =
  counterexample (ppTestSetup setup)
    $ classify
      (isOverride testMempoolCapOverride)
      "MempoolCapacityBytesOverride"
    $ classify
      (not (isOverride testMempoolCapOverride))
      "NoMempoolCapacityBytesOverride"
    $ classify (null testInitialTxs) "empty Mempool"
    $ classify (not (null testInitialTxs)) "non-empty Mempool"
    $ runSimOrThrow setUpAndRun
 where
  isOverride (MempoolCapacityBytesOverride _) = True
  isOverride NoMempoolCapacityBytesOverride = False

  setUpAndRun :: forall m. IOLike m => m Property
  setUpAndRun = do
    -- Set up the LedgerInterface
    varCurrentLedgerState <- uncheckedNewTVarM testLedgerState
    let ledgerInterface =
          LedgerInterface
            { getCurrentLedgerState = \_reg -> do
                st <- readTVar varCurrentLedgerState
                pure $
                  MempoolLedgerDBView
                    (forgetLedgerTables st)
                    ( pure $
                        Right $
                          ReadOnlyForker
                            { roforkerClose = pure ()
                            , roforkerReadTables =
                                pure . (projectLedgerTables st `restrictValues'`)
                            , roforkerRangeReadTables = const $ pure emptyLedgerTables
                            , roforkerGetLedgerState = pure $ forgetLedgerTables st
                            , roforkerReadStatistics = pure Nothing
                            }
                    )
            }

    -- Set up the Tracer
    varEvents <- uncheckedNewTVarM []
    -- TODO use IOSim's dynamicTracer
    let tracer = Tracer $ \ev -> atomically $ modifyTVar varEvents (ev :)

    withRegistry $ \reg -> do
      -- Open the mempool and add the initial transactions
      mempool <-
        openMempoolWithoutSyncThread
          reg
          ledgerInterface
          testLedgerCfg
          testMempoolCapOverride
          tracer
      result <- addTxs mempool testInitialTxs

      -- the invalid transactions are reported in the same order they were
      -- added, so the first error is not the result of a cascade
      sequence_
        [ error $ "Invalid initial transaction: " <> condense invalidTx <> " because of error " <> show err
        | MempoolTxRejected invalidTx err <- result
        ]

      -- Clear the trace
      atomically $ writeTVar varEvents []

      -- Apply the property to the 'TestMempool' record
      res <-
        property
          <$> prop
            TestMempool
              { mempool
              , getTraceEvents = atomically $ reverse <$> readTVar varEvents
              , eraseTraceEvents = atomically $ writeTVar varEvents []
              , addTxsToLedger = addTxsToLedger varCurrentLedgerState
              , getCurrentLedger = readTVar varCurrentLedgerState
              }
      validContents <-
        atomically $
          checkMempoolValidity
            <$> readTVar varCurrentLedgerState
            <*> getSnapshot mempool
      return $ res .&&. validContents

  addTxToLedger ::
    forall m.
    IOLike m =>
    StrictTVar m (LedgerState TestBlock ValuesMK) ->
    TestTx ->
    STM m (Either TestTxError ())
  addTxToLedger varCurrentLedgerState tx = do
    ledgerState <- readTVar varCurrentLedgerState
    case runExcept (applyTxToLedger testLedgerCfg ledgerState tx) of
      Left e -> return $ Left e
      Right ledgerState' -> do
        writeTVar varCurrentLedgerState ledgerState'
        return $ Right ()

  addTxsToLedger ::
    forall m.
    IOLike m =>
    StrictTVar m (LedgerState TestBlock ValuesMK) ->
    [TestTx] ->
    STM m [(Either TestTxError ())]
  addTxsToLedger varCurrentLedgerState txs =
    mapM (addTxToLedger varCurrentLedgerState) txs

  -- \| Check whether the transactions in the 'MempoolSnapshot' are valid
  -- w.r.t. the current ledger state.
  checkMempoolValidity ::
    LedgerState TestBlock ValuesMK ->
    MempoolSnapshot TestBlock ->
    Property
  checkMempoolValidity
    ledgerState
    MempoolSnapshot
      { snapshotTxs
      , snapshotSlotNo
      } =
      case runExcept $
        repeatedlyM
          applyTx'
          [txForgetValidated tx | (tx, _, _) <- snapshotTxs]
          (TickedSimpleLedgerState ledgerState) of
        Right _ -> property True
        Left e -> counterexample (mkErrMsg e) $ property False
     where
      applyTx' tx st = do
        st' <-
          applyTx
            testLedgerCfg
            DoNotIntervene
            snapshotSlotNo
            tx
            st
        pure $ applyDiffs st (fst st')

      mkErrMsg e =
        "At the end of the test, the Mempool contents were invalid: "
          <> show e

{-------------------------------------------------------------------------------
  MempoolCapTestSetup
-------------------------------------------------------------------------------}

-- | Reuse 'TestSetupWithTxs' but just pick a specific capacity based on the
-- transactions to add.
newtype MempoolCapTestSetup = MempoolCapTestSetup TestSetupWithTxs
  deriving Show

instance Arbitrary MempoolCapTestSetup where
  -- TODO: shrink
  arbitrary = do
    testSetupWithTxs@TestSetupWithTxs{testSetup, txs} <- arbitrary
    -- The Mempool should at least be capable of containing the transactions
    -- it already contains.
    let currentSize = foldMap genTxSize (testInitialTxs testSetup)
        capacityMinBound = currentSize
        validTxsToAdd = [tx | (tx, True) <- txs]
        -- Use the current size + the sum of all the valid transactions to add
        -- as the upper bound.
        capacityMaxBound = currentSize <> foldMap genTxSize validTxsToAdd
    -- Note that we could pick @currentSize@, meaning that we can't add any
    -- more transactions to the Mempool

    capacity <-
      choose
        ( unByteSize32 capacityMinBound
        , unByteSize32 capacityMaxBound
        )
    let testSetup' =
          testSetup
            { testMempoolCapOverride =
                MempoolCapacityBytesOverride $
                  ByteSize32 $
                    capacity
            }
    return $ MempoolCapTestSetup testSetupWithTxs{testSetup = testSetup'}

{-------------------------------------------------------------------------------
  TxSeq Properties
-------------------------------------------------------------------------------}

-- | Finds elements in the sequence
prop_TxSeq_lookupByTicketNo_complete :: [Int] -> Property
prop_TxSeq_lookupByTicketNo_complete xs =
  counterexample (show txseq) $
    conjoin
      [ case TxSeq.lookupByTicketNo txseq tn of
          Just tx' -> tx === tx'
          Nothing -> property False
      | (tx, tn, _byteSize) <- TxSeq.toTuples txseq
      ]
 where
  txseq :: TxSeq TheMeasure Int
  txseq =
    TxSeq.fromList $
      [TxTicket x (TicketNo i) mempty | x <- xs | i <- [0 ..]]

-- | Only finds elements in the sequence
prop_TxSeq_lookupByTicketNo_sound ::
  [Small Int] -> Small Int -> Property
prop_TxSeq_lookupByTicketNo_sound smalls small =
  case TxSeq.lookupByTicketNo txseq (mkTicketNo needle) of
    Just tx' ->
      label "successful hit" $
        counterexample ("needle: " ++ show needle) $
          counterexample ("haystack: " ++ show haystack) $
            tx' === needle
    Nothing ->
      label "successful miss" $
        property $
          needle `Set.notMember` haystack'
 where
  -- an ascending haystack of nonnegatives
  haystack = Set.toAscList haystack'
  haystack' = Set.fromList $ map (abs . getSmall) smalls

  -- a nonnegative needle
  needle = abs (getSmall small)

  -- the identity mapping over haystack
  txseq :: TxSeq TheMeasure Int
  txseq =
    List.foldl' (TxSeq.:>) TxSeq.Empty $ map mkTicket haystack

  mkTicket x = TxTicket x (mkTicketNo x) mempty
  mkTicketNo = TicketNo . toEnum

-- | Test that the 'fst' of the result of 'splitAfterTxSize' only contains
-- 'TxTicket's whose summed up transaction sizes are less than or equal to
-- that of the byte size which the 'TxSeq' was split on.
prop_TxSeq_splitAfterTxSize :: TxSizeSplitTestSetup -> Property
prop_TxSeq_splitAfterTxSize tss =
  property $ txSizeSum (TxSeq.toList before) <= tssTxSizeToSplitOn
 where
  TxSizeSplitTestSetup{tssTxSizeToSplitOn} = tss

  (before, _after) = splitAfterTxSize txseq tssTxSizeToSplitOn

  txseq :: TxSeq TheMeasure Int
  txseq = txSizeSplitTestSetupToTxSeq tss

  txSizeSum :: [TxTicket TheMeasure tx] -> TheMeasure
  txSizeSum = foldMap txTicketSize

-- | Test that the results of 'splitAfterTxSizeSpec', a specification of
-- 'splitAfterTxSize', match those of the real 'splitAfterTxSize'
-- implementation.
prop_TxSeq_splitAfterTxSizeSpec :: TxSizeSplitTestSetup -> Property
prop_TxSeq_splitAfterTxSizeSpec tss =
  TxSeq.toList implBefore === TxSeq.toList specBefore
    .&&. TxSeq.toList implAfter === TxSeq.toList specAfter
 where
  TxSizeSplitTestSetup{tssTxSizeToSplitOn} = tss

  (implBefore, implAfter) = splitAfterTxSize txseq tssTxSizeToSplitOn

  (specBefore, specAfter) = splitAfterTxSizeSpec txseq tssTxSizeToSplitOn

  txseq :: TxSeq TheMeasure Int
  txseq = txSizeSplitTestSetupToTxSeq tss

{-------------------------------------------------------------------------------
  TxSizeSplitTestSetup
-------------------------------------------------------------------------------}

data TxSizeSplitTestSetup = TxSizeSplitTestSetup
  { tssTxSizes :: ![TheMeasure]
  , tssTxSizeToSplitOn :: !TheMeasure
  }
  deriving Show

instance Arbitrary TxSizeSplitTestSetup where
  arbitrary = do
    let txSizeMaxBound = 10 * 1024 * 1024 -- 10 mebibyte transaction max bound
    txSizes <- listOf $ choose (1, txSizeMaxBound :: Word32)
    let totalTxsSize = sum txSizes
    txSizeToSplitOn <-
      frequency
        [ (1, pure 0)
        , (7, choose (0, totalTxsSize))
        , (1, pure totalTxsSize)
        , (1, choose (totalTxsSize + 1, totalTxsSize + 1000))
        ]
    pure
      TxSizeSplitTestSetup
        { tssTxSizes = map (IgnoringOverflow . ByteSize32) txSizes
        , tssTxSizeToSplitOn = IgnoringOverflow $ ByteSize32 txSizeToSplitOn
        }

  shrink TxSizeSplitTestSetup{tssTxSizes, tssTxSizeToSplitOn} =
    [ TxSizeSplitTestSetup
        { tssTxSizes = map (IgnoringOverflow . ByteSize32) tssTxSizes'
        , tssTxSizeToSplitOn = IgnoringOverflow $ ByteSize32 tssTxSizeToSplitOn'
        }
    | tssTxSizes' <- shrinkList (const []) [y | IgnoringOverflow (ByteSize32 y) <- tssTxSizes]
    , tssTxSizeToSplitOn' <- shrinkIntegral x
    ]
   where
    IgnoringOverflow (ByteSize32 x) = tssTxSizeToSplitOn

-- | Convert a 'TxSizeSplitTestSetup' to a 'TxSeq'.
txSizeSplitTestSetupToTxSeq :: TxSizeSplitTestSetup -> TxSeq TheMeasure Int
txSizeSplitTestSetupToTxSeq TxSizeSplitTestSetup{tssTxSizes} =
  TxSeq.fromList
    [ TxTicket 1 (TicketNo i) tssTxSize
    | tssTxSize <- tssTxSizes
    | i <- [0 ..]
    ]

{-------------------------------------------------------------------------------
  TicketNo Properties
-------------------------------------------------------------------------------}

-- | Testing plan:
--
-- * Perform a number of actions: either add a new valid transaction to the
--   Mempool (invalid transactions have no effect on the @idx@s) or remove an
--   existing transaction from the Mempool.
--
-- * After executing each action, check whether the current ticket assignment
--   is still consistent with the expected ticket assignment. The ticket
--   assignment is a mapping from 'TicketNo' (@idx@) to transaction. The same
--   ticket may never be reused for another transaction, which is exactly what
--   we're testing here.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'. This makes it easier to generate 'Actions', because they
-- don't have to take the initial contents of the Mempool and 'LedgerState'
-- into account.
prop_Mempool_idx_consistency :: Actions -> Property
prop_Mempool_idx_consistency (Actions actions) =
  withTestMempool emptyTestSetup $ \testMempool@TestMempool{mempool} ->
    fmap conjoin $ forM actions $ \action -> do
      txsInMempool <-
        map prjTx . snapshotTxs
          <$> atomically (getSnapshot mempool)
      actionProp <- executeAction testMempool action
      currentAssignment <- currentTicketAssignment mempool
      return
        $
        --  #692, fixed in #742: if the mempool becomes empty during
        -- operation. In this case, the 'TicketNo' counter would "reset" to
        -- 'zeroTicketNo'. Clients interacting with the mempool likely won't
        -- account for this.
        classify
          (Map.null currentAssignment)
          "Mempool became empty"
        $
        -- #692, fixed in #742: the transaction at the "back" of the mempool
        -- becomes invalid and is removed. In this case, the next
        -- transaction to be appended would take on the 'TicketNo' of the
        -- removed transaction (since this function only increments the
        -- 'TicketNo' associated with the transaction at the back of the
        -- mempool). Clients interacting with the mempool likely won't
        -- account for this.
        classify
          (lastOfMempoolRemoved (map txForgetValidated txsInMempool) action)
          "The last transaction in the mempool is removed"
        $ actionProp
          .&&. currentAssignment `isConsistentWith` expectedAssignment
 where
  expectedAssignment = expectedTicketAssignment actions

  emptyTestSetup =
    TestSetup
      { testLedgerCfg = testLedgerConfigNoSizeLimits
      , testLedgerState = testInitLedger
      , testInitialTxs = []
      , testMempoolCapOverride =
          MempoolCapacityBytesOverride $
            ByteSize32 $
              1024 * 1024 * 1024
              -- There's no way this test will need more than a gibibyte.
      }

  lastOfMempoolRemoved txsInMempool = \case
    AddTxs _ -> False
    RemoveTxs txs -> last txsInMempool `elem` txs

  isConsistentWith curAsgn expAsgn
    | curAsgn `Map.isSubmapOf` expAsgn =
        property True
    | otherwise =
        counterexample
          ( "Current tickets assignments: "
              <> show curAsgn
              <> "\ninconsistent with expected: "
              <> show expAsgn
          )
          False

{-------------------------------------------------------------------------------
  TicketAssignment & Actions
-------------------------------------------------------------------------------}

data Action
  = -- | When part of 'Actions', all these transactions are valid.
    AddTxs [TestTx]
  | -- | When part of 'Actions', removing these transactions will not
    -- invalidate any other transactions.
    RemoveTxs [TestTx]
  deriving Show

newtype Actions = Actions [Action]
  deriving Show

-- | Track to which ticket number each transaction is assigned.
--
-- * We don't want multiple transaction to be assigned the same ticket number.
-- * We want each transaction to be always assigned the same ticket number.
type TicketAssignment = Map TicketNo TestTxId

-- | Compute the expected 'TicketAssignment' for the given actions.
expectedTicketAssignment :: [Action] -> TicketAssignment
expectedTicketAssignment actions =
  evalState (foldM addMapping mempty actions) (succ zeroTicketNo)
 where
  addMapping :: TicketAssignment -> Action -> State TicketNo TicketAssignment
  addMapping mapping (RemoveTxs _txs) = return mapping
  addMapping mapping (AddTxs txs) = do
    newMappings <- forM txs $ \tx -> do
      nextTicketNo <- get
      modify succ
      return (nextTicketNo, txId tx)
    return $ Map.union mapping (Map.fromList newMappings)

-- | Executes the action and verifies that it is actually executed using the
-- tracer, hence the 'Property' in the return type.
executeAction :: forall m. IOLike m => TestMempool m -> Action -> m Property
executeAction testMempool action = case action of
  AddTxs txs -> do
    void $ addTxs mempool txs
    allTraces <- expectTraceEvent Just
    let tracedAddedTxs = [tx | TraceMempoolAddedTx tx _ _ <- allTraces] -- expectTraceEvent $ \case
    -- TraceMempoolAddedTx tx _ _ -> Just tx
    -- _                          -> Nothing
    return $
      if map txForgetValidated tracedAddedTxs == txs
        then property True
        else
          counterexample
            ( "Expected TraceMempoolAddedTx events for "
                <> condense txs
                <> " but got "
                <> condense (map txForgetValidated tracedAddedTxs)
                <> " evs: "
                <> show allTraces
            )
            False
  RemoveTxs txs -> do
    let txs' = NE.fromList $ map txId txs
    removeTxsEvenIfValid mempool txs'
    tracedManuallyRemovedTxs <- expectTraceEvent $ \case
      TraceMempoolManuallyRemovedTxs txIds _ _ -> Just txIds
      _ -> Nothing
    return $
      if concatMap NE.toList tracedManuallyRemovedTxs == map txId txs
        then property True
        else
          counterexample
            ( "Expected a TraceMempoolManuallyRemovedTxs event for "
                <> condense txs
                <> " but got "
                <> condense (map NE.toList tracedManuallyRemovedTxs)
            )
            False
 where
  TestMempool
    { mempool
    , eraseTraceEvents
    , getTraceEvents
    } = testMempool

  expectTraceEvent :: (TraceEventMempool TestBlock -> Maybe a) -> m [a]
  expectTraceEvent extractor = do
    evs <- getTraceEvents
    eraseTraceEvents
    return $ mapMaybe extractor evs

currentTicketAssignment ::
  IOLike m =>
  Mempool m TestBlock -> m TicketAssignment
currentTicketAssignment Mempool{testSyncWithLedger} = do
  MempoolSnapshot{snapshotTxs} <- testSyncWithLedger
  return $
    Map.fromList
      [ (ticketNo, txId (txForgetValidated tx))
      | (tx, ticketNo, _byteSize) <- snapshotTxs
      ]

instance Arbitrary Actions where
  arbitrary = sized $ genActions (choose (1, 3))

genActions ::
  -- | Generate the number of transactions to add
  Gen Int ->
  -- | How many actions
  Int ->
  Gen Actions
genActions genNbToAdd = go testInitLedger mempty mempty
 where
  cfg = testLedgerConfigNoSizeLimits

  go ::
    LedgerState TestBlock ValuesMK ->
    -- \^ Current ledger state with the contents of the Mempool applied
    [TestTx] ->
    -- \^ Transactions currently in the Mempool
    [Action] ->
    -- \^ Already generated actions
    Int ->
    -- \^ Number of actions left to generate
    Gen Actions
  go ledger txs actions n
    | n <= 0 = return $ Actions (reverse actions)
    | otherwise =
        arbitrary >>= \case
          True
            | not (null txs) ->
                -- Remove a transaction (or multiple), but only if there are
                -- transactions to remove
                do
                  tx <- elements txs
                  let ((vTxs, iTxs), ledger') =
                        first (List.partition (isRight . snd)) $
                          validateTxs cfg testInitLedger (filter (/= tx) txs)
                      txs' = map fst vTxs
                      removedTxs = tx : map fst iTxs
                  go ledger' txs' (RemoveTxs removedTxs : actions) (n - 1)
          _ -> do
            nbToAdd <- genNbToAdd
            (txs', ledger') <- genValidTxs nbToAdd ledger
            go ledger' (txs' <> txs) (AddTxs txs' : actions) (n - 1)
