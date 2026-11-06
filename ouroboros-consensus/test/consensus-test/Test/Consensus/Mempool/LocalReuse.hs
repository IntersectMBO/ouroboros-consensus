{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Deterministic mempool source-index and blocked-reader regressions for E22.
module Test.Consensus.Mempool.LocalReuse (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import Control.Monad (void)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block.SupportsPeras (pattern PerasEnabled)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Mempool.Update
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar
import Ouroboros.Consensus.Util.STM (withTMVarAnd)
import Test.Consensus.Mempool.Fairness.TestBlock
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (testBlockLedgerConfigFrom, testInitLedgerWithState)

newEnv :: IOLike m => Bool -> m (MempoolEnv m TestBlock)
newEnv enabled =
  initMempoolEnvWithLocalReuse
    enabled
    ledger
    cfg
    (mkCapacityBytesOverride (ByteSize32 100000))
    Nothing
    nullTracer
 where
  ledger =
    LedgerInterface $
      pure $
        MempoolLedgerDBView
          (testInitLedgerWithState NoPayLoadDependentState)
          ( pure $
              Right $
                ReadOnlyForker
                  { roforkerClose = pure ()
                  , roforkerReadTables = const (pure emptyLedgerTables)
                  , roforkerRangeReadTables = const (pure (emptyLedgerTables, Nothing))
                  , roforkerGetLedgerState = pure (testInitLedgerWithState NoPayLoadDependentState)
                  , roforkerReadStatistics = pure (Statistics 0)
                  }
          )
  cfg =
    testBlockLedgerConfigFrom
      ( HardFork.defaultEraParams
          (SecurityParam (knownNonZeroBounded @10))
          (Time.slotLengthFromSec 2)
          (PerasEnabled ())
      )

add :: (IOLike m, MonadTimer m) => MempoolEnv m TestBlock -> Int -> m ()
add env n = void $ implAddTx env ProductionAddTx AddTxForRemotePeer (mkGenTx n (ByteSize32 100))

readState :: MonadSTM m => MempoolEnv m TestBlock -> STM m (InternalState TestBlock)
readState = readTMVar . mpEnvStateVar

-- Compare serializable metadata and ordinary TXs, rather than requiring an Eq
-- instance for every ledger's Validated wrapper.
view :: InternalState TestBlock -> (Natural, [(TxHash, BytesSize, GenTx TestBlock)])
view is =
  ( isLeiosEpoch is
  , [(h, n, txForgetValidated @TestBlock v) | (h, (n, v)) <- Map.toAscList (isLeiosIndex is)]
  )

tests :: TestTree
tests =
  testGroup
    "Mempool local reuse"
    [ testCase "disabled index does no optional source work" $
        runSimOrThrow
          ( do
              env <- newEnv False
              add env 1
              st <- atomically (readState env)
              pure (isLeiosEpoch st, Map.null (isLeiosIndex st))
          )
          @?= (0, True)
    , testCase "exact serialized hash and size follow insertion/removal" $
        assertBool "coherent source index" $
          runSimOrThrow $ do
            env <- newEnv True
            add env 1
            add env 2
            before <- atomically (readState env)
            let checkEntry (h, (n, v)) = case txBytesForLeios (Proxy @TestBlock) (txForgetValidated @TestBlock v) of
                  Just bytes -> h == hashLeiosTx (MkLeiosTx bytes) && fromIntegral n == BS.length bytes
                  _ -> False
            implRemoveTxsEvenIfValid env (txId (mkGenTx 1 (ByteSize32 100)) NE.:| [])
            after <- atomically (readState env)
            pure
              ( Map.size (isLeiosIndex before) == 2
                  && all checkEntry (Map.toList (isLeiosIndex before))
                  && Map.size (isLeiosIndex after) == 1
                  && isLeiosEpoch after > isLeiosEpoch before
              )
    , testCase "full and incremental revalidation preserve matching index entries" $
        assertBool "same surviving index" $
          runSimOrThrow $ do
            env <- newEnv True
            add env 1
            add env 2
            st <- atomically (readState env)
            frk <- MVar.readMVar (mpEnvForker env)
            let tickets = TxSeq.toList (isTxs st)
            seed <-
              revalidateTxsFor
                frk
                (mpEnvCapacityOverride env)
                (mpEnvLedgerCfg env)
                (isSlotNo st)
                (isLedgerState st `withLedgerTables` emptyLedgerTables)
                (isLastTicketNo st)
                (isRemovalCounter st)
                True
                (take 1 tickets)
            RevalidateTxsResult final _ <-
              revalidateTxsFor'
                frk
                (mpEnvCapacityOverride env)
                (mpEnvLedgerCfg env)
                (isSlotNo st)
                seed
                (isLastTicketNo st)
                (drop 1 tickets)
            pure (snd (view final) == snd (view st))
    , testCase "R01 removal alone wakes a source-epoch waiter" $
        assertBool "epoch wake" $
          runSimOrThrow $ do
            env <- newEnv True
            add env 1
            before <- isLeiosEpoch <$> atomically (readState env)
            withAsync
              ( atomically $ do st <- readState env; check (isLeiosEpoch st > before); pure (Map.null (isLeiosIndex st))
              )
              $ \reader -> do
                implRemoveTxsEvenIfValid env (txId (mkGenTx 1 (ByteSize32 100)) NE.:| [])
                wait reader
    , testCase "R03 canceled state owner restores index/epoch and releases blocked reader" $
        assertBool "restored state remains usable" $
          runSimOrThrow $ do
            env <- newEnv True
            add env 1
            before <- atomically (readState env)
            entered <- newEmptyTMVarIO
            block <- newEmptyTMVarIO
            withAsync
              ( withTMVarAnd (mpEnvStateVar env) (const (pure ())) $ \old () -> do
                  atomically $ putTMVar entered ()
                  _ <- atomically $ readTMVar block
                  pure ((), old{isLeiosEpoch = isLeiosEpoch old + 100, isLeiosIndex = Map.empty})
              )
              $ \owner -> do
                atomically $ readTMVar entered
                withAsync (view <$> atomically (readState env)) $ \reader -> do
                  void $ cancel owner
                  observed <- wait reader
                  add env 2
                  after <- atomically (readState env)
                  pure
                    ( observed == view before
                        && Map.size (isLeiosIndex after) == 2
                        && isLeiosEpoch after == isLeiosEpoch before + 1
                    )
    ]
