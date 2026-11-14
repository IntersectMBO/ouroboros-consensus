{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Exact local discovery and coherent mempool maintenance regressions.
module Test.Consensus.Mempool.LocalReuse (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Slotting.Time as Time
import Control.Monad (foldM, void)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
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
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (testBlockLedgerConfigFrom, testInitLedgerWithState)

newEnv :: IOLike m => m (MempoolEnv m TestBlock)
newEnv =
  initMempoolEnv
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

-- Compare exact hash/byte observations without requiring equality on the
-- existential input retained by a transaction view.
view :: InternalState TestBlock -> [(TxHash, BS.ByteString)]
view st = [(h, leiosTxMempoolViewBytes tx) | (h, tx) <- Map.toAscList (isLeiosTxs st)]

projectBytes :: GenTx TestBlock -> Maybe BS.ByteString
projectBytes = fmap leiosTxMempoolViewBytes . leiosTxMempoolView

tests :: TestTree
tests =
  testGroup
    "Mempool local reuse"
    [ testProperty "index agrees with ordinary TX sequence through add/remove scripts" prop_indexCoherent
    , testCase "exact serialized hash and size follow insertion/removal" $
        assertBool "coherent source index" $
          runSimOrThrow $ do
            env <- newEnv
            add env 1
            add env 2
            before <- atomically (readState env)
            let checkEntry (h, v) = h == ltmvHash v && h == hashLeiosTx (MkLeiosTx (leiosTxMempoolViewBytes v))
            implRemoveTxsEvenIfValid env (txId (mkGenTx 1 (ByteSize32 100)) NE.:| [])
            after <- atomically (readState env)
            pure
              ( Map.size (isLeiosTxs before) == 2
                  && all checkEntry (Map.toList (isLeiosTxs before))
                  && Map.size (isLeiosTxs after) == 1
              )
    , testCase "index hashes validated output after ledger normalization" $
        assertBool "validated output, not the original input" $
          runSimOrThrow $ do
            env <- newEnv
            let input = mkRectifyingGenTx 1 (ByteSize32 100)
                normalized = mkGenTx 1 (ByteSize32 100)
            void $ implAddTx env ProductionAddTx AddTxForRemotePeer input
            st <- atomically (readState env)
            implRemoveTxsEvenIfValid env (txId input NE.:| [])
            after <- atomically (readState env)
            pure $ case (projectBytes input, projectBytes normalized) of
              (Just original, Just bytes) ->
                txId input == txId normalized
                  && txWireSize input == txWireSize normalized
                  && BS.length original == BS.length bytes
                  && original /= bytes
                  && hashLeiosTx (MkLeiosTx original) /= hashLeiosTx (MkLeiosTx bytes)
                  && view st == [(hashLeiosTx (MkLeiosTx bytes), bytes)]
                  && Map.null (isLeiosTxs after)
              _ -> False
    , testCase "full and incremental revalidation preserve matching index entries" $
        assertBool "same surviving index" $
          runSimOrThrow $ do
            env <- newEnv
            void $ implAddTx env ProductionAddTx AddTxForRemotePeer (mkRectifyingGenTx 1 (ByteSize32 100))
            add env 2
            st <- atomically (readState env)
            frk <- MVar.readMVar (mpEnvForker env)
            let tickets = TxSeq.toList (isTxs st)
                revalidate =
                  revalidateTxsFor
                    frk
                    (mpEnvCapacityOverride env)
                    (mpEnvLedgerCfg env)
                    (isSlotNo st)
                    (isLedgerState st `withLedgerTables` emptyLedgerTables)
                    (isLastTicketNo st)
                    (isRemovalCounter st)
            RevalidateTxsResult full _ <- revalidate tickets
            seed <- revalidate (take 1 tickets)
            RevalidateTxsResult final _ <-
              revalidateTxsFor'
                frk
                (mpEnvCapacityOverride env)
                (mpEnvLedgerCfg env)
                (isSlotNo st)
                seed
                (isLastTicketNo st)
                (drop 1 tickets)
            pure (view final == view st && view full == view st)
    , testCase "canceled state owner restores the index and releases blocked readers" $
        assertBool "restored state remains usable" $
          runSimOrThrow $ do
            env <- newEnv
            add env 1
            before <- atomically (readState env)
            entered <- newEmptyTMVarIO
            block <- newEmptyTMVarIO
            withAsync
              ( withTMVarAnd (mpEnvStateVar env) (const (pure ())) $ \old () -> do
                  atomically $ putTMVar entered ()
                  _ <- atomically $ readTMVar block
                  pure ((), old{isLeiosTxs = Map.empty})
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
                        && Map.size (isLeiosTxs after) == 2
                    )
    ]

-- Compute the oracle from the ordinary transaction sequence and fresh wire
-- projections, independently of the maintained fingerprints/index.
prop_indexCoherent :: [(Bool, Word8)] -> Bool
prop_indexCoherent actions = runSimOrThrow $ do
  env <- newEnv
  foldM
    ( \ok (insert, raw) -> do
        let n = fromIntegral (raw `mod` 16)
        if insert
          then
            void $
              implAddTx env ProductionAddTx AddTxForRemotePeer $
                (if even raw then mkRectifyingGenTx else mkGenTx) n (ByteSize32 100)
          else
            implRemoveTxsEvenIfValid env (txId (mkGenTx n (ByteSize32 100)) NE.:| [])
        st <- atomically (readState env)
        let expected =
              Map.fromList
                [ (hashLeiosTx (MkLeiosTx bytes), bytes)
                | ticket <- TxSeq.toList (isTxs st)
                , let tx = txForgetValidated @TestBlock (validatedTx (TxSeq.txTicketTx ticket))
                , Just bytes <- [projectBytes tx]
                ]
            actual = Map.map leiosTxMempoolViewBytes (isLeiosTxs st)
        pure (ok && actual == expected)
    )
    True
    actions
