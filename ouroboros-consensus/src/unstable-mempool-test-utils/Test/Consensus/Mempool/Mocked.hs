{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mempool with a mocked ledger interface
module Test.Consensus.Mempool.Mocked (
    InitialMempoolAndModelParams (..)
    -- * Mempool with a mocked LedgerDB interface
  , MockedMempool (getMempool)
  , openMockedMempool
  , setLedgerState
    -- * Mempool API functions
  , addTx
  , getTxs
  , removeTxs
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar,
                     atomically, newTVarIO, readTVar, writeTVar)
import           Control.DeepSeq (NFData (rnf))
import           Control.Tracer (Tracer)
import           Ouroboros.Consensus.HeaderValidation as Header
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Mempool.API (AddTxOnBehalfOf,
                     MempoolAddTxResult)

data MockedMempool m blk = MockedMempool {
      getLedgerInterface :: !(Mempool.LedgerInterface m blk)
    , getLedgerStateTVar :: !(StrictTVar m (LedgerState blk))
    , getMempool         :: !(Mempool m blk)
    }

instance NFData (MockedMempool m blk) where
  -- TODO: check we're OK with skipping the evaluation of the
  -- MockedMempool. The only data we could force here is the
  -- 'LedgerState' inside 'getLedgerStateTVar', but that would require adding a
  -- 'NFData' constraint and perform unsafe IO. Since we only require this
  -- instance to be able to use
  -- [env](<https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env),
  -- and we only care about initializing the mempool before running the
  -- benchmarks, maybe this definition is enough.
  rnf MockedMempool {} = ()

data InitialMempoolAndModelParams blk = MempoolAndModelParams {
      -- | Initial ledger state for the mocked Ledger DB interface.
      immpInitialState :: !(Ledger.LedgerState blk)
      -- | Ledger configuration, which is needed to open the mempool.
    , immpLedgerConfig :: !(Ledger.LedgerConfig blk)
    }

openMockedMempool ::
     ( Ledger.LedgerSupportsMempool blk
     , Ledger.HasTxId (Ledger.GenTx blk)
     , Header.ValidateEnvelope blk
     )
  => Mempool.MempoolCapacityBytesOverride
  -> Tracer IO (Mempool.TraceEventMempool blk)
  -> InitialMempoolAndModelParams blk
  -> IO (MockedMempool IO blk)
openMockedMempool capacityOverride tracer initialParams = do
    currentLedgerStateTVar <- newTVarIO (immpInitialState initialParams)
    let ledgerItf = Mempool.LedgerInterface {
            Mempool.getCurrentLedgerState = readTVar currentLedgerStateTVar
        }
    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   (immpLedgerConfig initialParams)
                   capacityOverride
                   tracer
    pure MockedMempool {
        getLedgerInterface = ledgerItf
      , getLedgerStateTVar = currentLedgerStateTVar
      , getMempool         = mempool
    }

setLedgerState ::
     MockedMempool IO blk
  -> LedgerState blk
  -> IO ()
setLedgerState MockedMempool {getLedgerStateTVar} newSt =
  atomically $ writeTVar getLedgerStateTVar newSt

addTx ::
     MockedMempool m blk
  -> AddTxOnBehalfOf
  -> Ledger.GenTx blk
  -> m (MempoolAddTxResult blk)
addTx = Mempool.addTx . getMempool

removeTxs ::
     MockedMempool m blk
  -> [Ledger.GenTxId blk]
  -> m ()
removeTxs = Mempool.removeTxs . getMempool

getTxs :: forall blk.
     (Ledger.LedgerSupportsMempool blk)
  => MockedMempool IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
    snapshotTxs <- fmap Mempool.snapshotTxs $ atomically
                                            $ Mempool.getSnapshot
                                            $ getMempool mockedMempool
    pure $ fmap prjTx snapshotTxs
  where
    prjTx (a, _b, _c) = Ledger.txForgetValidated a :: Ledger.GenTx blk
