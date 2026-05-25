{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mempool with a mocked ledger interface
module Test.Consensus.Mempool.Mocked
  ( InitialMempoolAndModelParams (..)

    -- * Mempool with a mocked LedgerDB interface
  , MockedMempool (getMempool)
  , openMockedMempool
  , setLedgerState

    -- * Mempool API functions
  , addTx
  , getTxs
  , removeTxsEvenIfValid
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.DeepSeq (NFData (rnf))
import Control.Tracer (Tracer)
import qualified Data.List.NonEmpty as NE
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.HeaderValidation as Header
import Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool
import Ouroboros.Consensus.Mempool.API
  ( AddTxOnBehalfOf
  , MempoolAddTxResult
  )

-- | Mocked mempool, parameterised over a function that wraps the in-memory
-- 'LedgerState' into a 'StateHandle' on demand.
--
-- The TVar holds the in-memory 'LedgerState' only. Every call into the
-- mempool that needs a handle invokes 'immpMakeStateHandle' to build one. For
-- blocks whose 'StateHandle' is just a newtype around 'LedgerState' (Byron,
-- the test blocks, mock blocks) the caller can pass the data constructor
-- directly.
data MockedMempool m blk = MockedMempool
  { getLedgerInterface :: !(Mempool.LedgerInterface m blk)
  , getLedgerStateTVar :: !(StrictTVar m (LedgerState blk))
  , getMempool :: !(Mempool m blk)
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
  rnf MockedMempool{} = ()

data InitialMempoolAndModelParams m blk = MempoolAndModelParams
  { immpInitialState :: !(LedgerState blk)
  -- ^ Initial ledger state for the mocked Ledger DB interface.
  , immpLedgerConfig :: !(Ledger.LedgerConfig blk)
  -- ^ Ledger configuration, which is needed to open the mempool.
  , immpMakeStateHandle :: !(LedgerState blk -> StateHandle m blk)
  -- ^ How to wrap a pure 'LedgerState' into a 'StateHandle'. For blocks
  -- with no on-disk tables this is just the 'StateHandle' constructor
  -- (e.g. @ByronStateHandle@).
  }

openMockedMempool ::
  ( Ledger.LedgerSupportsMempool blk
  , Ledger.HasTxId (Ledger.GenTx blk)
  , Header.ValidateEnvelope blk
  , BlockSupportsLedgerHD IO blk
  , Ledger.GetTip LedgerState blk
  , NoThunks (TickedStateHandle IO blk)
  ) =>
  Mempool.MempoolCapacityBytesOverride ->
  Tracer IO (Mempool.TraceEventMempool blk) ->
  InitialMempoolAndModelParams IO blk ->
  IO (MockedMempool IO blk)
openMockedMempool capacityOverride tracer initialParams = do
  currentLedgerStateTVar <- newTVarIO (immpInitialState initialParams)
  let mkHandle = immpMakeStateHandle initialParams
      ledgerItf =
        Mempool.LedgerInterface
          { Mempool.getCurrentLedgerTip =
              Ledger.getTip <$> readTVar currentLedgerStateTVar
          , Mempool.withCurrentLedgerStateDup = \k -> do
              st <- atomically $ readTVar currentLedgerStateTVar
              k (mkHandle st)
          }
  mempool <-
    Mempool.openMempoolWithoutSyncThread
      ledgerItf
      (immpLedgerConfig initialParams)
      capacityOverride
      (Nothing :: Maybe Mempool.MempoolTimeoutConfig)
      tracer
  pure
    MockedMempool
      { getLedgerInterface = ledgerItf
      , getLedgerStateTVar = currentLedgerStateTVar
      , getMempool = mempool
      }

setLedgerState ::
  MockedMempool IO blk ->
  LedgerState blk ->
  IO ()
setLedgerState MockedMempool{getLedgerStateTVar} newSt =
  atomically $ writeTVar getLedgerStateTVar newSt

addTx ::
  MockedMempool m blk ->
  AddTxOnBehalfOf ->
  Ledger.GenTx blk ->
  m (MempoolAddTxResult blk)
addTx = Mempool.addTx . getMempool

removeTxsEvenIfValid ::
  MockedMempool m blk ->
  NE.NonEmpty (Ledger.GenTxId blk) ->
  m ()
removeTxsEvenIfValid = Mempool.removeTxsEvenIfValid . getMempool

getTxs ::
  forall blk.
  Ledger.LedgerSupportsMempool blk =>
  MockedMempool IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
  snapshotTxs <-
    fmap Mempool.snapshotTxs $
      atomically $
        Mempool.getSnapshot $
          getMempool mockedMempool
  pure $ fmap prjTx snapshotTxs
 where
  prjTx (a, _b, _c) = Ledger.txForgetValidated a :: Ledger.GenTx blk
