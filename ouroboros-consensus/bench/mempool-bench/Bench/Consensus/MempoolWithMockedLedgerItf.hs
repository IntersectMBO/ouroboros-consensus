{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Mempool with a mocked ledger interface
module Bench.Consensus.MempoolWithMockedLedgerItf (
    InitialMempoolAndModelParams (..)
    -- * Mempool with a mocked LedgerDB interface
  , MempoolWithMockedLedgerItf (getMempool)
  , openMempoolWithMockedLedgerItf
    -- * Mempool API functions
  , addTx
  , getTxs
  , removeTxs
  ) where

import           Bench.Consensus.Mempool.Params
import           Control.Concurrent.Class.MonadSTM.Strict
                     (MonadSTM (atomically), newTVarIO)
import           Control.DeepSeq (NFData (rnf))
import           Control.Tracer (Tracer, nullTracer)
import           Data.Foldable (foldMap')
import qualified Data.List.NonEmpty as NE
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsProtocol as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Mempool.API (AddTxOnBehalfOf,
                     MempoolAddTxResult)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update
import qualified Ouroboros.Consensus.Storage.LedgerDB.Query as Query
import           System.Directory (getTemporaryDirectory)
import           System.FS.API (SomeHasFS (SomeHasFS))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)
import           System.IO.Temp (createTempDirectory)

data MempoolWithMockedLedgerItf m blk = MempoolWithMockedLedgerItf {
      getLedgerInterface    :: !(Mempool.LedgerInterface m blk)
    , getLedgerDB           :: !(DbChangelog (LedgerState blk))
    , getLedgerBackingStore :: !(LedgerBackingStore m (LedgerState blk))
    , getMempool            :: !(Mempool m blk)
    }

instance NFData (MempoolWithMockedLedgerItf m blk) where
  -- TODO: check we're OK with skipping the evaluation of the
  -- MempoolWithMockedLedgerItf. The only data we could force here is the
  -- 'LedgerState' inside 'getLedgerStateTVar', but that would require adding a
  -- 'NFData' constraint and perform unsafe IO. Since we only require this
  -- instance to be able to use
  -- [env](<https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env),
  -- and we only care about initializing the mempool before running the
  -- benchmarks, maybe this definition is enough.
  rnf MempoolWithMockedLedgerItf {} = ()

openMempoolWithMockedLedgerItf ::
     ( Ledger.LedgerSupportsMempool blk
     , Ledger.HasTxId (Ledger.GenTx blk)
     , Ledger.CanSerializeLedgerTables (LedgerState blk)
     , Ledger.LedgerSupportsProtocol blk
     )
  => Mempool.MempoolCapacityBytesOverride
  -> Tracer IO (Mempool.TraceEventMempool blk)
  -> (Ledger.GenTx blk -> Mempool.TxSizeInBytes)
  -> InitialMempoolAndModelParams IO blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MempoolWithMockedLedgerItf IO blk)
openMempoolWithMockedLedgerItf capacityOverride tracer txSizeImpl params = do
    -- Set up a backing store with initial values
    sysTmpDir <- getTemporaryDirectory
    tmpDir <- createTempDirectory sysTmpDir "mempool-bench"
    let lbsi = newBackingStoreInitialiser nullTracer bss
        sfhs = SomeHasFS $ ioHasFS $ MountPoint tmpDir
        values = Ledger.projectLedgerTables backingState
    lbs <- newBackingStore lbsi sfhs values

    -- Set up an empty changelog and populate it by applying blocks
    let ldb0 = empty $ Ledger.forgetLedgerTables backingState
    ldb <- onChangelogM (applyThenPushMany
              (const $ pure ())
              ldbcfg
              (fmap ReapplyVal blks)
              (LedgerDB.readKeySets lbs))
              ldb0
    dbVar <- newTVarIO ldb
    -- Create a ledger interface, mimicking @getLedgerTablesAtFor@ from the
    -- @ChainDB.Impl.LgrDB@ module.
    let ledgerItf = Mempool.LedgerInterface {
            Mempool.getCurrentLedgerState = pure $ current $ anchorlessChangelog ldb
          , Mempool.getLedgerTablesAtFor = \pt txs -> do
              let keys = foldMap' Ledger.getTransactionKeySets txs
              Query.getLedgerTablesAtFor pt keys dbVar lbs
          }

    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   lcfg
                   capacityOverride
                   tracer
                   txSizeImpl
    pure MempoolWithMockedLedgerItf {
        getLedgerInterface    = ledgerItf
      , getLedgerDB           = ldb
      , getLedgerBackingStore = lbs
      , getMempool            = mempool
    }
  where
    MempoolAndModelParams {
        immpBackingState         = backingState
      , immpLedgerConfig         = ldbcfg
      , immpBackingStoreSelector = bss
      , immpChangelogBlocks      = blks
      } = params

    lcfg = ledgerDbCfg ldbcfg

addTx ::
     MempoolWithMockedLedgerItf m blk
  -> AddTxOnBehalfOf
  -> Ledger.GenTx blk
  -> m (MempoolAddTxResult blk)
addTx = Mempool.addTx . getMempool

removeTxs ::
     MempoolWithMockedLedgerItf m blk
  -> NE.NonEmpty (Ledger.GenTxId blk)
  -> m ()
removeTxs = Mempool.removeTxs . getMempool

getTxs ::
     (Ledger.LedgerSupportsMempool blk)
  => MempoolWithMockedLedgerItf IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
    snapshotTxs <- fmap Mempool.snapshotTxs $ atomically
                                            $ Mempool.getSnapshot
                                            $ getMempool mockedMempool
    pure $ fmap (Ledger.txForgetValidated . fst) snapshotTxs
