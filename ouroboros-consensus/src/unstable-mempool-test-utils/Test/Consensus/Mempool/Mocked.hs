{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Mempool with a mocked ledger interface
module Test.Consensus.Mempool.Mocked (
    InitialMempoolAndModelParams (..)
    -- * Mempool with a mocked LedgerDB interface
  , MockedMempool (getMempool)
  , openMockedMempool
    -- * Mempool API functions
  , addTx
  , getTxs
  , removeTxs
  ) where

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
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           System.Directory (getTemporaryDirectory)
import           System.FS.API (SomeHasFS (SomeHasFS))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)
import           System.IO.Temp (createTempDirectory)

data MockedMempool m blk = MockedMempool {
      getLedgerInterface :: !(Mempool.LedgerInterface m blk)
    , getDbChangelog     :: !(DbChangelog (LedgerState blk))
    , getBackingStore    :: !(LedgerBackingStore m (LedgerState blk))
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

openMockedMempool ::
     ( Ledger.LedgerSupportsMempool blk
     , Ledger.HasTxId (Ledger.GenTx blk)
     , Ledger.LedgerSupportsProtocol blk
     , Ledger.CanSerializeLedgerTables (LedgerState blk)
     )
  => Mempool.MempoolCapacityBytesOverride
  -> Tracer IO (Mempool.TraceEventMempool blk)
  -> (Ledger.GenTx blk -> Mempool.TxSizeInBytes)
  -> InitialMempoolAndModelParams IO blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MockedMempool IO blk)
openMockedMempool capacityOverride tracer txSizeImpl params = do
    -- Set up a backing store with initial values
    sysTmpDir <- getTemporaryDirectory
    tmpDir <- createTempDirectory sysTmpDir "mempool-bench"
    let sfhs = SomeHasFS $ ioHasFS $ MountPoint tmpDir
        values = Ledger.projectLedgerTables backingState
    lbs <- newBackingStore nullTracer bss sfhs values

    -- Set up an empty changelog and populate it by applying blocks
    let ldb0 = empty $ Ledger.forgetLedgerTables backingState
    ldb <- onChangelogM (applyThenPushMany
              (const $ pure ())
              ldbcfg
              (fmap ReapplyVal blks)
              (readKeySets lbs))
              ldb0

    dbVar <- newTVarIO ldb
    -- Create a ledger interface, mimicking @getLedgerTablesAtFor@ from the
    -- @ChainDB.Impl.LgrDB@ module.
    let ledgerItf = Mempool.LedgerInterface {
            Mempool.getCurrentLedgerState = pure $ current $ anchorlessChangelog ldb
          , Mempool.getLedgerTablesAtFor = \pt txs -> do
              let keys = foldMap' Ledger.getTransactionKeySets txs
              LedgerDB.getLedgerTablesAtFor' pt keys dbVar lbs
          }

    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   lcfg
                   capacityOverride
                   tracer
                   txSizeImpl
    pure MockedMempool {
        getLedgerInterface = ledgerItf
      , getDbChangelog     = ldb
      , getBackingStore    = lbs
      , getMempool         = mempool
    }
  where
    InitialMempoolAndModelParams {
        immpBackingState         = backingState
      , immpLedgerConfig         = ldbcfg
      , immpBackingStoreSelector = bss
      , immpChangelogBlocks      = blks
      } = params

    lcfg = dbChangelogCfg ldbcfg

addTx ::
     MockedMempool m blk
  -> AddTxOnBehalfOf
  -> Ledger.GenTx blk
  -> m (MempoolAddTxResult blk)
addTx = Mempool.addTx . getMempool

removeTxs ::
     MockedMempool m blk
  -> NE.NonEmpty (Ledger.GenTxId blk)
  -> m ()
removeTxs = Mempool.removeTxs . getMempool

getTxs ::
     (Ledger.LedgerSupportsMempool blk)
  => MockedMempool IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
    snapshotTxs <- fmap Mempool.snapshotTxs $ atomically
                                            $ Mempool.getSnapshot
                                            $ getMempool mockedMempool
    pure $ fmap (Ledger.txForgetValidated . fst) snapshotTxs

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Initial parameters for the mempool.
--
-- === Parameters for the ledger interface
--
-- One goal of the mempool parameters is to provide enough information to set up
-- an interface to the ledger database. Setting up a ledger interface requires
-- two main parts of the ledger database: a backing store, and a changelog.
--
-- Which backing store implementation we use is determined by
-- 'immpBackingStoreSelector'. The backing store will be initialised using
-- values from 'immpBackingState'. The changelog keeps track of differences on
-- values that are induced by applying blocks. Each diff in the changelog
-- corresponds to a block. As such, the changelog will be populated by applying
-- blocks from 'immpChangelogBlocks' in sequence to 'immpBackingState'.
--
-- INVARIANT: applying the blocks in 'immpChangelogBlocks' in sequence to
-- 'immpBackingState' should not fail.
--
-- ==== Effect on performance
--
-- How we populate the ledger database with values and differences could affect
-- the performance of mempool operations. To be precise, each time we need a
-- partial ledger state to apply transactions to, we /rewind-read-forward/.
--
-- * Rewind: Rewind keys by determining which slot the tip of the backing store
--   points to.
-- * Read: Read values from the backing store for the rewound keys.
-- * Forward: Forward the read values through the changelog.
--
-- How expensive these steps are depends on how we populate the backing store
-- and changelog. We are not sure if we can estimate the cost of mempool
-- operations on these parameters only, but in general, we suspect that:
--
-- * Reading values succesfully from the backing store incurs extra costs (e.g.,
--   deserialisation and I/O costs), compared to when a value is not found in
--   the backing store.
-- * Forwarding becomes more expensive as the following increase: (i) the number
--   of blocks, and (ii) the size of the diffs induced by blocks.
--
data InitialMempoolAndModelParams m blk = InitialMempoolAndModelParams {
      -- | The values that will be used to initialise a backing store.
      immpBackingState         :: !(LedgerState blk Ledger.ValuesMK)
      -- | Blocks that will be used to populate a changelog.
    , immpChangelogBlocks      :: ![blk]
    , immpLedgerConfig         :: !(LedgerDbCfg (LedgerState blk))
    , immpBackingStoreSelector :: !(BackingStoreSelector m)
    }
