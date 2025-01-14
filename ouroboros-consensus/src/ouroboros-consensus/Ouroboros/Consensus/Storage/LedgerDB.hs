{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LedgerDB (
    -- * API
    module Ouroboros.Consensus.Storage.LedgerDB.API
  , module Ouroboros.Consensus.Storage.LedgerDB.Args
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  , module Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
    -- * Impl
  , openDB
  , openDBInternal
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ImmutableDB.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Storage.LedgerDB.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1 as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2 as V2
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API

openDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , InspectLedger blk
  , HasCallStack
  , HasHardForkHistory blk
  , NoThunks (LedgerTables (LedgerState blk) ValuesMK)
  , NoThunks (LedgerTables (LedgerState blk) SeqDiffMK)
  )
  => Complete LedgerDbArgs m blk
  -- ^ Stateless initializaton arguments
  -> StreamAPI m blk blk
  -- ^ Stream source for blocks.
  --
  -- After reading a snapshot from disk, the ledger DB will be brought up to
  -- date with the tip of this steam of blocks. The corresponding ledger state
  -- can then be used as the starting point for chain selection in the ChainDB
  -- driver.
  -> Point blk
  -- ^ The Replay goal i.e. the tip of the stream of blocks.
  -> ResolveBlock m blk
  -- ^ How to get blocks from the ChainDB
  -> m (LedgerDB' m blk, Word64)
openDB
  args
  stream
  replayGoal
  getBlock = case lgrFlavorArgs args of
    LedgerDbFlavorArgsV1 bss ->
      let initDb = V1.mkInitDb
                       args
                       bss
                       getBlock
        in
          doOpenDB args initDb stream replayGoal
    LedgerDbFlavorArgsV2 bss ->
        let initDb = V2.mkInitDb
                       args
                       bss
                       getBlock
        in
          doOpenDB args initDb stream replayGoal


{-------------------------------------------------------------------------------
  Opening a LedgerDB
-------------------------------------------------------------------------------}

doOpenDB ::
  forall m blk db. ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  )
  => Complete LedgerDbArgs m blk
  -> InitDB db m blk
  -> StreamAPI m blk blk
  -> Point blk
  -> m (LedgerDB' m blk, Word64)
doOpenDB args initDb stream replayGoal =
    f <$> openDBInternal args initDb stream replayGoal
  where f (ldb, replayCounter, _) = (ldb, replayCounter)

-- | Open the ledger DB and expose internals for testing purposes
openDBInternal ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  )
  => Complete LedgerDbArgs m blk
  -> InitDB db m blk
  -> StreamAPI m blk blk
  -> Point blk
  -> m (LedgerDB' m blk, Word64, TestInternals' m blk)
openDBInternal args@(LedgerDbArgs { lgrHasFS = SomeHasFS fs }) initDb stream replayGoal = do
    createDirectoryIfMissing fs True (mkFsPath [])
    (_initLog, db, replayCounter) <-
          initialize
            replayTracer
            snapTracer
            lgrHasFS
            lgrConfig
            stream
            replayGoal
            initDb
            lgrStartSnapshot
            doDiskSnapshotChecksum
    (ledgerDb, internal) <- mkLedgerDb initDb db
    return (ledgerDb, replayCounter, internal)

  where
    LedgerDbArgs {
        lgrConfig
      , lgrTracer
      , lgrHasFS
      , lgrStartSnapshot
      } = args

    replayTracer = LedgerReplayEvent     >$< lgrTracer
    snapTracer   = LedgerDBSnapshotEvent >$< lgrTracer

    SnapshotPolicyArgs _ _ doDiskSnapshotChecksum = lgrSnapshotPolicyArgs args
