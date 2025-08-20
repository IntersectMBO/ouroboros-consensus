{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LedgerDB
  ( -- * API
    module Ouroboros.Consensus.Storage.LedgerDB.API
  , module Ouroboros.Consensus.Storage.LedgerDB.Args
  , module Ouroboros.Consensus.Storage.LedgerDB.Forker
  , module Ouroboros.Consensus.Storage.LedgerDB.TraceEvent

    -- * Impl
  , openDB
  , openDBInternal
  ) where

import Control.ResourceRegistry
import Data.Functor.Contravariant ((>$<))
import Data.Word
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ImmutableDB.Stream
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1 as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2 as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import System.FS.API

openDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  , HasHardForkHistory blk
  , LedgerSupportsLedgerDB blk
  ) =>
  -- | Stateless initializaton arguments
  Complete LedgerDbArgs m blk ->
  -- | Stream source for blocks.
  --
  -- After reading a snapshot from disk, the ledger DB will be brought up to
  -- date with the tip of this steam of blocks. The corresponding ledger state
  -- can then be used as the starting point for chain selection in the ChainDB
  -- driver.
  StreamAPI m blk blk ->
  -- | The Replay goal i.e. the tip of the stream of blocks.
  Point blk ->
  -- | How to get blocks from the ChainDB
  ResolveBlock m blk ->
  m (LedgerDB' m blk, Word64)
openDB
  args
  stream
  replayGoal
  getBlock = case lgrFlavorArgs args of
    LedgerDbFlavorArgsV1 bss ->
      let snapManager = V1.snapshotManager args
          initDb =
            V1.mkInitDb
              args
              bss
              getBlock
              snapManager
       in doOpenDB args initDb snapManager stream replayGoal
    LedgerDbFlavorArgsV2 bss -> do
      (snapManager, bss') <- case bss of
        V2.V2Args V2.InMemoryHandleArgs -> pure (InMemory.snapshotManager args, V2.InMemoryHandleEnv)
        V2.V2Args (V2.LSMHandleArgs (V2.LSMArgs path salt mkFS)) -> do
          (rk1, V2.SomeHasFSAndBlockIO fs blockio) <- mkFS (lgrRegistry args)
          session <-
            allocate
              (lgrRegistry args)
              ( \_ ->
                  LSM.openSession
                    (LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 . V2.LSMTrace >$< lgrTracer args)
                    fs
                    blockio
                    salt
                    path
              )
              LSM.closeSession
          pure
            ( LSM.snapshotManager (snd session) args
            , V2.LSMHandleEnv (V2.LSMResources (fst session) (snd session) rk1)
            )
      let initDb =
            V2.mkInitDb
              args
              bss'
              getBlock
              snapManager
      doOpenDB args initDb snapManager stream replayGoal

{-------------------------------------------------------------------------------
  Opening a LedgerDB
-------------------------------------------------------------------------------}

doOpenDB ::
  forall m n blk db st.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  ) =>
  Complete LedgerDbArgs m blk ->
  InitDB db m blk ->
  SnapshotManager m n blk st ->
  StreamAPI m blk blk ->
  Point blk ->
  m (LedgerDB' m blk, Word64)
doOpenDB args initDb snapManager stream replayGoal =
  f <$> openDBInternal args initDb snapManager stream replayGoal
 where
  f (ldb, replayCounter, _) = (ldb, replayCounter)

-- | Open the ledger DB and expose internals for testing purposes
openDBInternal ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  ) =>
  Complete LedgerDbArgs m blk ->
  InitDB db m blk ->
  SnapshotManager m n blk st ->
  StreamAPI m blk blk ->
  Point blk ->
  m (LedgerDB' m blk, Word64, TestInternals' m blk)
openDBInternal args@(LedgerDbArgs{lgrHasFS = SomeHasFS fs}) initDb snapManager stream replayGoal = do
  createDirectoryIfMissing fs True (mkFsPath [])
  (_initLog, db, replayCounter) <-
    initialize
      replayTracer
      snapTracer
      lgrConfig
      stream
      replayGoal
      initDb
      snapManager
      lgrStartSnapshot
  (ledgerDb, internal) <- mkLedgerDb initDb db
  return (ledgerDb, replayCounter, internal)
 where
  LedgerDbArgs
    { lgrConfig
    , lgrTracer
    , lgrStartSnapshot
    } = args

  replayTracer = LedgerReplayEvent >$< lgrTracer
  snapTracer = LedgerDBSnapshotEvent >$< lgrTracer
