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
import qualified Database.LSMTree as LSM hiding (deleteSnapshot)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ImmutableDB.Stream
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1 as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2 as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.BlockIO.API
import System.FS.BlockIO.IO
import System.FS.IO
import System.Random

openDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , InspectLedger blk
  , HasCallStack
  , HasHardForkHistory blk
  , LedgerSupportsLedgerDB blk
  , LSM.GoodForLSM (LedgerState blk)
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
      let initDb =
            V1.mkInitDb
              args
              bss
              getBlock
       in doOpenDB args defaultDeleteSnapshot initDb stream replayGoal
    LedgerDbFlavorArgsV2 bss -> do
      (ds, bss') <- case bss of
        V2.V2Args V2.InMemoryHandleArgs -> pure (defaultDeleteSnapshot, V2.InMemoryHandleEnv)
        V2.V2Args (V2.LSMHandleArgs path) -> do
          session <-
            snd
              <$> allocate
                (lgrRegistry args)
                ( \_ -> do
                    hasBlockIO <- ioHasBlockIO (lgrHasFS args) defaultIOCtxParams
                    salt <- fst . genWord64 <$> initStdGen
                    LSM.openSession
                      (LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 . V2.LSMTrace >$< lgrTracer args)
                      (lgrHasFS args)
                      hasBlockIO
                      salt
                      (mkFsPath [path])
                )
                LSM.closeSession
          pure (LSM.deleteSnapshot session, V2.LSMHandleEnv session)
      let initDb =
            V2.mkInitDb
              args
              bss'
              getBlock
      doOpenDB args ds initDb stream replayGoal

{-------------------------------------------------------------------------------
  Opening a LedgerDB
-------------------------------------------------------------------------------}

doOpenDB ::
  forall m blk db.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  ) =>
  Complete LedgerDbArgs m blk ->
  (SomeHasFS m -> DiskSnapshot -> m ()) ->
  InitDB db m blk ->
  StreamAPI m blk blk ->
  Point blk ->
  m (LedgerDB' m blk, Word64)
doOpenDB args deleteSnapshot initDb stream replayGoal =
  f <$> openDBInternal args deleteSnapshot initDb stream replayGoal
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
  (SomeHasFS m -> DiskSnapshot -> m ()) ->
  InitDB db m blk ->
  StreamAPI m blk blk ->
  Point blk ->
  m (LedgerDB' m blk, Word64, TestInternals' m blk)
openDBInternal args@(LedgerDbArgs{lgrHasFS = SomeHasFS fs}) deleteSnapshot initDb stream replayGoal = do
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
      deleteSnapshot
  (ledgerDb, internal) <- mkLedgerDb initDb db
  return (ledgerDb, replayCounter, internal)
 where
  LedgerDbArgs
    { lgrConfig
    , lgrTracer
    , lgrHasFS
    , lgrStartSnapshot
    } = args

  replayTracer = LedgerReplayEvent >$< lgrTracer
  snapTracer = LedgerDBSnapshotEvent >$< lgrTracer
