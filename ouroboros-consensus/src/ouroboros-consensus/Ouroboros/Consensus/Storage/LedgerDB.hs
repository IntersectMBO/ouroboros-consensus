{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Monad.Trans.Class
import Control.ResourceRegistry
import Data.Functor.Contravariant ((>$<))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Extended
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
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import System.FS.API

-- | Open the LedgerDB database
--
-- It's crucial that this is scoped within the same 'runWithTempRegistry' call
-- that includes the allocation of the ChainDB itself into the top-level
-- resource registry. That's why the whole 'openDB' function is in
-- WithTempRegistry even though there's just the one part of it that actually
-- puts stuff in that registry.
openDB ::
  forall m blk st.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  , HasHardForkHistory blk
  , LedgerDbSerialiseConstraints blk
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
  GetVolatileSuffix m blk ->
  WithTempRegistry st m (LedgerDB' m blk)
openDB
  args
  stream
  replayGoal
  getBlock
  getVolatileSuffix =
    case lgrBackendArgs args of
      LedgerDbBackendArgsV1 bss ->
        let snapManager = V1.snapshotManager args
            initDb =
              V1.mkInitDb
                args
                bss
                getBlock
                snapManager
                getVolatileSuffix
         in lift $ doOpenDB args initDb snapManager stream replayGoal
      LedgerDbBackendArgsV2 (SomeBackendArgs bArgs) -> do
        -- Note this is the only step that cares about the temporary
        -- registry. Note also that the final state is an polymorphic and
        -- unconstrained 'st' so it is clear that this function will allocate
        -- resources with 'impossibleToNotTransfer'.
        res <-
          mkResources
            (Proxy @blk)
            (LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 >$< lgrTracer args)
            bArgs
            (lgrHasFS args)
        let snapManager =
              snapshotManager
                (Proxy @blk)
                res
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig args)
                snapTracer
                (lgrHasFS args)
        let initDb = V2.mkInitDb args getBlock snapManager getVolatileSuffix res
        lift $ doOpenDB args initDb snapManager stream replayGoal
       where
        !tr = lgrTracer args
        !snapTracer = LedgerDBSnapshotEvent >$< tr

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
  m (LedgerDB' m blk)
doOpenDB args initDb snapManager stream replayGoal =
  fst <$> openDBInternal args initDb snapManager stream replayGoal

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
  m (LedgerDB' m blk, TestInternals' m blk)
openDBInternal args@(LedgerDbArgs{lgrHasFS = SomeHasFS fs}) initDb snapManager stream replayGoal = do
  createDirectoryIfMissing fs True (mkFsPath [])
  (_initLog, db) <-
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
  return (ledgerDb, internal)
 where
  LedgerDbArgs
    { lgrConfig
    , lgrTracer
    , lgrStartSnapshot
    } = args

  replayTracer = LedgerReplayEvent >$< lgrTracer
  snapTracer = LedgerDBSnapshotEvent >$< lgrTracer
