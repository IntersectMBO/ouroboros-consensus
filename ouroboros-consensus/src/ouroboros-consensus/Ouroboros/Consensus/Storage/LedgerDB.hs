{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
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

import Data.Functor.Contravariant ((>$<))
import Data.Word
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
-- import qualified Ouroboros.Consensus.Storage.LedgerDB.V1 as V1
-- import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots as V1

import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ImmutableDB.Stream
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2 as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
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
  , GetBlockKeySets blk
  , LedgerSupportsV2LedgerDB LedgerState blk
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
  m (LedgerDB' m blk, Word64)
openDB
  args
  stream
  replayGoal
  getBlock
  getVolatileSuffix =
    case lgrBackendArgs args of
      -- LedgerDbBackendArgsV1 bss ->
      --   let snapManager = V1.snapshotManager args
      --       initDb =
      --         V1.mkInitDb
      --           args
      --           bss
      --           getBlock
      --           snapManager
      --           getVolatileSuffix
      --    in doOpenDB args initDb snapManager stream replayGoal
      LedgerDbBackendArgsV2 (SomeBackendArgs bArgs) -> do
        res <-
          mkResources
            (Proxy @blk)
            (LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 >$< lgrTracer args)
            bArgs
            (lgrRegistry args)
            (lgrHasFS args)
        let snapManager =
              snapshotManager
                (Proxy @blk)
                res
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig args)
                (LedgerDBSnapshotEvent >$< lgrTracer args)
                (lgrHasFS args)
        let initDb = V2.mkInitDb args getBlock snapManager getVolatileSuffix res
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
