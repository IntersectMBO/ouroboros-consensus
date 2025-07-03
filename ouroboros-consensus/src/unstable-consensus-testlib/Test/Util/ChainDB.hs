{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Util.ChainDB
  ( MinimalChainDbArgs (..)
  , NodeDBs (..)
  , emptyNodeDBs
  , fromMinimalChainDbArgs
  , mkTestChunkInfo
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer (nullTracer)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Config
  ( TopLevelConfig (topLevelConfigLedger)
  , configCodec
  )
import Ouroboros.Consensus.HardFork.History.EraParams (eraEpochSize)
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB hiding
  ( TraceFollowerEvent (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import Ouroboros.Consensus.Storage.ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Snapshots as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args
import Ouroboros.Consensus.Storage.PerasCertDB (PerasCertDbArgs (..))
import Ouroboros.Consensus.Storage.VolatileDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike hiding (invariant)
import System.FS.API (SomeHasFS (..))
import System.FS.Sim.MockFS
import qualified System.FS.Sim.MockFS as Mock
import System.FS.Sim.STM (simHasFS)
import Test.Util.Orphans.NoThunks ()
import Test.Util.TestBlock (TestBlock, TestBlockLedgerConfig (..))

-- | A vector with an element for each database of a node
--
-- The @db@ type parameter is instantiated by this module at types for mock
-- filesystems; either the 'MockFS' type or reference cells thereof.
data NodeDBs db = NodeDBs
  { nodeDBsImm :: db
  , nodeDBsVol :: db
  , nodeDBsLgr :: db
  , nodeDBsGsm :: db
  }
  deriving (Functor, Foldable, Traversable)

emptyNodeDBs :: MonadSTM m => m (NodeDBs (StrictTMVar m MockFS))
emptyNodeDBs =
  atomically $
    NodeDBs
      <$> newTMVar Mock.empty
      <*> newTMVar Mock.empty
      <*> newTMVar Mock.empty
      <*> newTMVar Mock.empty

-- | Minimal set of arguments for creating a ChainDB instance for testing purposes.
data MinimalChainDbArgs m blk = MinimalChainDbArgs
  { mcdbTopLevelConfig :: TopLevelConfig blk
  , mcdbChunkInfo :: ImmutableDB.ChunkInfo
  -- ^ Specifies the layout of the ImmutableDB on disk.
  , mcdbInitLedger :: ExtLedgerState blk ValuesMK
  -- ^ The initial ledger state.
  , mcdbRegistry :: ResourceRegistry m
  -- ^ Keeps track of non-lexically scoped resources.
  , mcdbNodeDBs :: NodeDBs (StrictTMVar m MockFS)
  -- ^ File systems underlying the immutable, volatile and ledger databases.
  -- Would be useful to default this to StrictTMVar's containing empty MockFS's.
  }

-- | Utility function to get a default chunk info in case we have EraParams available.
mkTestChunkInfo :: TopLevelConfig TestBlock -> ImmutableDB.ChunkInfo
mkTestChunkInfo = simpleChunkInfo . eraEpochSize . tblcHardForkParams . topLevelConfigLedger

-- | Creates a default set of of arguments for ChainDB tests.
fromMinimalChainDbArgs ::
  ( MonadThrow m
  , MonadSTM m
  , ConsensusProtocol (BlockProtocol blk)
  , PrimMonad m
  ) =>
  MinimalChainDbArgs m blk -> Complete ChainDbArgs m blk
fromMinimalChainDbArgs MinimalChainDbArgs{..} =
  ChainDbArgs
    { cdbImmDbArgs =
        ImmutableDbArgs
          { immCacheConfig = ImmutableDB.CacheConfig 2 60
          , -- Cache at most 2 chunks and expire each chunk after 60 seconds of
            -- being unused.
            immCheckIntegrity = const True
          , -- Getting a verified block component does not do any integrity
            -- checking, both for the ImmutableDB, as the VolatileDB. This is
            -- done in @extractBlockComponent@ in the iterator for the
            -- ImmutableDB, and in @getBlockComponent@ for the VolatileDB.
            immChunkInfo = mcdbChunkInfo
          , immHasFS = SomeHasFS $ simHasFS (nodeDBsImm mcdbNodeDBs)
          , immRegistry = mcdbRegistry
          , immTracer = nullTracer
          , immCodecConfig = configCodec mcdbTopLevelConfig
          , immValidationPolicy = ImmutableDB.ValidateAllChunks
          }
    , cdbVolDbArgs =
        VolatileDbArgs
          { volCheckIntegrity = const True
          , volCodecConfig = configCodec mcdbTopLevelConfig
          , volHasFS = SomeHasFS $ simHasFS (nodeDBsVol mcdbNodeDBs)
          , volMaxBlocksPerFile = VolatileDB.mkBlocksPerFile 4
          , volTracer = nullTracer
          , volValidationPolicy = VolatileDB.ValidateAll
          }
    , cdbLgrDbArgs =
        LedgerDbArgs
          { lgrSnapshotPolicyArgs =
              LedgerDB.SnapshotPolicyArgs
                LedgerDB.DefaultSnapshotInterval
                LedgerDB.DefaultNumOfDiskSnapshots
          , -- Keep 2 ledger snapshots, and take a new snapshot at least every 2 *
            -- k seconds, where k is the security parameter.
            lgrGenesis = return mcdbInitLedger
          , lgrHasFS = SomeHasFS $ simHasFS (nodeDBsLgr mcdbNodeDBs)
          , lgrTracer = nullTracer
          , lgrRegistry = mcdbRegistry
          , lgrConfig = configLedgerDb mcdbTopLevelConfig OmitLedgerEvents
          , lgrFlavorArgs = LedgerDbFlavorArgsV2 (V2Args InMemoryHandleArgs)
          , lgrQueryBatchSize = DefaultQueryBatchSize
          , lgrStartSnapshot = Nothing
          }
    , cdbPerasCertDbArgs =
        PerasCertDbArgs
          { pcdbaTracer = nullTracer
          }
    , cdbsArgs =
        ChainDbSpecificArgs
          { cdbsBlocksToAddSize = 1
          , cdbsGcDelay = 1
          , cdbsHasFSGsmDB = SomeHasFS $ simHasFS (nodeDBsGsm mcdbNodeDBs)
          , cdbsGcInterval = 1
          , cdbsRegistry = mcdbRegistry
          , cdbsTracer = nullTracer
          , cdbsTopLevelConfig = mcdbTopLevelConfig
          , cdbsLoE = pure LoEDisabled
          }
    }
