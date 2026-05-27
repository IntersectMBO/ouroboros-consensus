{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.ChainDB
  ( MinimalChainDbArgs (..)
  , NodeDBs (..)
  , emptyNodeDBs
  , fromMinimalChainDbArgs
  , mkTestChunkInfo
  , testBackendArgs
  , testBackendArgsRoundtrippingSnapshots
  , testBackendArgsWithSnapshots
  ) where

import Codec.Serialise (Serialise (decode, encode))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer (nullTracer)
import Ouroboros.Consensus.Block
  ( HeaderHash
  , WithOrigin (NotOrigin, Origin)
  , pointToWithOriginRealPoint
  , realPointSlot
  , unSlotNo
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig (topLevelConfigLedger)
  , configCodec
  )
import Ouroboros.Consensus.HardFork.History.EraParams (eraEpochSize)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerState (..)
  , ExtStateHandle (..)
  , extLedgerState
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Peras.Params (mkPerasParams)
import Ouroboros.Consensus.Storage.ChainDB hiding
  ( TraceFollowerEvent (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import Ouroboros.Consensus.Storage.ImmutableDB hiding (getTip)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
  ( DiskSnapshot (..)
  , ReadSnapshotErr (..)
  , SnapshotBackend (UTxOHDMemSnapshot)
  , SnapshotFailure (..)
  , SnapshotMetadata (..)
  , TablesCodecVersion (TablesCodecVersion1)
  , readExtLedgerState
  , snapshotToStatePath
  , writeExtLedgerState
  , writeSnapshotMetadata
  )
import qualified Ouroboros.Consensus.Storage.LedgerDB.Snapshots as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( BackendResources (..)
  , LedgerDbBackendArgs (..)
  )
import Ouroboros.Consensus.Storage.PerasCertDB (PerasCertDbArgs (..))
import Ouroboros.Consensus.Storage.PerasVoteDB (PerasVoteDbArgs (..))
import Ouroboros.Consensus.Storage.VolatileDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike hiding (invariant)
import System.FS.API (SomeHasFS (..), createDirectoryIfMissing)
import System.FS.CRC (CRC (..))
import System.FS.Sim.MockFS (MockFS)
import qualified System.FS.Sim.MockFS as Mock
import System.FS.Sim.STM (simHasFS)
import System.Random (mkStdGen)
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
  , mcdbInitLedger :: LedgerTablesFactory m blk -> m (ExtStateHandle m blk)
  -- ^ Continuation producing the initial ledger handle, given the
  -- 'LedgerTablesFactory' the backend supplies. Matches the shape of
  -- 'lgrGenesis'.
  , mcdbBackendArgs :: LedgerDbBackendArgs m blk
  -- ^ The LedgerDB backend (e.g. in-memory or LSM). Provided by the
  -- caller because the test-lib has no knowledge of block-specific
  -- backends.
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
  ( IOLike m
  , LedgerSupportsProtocol blk
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
          { lgrSnapshotPolicyArgs = LedgerDB.defaultSnapshotPolicyArgs
          , -- Keep 2 ledger snapshots, and take a new snapshot at least every 2 *
            -- k seconds, where k is the security parameter.
            lgrGenesis = mcdbInitLedger
          , lgrHasFS = SomeHasFS $ simHasFS (nodeDBsLgr mcdbNodeDBs)
          , lgrTracer = nullTracer
          , lgrConfig = configLedgerDb mcdbTopLevelConfig OmitLedgerEvents
          , lgrBackendArgs = mcdbBackendArgs
          , lgrQueryBatchSize = DefaultQueryBatchSize
          , lgrStartSnapshot = Nothing
          }
    , cdbPerasCertDbArgs =
        PerasCertDbArgs
          { pcdbaTracer = nullTracer
          }
    , cdbPerasVoteDbArgs =
        PerasVoteDbArgs
          { pvdbaTracer = nullTracer
          , pvdbaPerasCfg = mkPerasParams
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
          , cdbsSnapshotDelayRNG = mkStdGen 0
          }
    }

-- | A 'LedgerDbBackendArgs' suitable for ThreadNet-style tests: wraps the
-- caller-supplied 'LedgerTablesFactory' in a 'BackendResources' whose
-- 'brSnapshotManager' / 'brLoadSnapshot' / 'brRelease' are stubs. Tests in
-- this harness never take or load snapshots; if they ever do, those stubs
-- will fail loudly.
testBackendArgs ::
  Monad m =>
  LedgerTablesFactory m blk -> LedgerDbBackendArgs m blk
testBackendArgs ledgerTablesFactory = LedgerDbBackendArgs $ \_tr _shfs ->
  pure
    BackendResources
      { brLoadSnapshot =
          \_cfg _shfs _ds ->
            error "Test.Util.ChainDB.testBackendArgs: brLoadSnapshot is unused in ThreadNet tests"
      , brSnapshotManager =
          \_cfg _tr shfs ->
            LedgerDB.SnapshotManager
              { LedgerDB.listSnapshots = LedgerDB.defaultListSnapshots shfs
              , LedgerDB.deleteSnapshotIfTemporary = \_ds -> pure ()
              , LedgerDB.takeSnapshot = \_suffix _st -> pure Nothing
              }
      , brRelease = pure ()
      , ledgerTablesFactory = ledgerTablesFactory
      }

-- | Variant of 'testBackendArgs' whose 'brSnapshotManager' actually takes
-- snapshots: 'takeSnapshot' writes a placeholder metadata file at the
-- snapshot's directory so 'defaultListSnapshots' / 'defaultDeleteSnapshotIfTemporary'
-- find it.
--
-- Used by snapshot-policy tests (e.g. @Test.Ouroboros.Storage.ChainDB.LedgerSnapshots@)
-- that care about /when/ snapshots are taken, not /what/ data they
-- contain. For trivial-tables blocks ('TestBlock' style) the snapshotted
-- state itself does not need to be serialised.
testBackendArgsWithSnapshots ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLedgerHD m blk
  , IsLedger LedgerState blk
  ) =>
  LedgerTablesFactory m blk -> LedgerDbBackendArgs m blk
testBackendArgsWithSnapshots ledgerTablesFactory = LedgerDbBackendArgs $ \_tr _shfs ->
  pure
    BackendResources
      { brLoadSnapshot =
          \_cfg _shfs _ds ->
            error "Test.Util.ChainDB.testBackendArgsWithSnapshots: brLoadSnapshot is unused"
      , brSnapshotManager = \_cfg _tr shfs@(SomeHasFS hasFS) ->
          LedgerDB.SnapshotManager
            { LedgerDB.listSnapshots = LedgerDB.defaultListSnapshots shfs
            , LedgerDB.deleteSnapshotIfTemporary =
                LedgerDB.defaultDeleteSnapshotIfTemporary shfs nullTracer
            , LedgerDB.takeSnapshot = \suffix st ->
                case pointToWithOriginRealPoint (getTip (extLedgerState st)) of
                  Origin -> pure Nothing
                  NotOrigin t -> do
                    let number = unSlotNo (realPointSlot t)
                        snapshot = DiskSnapshot number suffix
                    existing <- LedgerDB.defaultListSnapshots shfs
                    if snapshot `elem` existing
                      then pure Nothing
                      else do
                        createDirectoryIfMissing hasFS True $
                          LedgerDB.snapshotToDirPath snapshot
                        writeSnapshotMetadata shfs snapshot $
                          SnapshotMetadata
                            { snapshotBackend = UTxOHDMemSnapshot
                            , snapshotChecksum = CRC 0
                            , snapshotTablesCodecVersion = TablesCodecVersion1
                            }
                        pure $ Just (snapshot, t)
            }
      , brRelease = pure ()
      , ledgerTablesFactory = ledgerTablesFactory
      }

-- | Variant of 'testBackendArgsWithSnapshots' that also implements
-- 'brLoadSnapshot' end-to-end: 'takeSnapshot' serialises the whole
-- 'ExtLedgerState' (via 'Serialise') alongside the metadata file, and
-- 'brLoadSnapshot' reads it back, wrapping it via the supplied
-- @mkStateHandle :: LedgerState blk -> StateHandle m blk@.
--
-- Used by state-machine tests (e.g. @Test.Ouroboros.Storage.LedgerDB.StateMachine@)
-- that need snapshots to roundtrip. For trivial-tables blocks the
-- @StateHandle m blk@ is a newtype around 'LedgerState blk' so the
-- caller passes the data constructor directly (e.g. @TestStateHandle@).
testBackendArgsRoundtrippingSnapshots ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLedgerHD m blk
  , IsLedger LedgerState blk
  , Serialise (ExtLedgerState blk)
  , Serialise (HeaderHash blk)
  ) =>
  -- | Wrap a recovered 'LedgerState' as a 'StateHandle'.
  (LedgerState blk -> StateHandle m blk) ->
  LedgerTablesFactory m blk ->
  LedgerDbBackendArgs m blk
testBackendArgsRoundtrippingSnapshots mkStateHandle ledgerTablesFactory =
  LedgerDbBackendArgs $ \_tr _shfs ->
    pure
      BackendResources
        { brLoadSnapshot = \_cfg shfs ds -> do
            (ExtLedgerState ls hs, _crc) <-
              withExceptT (InitFailureRead . ReadSnapshotFailed) $
                readExtLedgerState shfs decode decode (snapshotToStatePath ds)
            case pointToWithOriginRealPoint (getTip ls) of
              Origin -> withExceptT id (throwE' InitFailureGenesis)
              NotOrigin pt ->
                pure (ExtStateHandle (mkStateHandle ls) hs, pt)
        , brSnapshotManager = \_cfg _tr shfs@(SomeHasFS hasFS) ->
            LedgerDB.SnapshotManager
              { LedgerDB.listSnapshots = LedgerDB.defaultListSnapshots shfs
              , LedgerDB.deleteSnapshotIfTemporary =
                  LedgerDB.defaultDeleteSnapshotIfTemporary shfs nullTracer
              , LedgerDB.takeSnapshot = \suffix st ->
                  case pointToWithOriginRealPoint (getTip (extLedgerState st)) of
                    Origin -> pure Nothing
                    NotOrigin t -> do
                      let number = unSlotNo (realPointSlot t)
                          snapshot = DiskSnapshot number suffix
                      existing <- LedgerDB.defaultListSnapshots shfs
                      if snapshot `elem` existing
                        then pure Nothing
                        else do
                          createDirectoryIfMissing hasFS True $
                            LedgerDB.snapshotToDirPath snapshot
                          crc <-
                            writeExtLedgerState
                              shfs
                              encode
                              (snapshotToStatePath snapshot)
                              (extLedgerState st)
                          writeSnapshotMetadata shfs snapshot $
                            SnapshotMetadata
                              { snapshotBackend = UTxOHDMemSnapshot
                              , snapshotChecksum = crc
                              , snapshotTablesCodecVersion = TablesCodecVersion1
                              }
                          pure $ Just (snapshot, t)
              }
        , brRelease = pure ()
        , ledgerTablesFactory = ledgerTablesFactory
        }
 where
  throwE' :: forall e a. e -> ExceptT e m a
  throwE' = ExceptT . pure . Left
