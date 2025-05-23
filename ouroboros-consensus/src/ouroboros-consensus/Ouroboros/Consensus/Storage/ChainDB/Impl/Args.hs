{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl.Args
  ( ChainDbArgs (..)
  , ChainDbSpecificArgs (..)
  , RelativeMountPoint (..)
  , completeChainDbArgs
  , defaultArgs
  , enableLedgerEvents
  , ensureValidateAll
  , updateQueryBatchSize
  , updateSnapshotPolicyArgs
  , updateTracer
  ) where

import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer (Tracer, nullTracer)
import Data.Function ((&))
import Data.Functor.Contravariant ((>$<))
import Data.Kind
import Data.Time.Clock (secondsToDiffTime)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( GetLoEFragment
  , LoE (LoEDisabled)
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
  ( TraceEvent (..)
  )
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB (LedgerDbFlavorArgs)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import System.FS.API

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs f m blk = ChainDbArgs
  { cdbImmDbArgs :: ImmutableDB.ImmutableDbArgs f m blk
  , cdbVolDbArgs :: VolatileDB.VolatileDbArgs f m blk
  , cdbLgrDbArgs :: LedgerDB.LedgerDbArgs f m blk
  , cdbsArgs :: ChainDbSpecificArgs f m blk
  }

-- | Arguments specific to the ChainDB, not to the ImmutableDB, VolatileDB, or
-- LedgerDB.
type ChainDbSpecificArgs ::
  (Type -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
data ChainDbSpecificArgs f m blk = ChainDbSpecificArgs
  { cdbsBlocksToAddSize :: Word
  -- ^ Size of the queue used to store asynchronously added blocks. This
  -- is the maximum number of blocks that could be kept in memory at the
  -- same time when the background thread processing the blocks can't keep
  -- up.
  , cdbsGcDelay :: DiffTime
  -- ^ Delay between copying a block to the ImmutableDB and triggering a
  -- garbage collection for the corresponding slot on the VolatileDB.
  --
  -- The goal of the delay is to ensure that the write to the ImmutableDB
  -- has been flushed to disk before deleting the block from the
  -- VolatileDB, so that a crash won't result in the loss of the block.
  , cdbsGcInterval :: DiffTime
  -- ^ Batch all scheduled GCs so that at most one GC happens every
  -- 'cdbsGcInterval'.
  , cdbsRegistry :: HKD f (ResourceRegistry m)
  , cdbsTracer :: Tracer m (TraceEvent blk)
  , cdbsHasFSGsmDB :: HKD f (SomeHasFS m)
  , cdbsTopLevelConfig :: HKD f (TopLevelConfig blk)
  , -- Limit on Eagerness
    cdbsLoE :: GetLoEFragment m blk
  -- ^ If this is 'LoEEnabled', it contains an action that returns the
  -- current LoE fragment.
  }

-- | Default arguments
--
-- The following fields must still be defined:
--
-- * 'cdbsTracer'
-- * 'cdbsRegistry'
--
-- We a 'cdbsGcDelay' of 60 seconds and a 'cdbsGcInterval' of 10 seconds, this
-- means (see the properties in "Test.Ouroboros.Storage.ChainDB.GcSchedule"):
--
-- * The length of the 'GcSchedule' queue is @<= ⌈gcDelay / gcInterval⌉ + 1@,
--   i.e., @<= 7@.
-- * The overlap (number of blocks in both the VolatileDB and the ImmutableDB)
--   is the number of blocks synced in @gcDelay + gcInterval@ = 70s. E.g, when
--   bulk syncing at 1k-2k blocks/s, this means 70k-140k blocks. During normal
--   operation, we receive 1 block/20s (for Byron /and/ for Shelley), meaning
--   at most 4 blocks.
-- * The unnecessary overlap (the blocks that we haven't GC'ed yet but could
--   have, because of batching) < the number of blocks sync in @gcInterval@.
--   E.g., when syncing at 1k-2k blocks/s, this means 10k-20k blocks. During
--   normal operation, we receive 1 block/20s, meaning at most 1 block.
defaultSpecificArgs :: Monad m => Incomplete ChainDbSpecificArgs m blk
defaultSpecificArgs =
  ChainDbSpecificArgs
    { cdbsBlocksToAddSize = 10
    , cdbsGcDelay = secondsToDiffTime 60
    , cdbsGcInterval = secondsToDiffTime 10
    , cdbsRegistry = noDefault
    , cdbsTracer = nullTracer
    , cdbsHasFSGsmDB = noDefault
    , cdbsTopLevelConfig = noDefault
    , cdbsLoE = pure LoEDisabled
    }

-- | Default arguments
--
-- See 'ImmutableDB.defaultArgs', 'VolatileDB.defaultArgs', 'LgrDB.defaultArgs',
-- and 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs ::
  forall m blk.
  Monad m =>
  Incomplete ChainDbArgs m blk
defaultArgs =
  ChainDbArgs
    ImmutableDB.defaultArgs
    VolatileDB.defaultArgs
    LedgerDB.defaultArgs
    defaultSpecificArgs

ensureValidateAll ::
  ChainDbArgs f m blk ->
  ChainDbArgs f m blk
ensureValidateAll args =
  args
    { cdbImmDbArgs =
        (cdbImmDbArgs args)
          { ImmutableDB.immValidationPolicy = ImmutableDB.ValidateAllChunks
          }
    , cdbVolDbArgs =
        (cdbVolDbArgs args)
          { VolatileDB.volValidationPolicy = VolatileDB.ValidateAll
          }
    }

completeChainDbArgs ::
  forall m blk.
  (ConsensusProtocol (BlockProtocol blk), IOLike m) =>
  ResourceRegistry m ->
  TopLevelConfig blk ->
  -- | Initial ledger
  ExtLedgerState blk ValuesMK ->
  ImmutableDB.ChunkInfo ->
  -- | Check integrity
  (blk -> Bool) ->
  -- | Immutable FS, see 'NodeDatabasePaths'
  (RelativeMountPoint -> SomeHasFS m) ->
  -- | Volatile  FS, see 'NodeDatabasePaths'
  (RelativeMountPoint -> SomeHasFS m) ->
  Complete LedgerDbFlavorArgs m ->
  -- | A set of incomplete arguments, possibly modified wrt @defaultArgs@
  Incomplete ChainDbArgs m blk ->
  Complete ChainDbArgs m blk
completeChainDbArgs
  registry
  cdbsTopLevelConfig
  initLedger
  immChunkInfo
  checkIntegrity
  mkImmFS
  mkVolFS
  flavorArgs
  defArgs =
    defArgs
      { cdbImmDbArgs =
          (cdbImmDbArgs defArgs)
            { ImmutableDB.immChunkInfo
            , ImmutableDB.immCheckIntegrity = checkIntegrity
            , ImmutableDB.immRegistry = registry
            , ImmutableDB.immCodecConfig = configCodec cdbsTopLevelConfig
            , ImmutableDB.immHasFS = mkImmFS $ RelativeMountPoint "immutable"
            }
      , cdbVolDbArgs =
          (cdbVolDbArgs defArgs)
            { VolatileDB.volHasFS = mkVolFS $ RelativeMountPoint "volatile"
            , VolatileDB.volCheckIntegrity = checkIntegrity
            , VolatileDB.volCodecConfig = configCodec cdbsTopLevelConfig
            }
      , cdbLgrDbArgs =
          (cdbLgrDbArgs defArgs)
            { LedgerDB.lgrGenesis = pure initLedger
            , LedgerDB.lgrHasFS = mkVolFS $ RelativeMountPoint "ledger"
            , LedgerDB.lgrConfig =
                LedgerDB.configLedgerDb
                  cdbsTopLevelConfig
                  (LedgerDB.ledgerDbCfgComputeLedgerEvents $ LedgerDB.lgrConfig (cdbLgrDbArgs defArgs))
            , LedgerDB.lgrFlavorArgs = flavorArgs
            , LedgerDB.lgrRegistry = registry
            }
      , cdbsArgs =
          (cdbsArgs defArgs)
            { cdbsRegistry = registry
            , cdbsTopLevelConfig
            , cdbsHasFSGsmDB = mkVolFS $ RelativeMountPoint "gsm"
            }
      }

updateTracer ::
  Tracer m (TraceEvent blk) ->
  ChainDbArgs f m blk ->
  ChainDbArgs f m blk
updateTracer trcr args =
  args
    { cdbImmDbArgs = (cdbImmDbArgs args){ImmutableDB.immTracer = TraceImmutableDBEvent >$< trcr}
    , cdbVolDbArgs = (cdbVolDbArgs args){VolatileDB.volTracer = TraceVolatileDBEvent >$< trcr}
    , cdbLgrDbArgs = (cdbLgrDbArgs args){LedgerDB.lgrTracer = TraceLedgerDBEvent >$< trcr}
    , cdbsArgs = (cdbsArgs args){cdbsTracer = trcr}
    }

updateSnapshotPolicyArgs ::
  SnapshotPolicyArgs ->
  ChainDbArgs f m blk ->
  ChainDbArgs f m blk
updateSnapshotPolicyArgs spa args =
  args{cdbLgrDbArgs = (cdbLgrDbArgs args){LedgerDB.lgrSnapshotPolicyArgs = spa}}

updateQueryBatchSize ::
  LedgerDB.QueryBatchSize ->
  ChainDbArgs f m blk ->
  ChainDbArgs f m blk
updateQueryBatchSize qbs args =
  args{cdbLgrDbArgs = (cdbLgrDbArgs args){LedgerDB.lgrQueryBatchSize = qbs}}

enableLedgerEvents ::
  Complete ChainDbArgs m blk ->
  Complete ChainDbArgs m blk
enableLedgerEvents args =
  args
    { cdbLgrDbArgs =
        (cdbLgrDbArgs args) & \x ->
          x
            { LedgerDB.lgrConfig =
                (LedgerDB.lgrConfig x){LedgerDB.ledgerDbCfgComputeLedgerEvents = ComputeLedgerEvents}
            }
    }

{-------------------------------------------------------------------------------
  Relative mount points
-------------------------------------------------------------------------------}

-- | A relative path for a 'MountPoint'
--
-- The root is determined by context.
newtype RelativeMountPoint = RelativeMountPoint FilePath
