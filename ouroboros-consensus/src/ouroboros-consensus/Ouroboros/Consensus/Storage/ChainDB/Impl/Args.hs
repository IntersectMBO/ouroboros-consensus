{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl.Args (
    ChainDbArgs (..)
  , ChainDbSpecificArgs (..)
  , RelativeMountPoint (..)
  , completeChainDbArgs
  , defaultArgs
  , ensureValidateAll
  , updateSnapshotInterval
  , updateTracer
  ) where

import           Control.Tracer (Tracer, nullTracer)
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind
import           Data.Time.Clock (secondsToDiffTime)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceEvent (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.API.Config as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
                     (LedgerDbFlavorArgs)
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Args as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           System.FS.API

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs f m blk = ChainDbArgs {
    cdbImmDbArgs :: ImmutableDB.ImmutableDbArgs f m blk
  , cdbVolDbArgs :: VolatileDB.VolatileDbArgs f m blk
  , cdbLgrDbArgs :: LedgerDB.LedgerDbArgs f m blk
  , cdbsArgs     :: ChainDbSpecificArgs f m blk
  }

-- | Arguments specific to the ChainDB, not to the ImmutableDB, VolatileDB, or
-- LedgerDB.
type ChainDbSpecificArgs ::
     (Type -> Type)
  -> (Type -> Type)
  -> Type
  -> Type
data ChainDbSpecificArgs f m blk = ChainDbSpecificArgs {
      cdbsBlocksToAddSize :: Word
      -- ^ Size of the queue used to store asynchronously added blocks. This
      -- is the maximum number of blocks that could be kept in memory at the
      -- same time when the background thread processing the blocks can't keep
      -- up.
    , cdbsCheckInFuture   :: HKD f (CheckInFuture m blk)
    , cdbsGcDelay         :: DiffTime
      -- ^ Delay between copying a block to the ImmutableDB and triggering a
      -- garbage collection for the corresponding slot on the VolatileDB.
      --
      -- The goal of the delay is to ensure that the write to the ImmutableDB
      -- has been flushed to disk before deleting the block from the
      -- VolatileDB, so that a crash won't result in the loss of the block.
    , cdbsGcInterval      :: DiffTime
      -- ^ Batch all scheduled GCs so that at most one GC happens every
      -- 'cdbsGcInterval'.
    , cdbsRegistry        :: HKD f (ResourceRegistry m)
    , cdbsTracer          :: Tracer m (TraceEvent blk)
    , cdbsTopLevelConfig  :: HKD f (TopLevelConfig blk)
    }

-- | Default arguments
--
-- The following fields must still be defined:
--
-- * 'cdbsTracer'
-- * 'cdbsRegistry'
-- * 'cdbsCheckInFuture'
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
defaultSpecificArgs = ChainDbSpecificArgs {
      cdbsBlocksToAddSize = 10
    , cdbsCheckInFuture   = NoDefault
    , cdbsGcDelay         = secondsToDiffTime 60
    , cdbsGcInterval      = secondsToDiffTime 10
    , cdbsRegistry        = NoDefault
    , cdbsTracer          = nullTracer
    , cdbsTopLevelConfig  = NoDefault
    }

-- | Default arguments
--
-- See 'ImmutableDB.defaultArgs', 'VolatileDB.defaultArgs', 'LgrDB.defaultArgs',
-- and 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs ::
     forall m blk .
     Monad m
  => Incomplete ChainDbArgs m blk
defaultArgs =
   ChainDbArgs ImmutableDB.defaultArgs
               VolatileDB.defaultArgs
               LedgerDB.defaultArgs
               defaultSpecificArgs

ensureValidateAll ::
     ChainDbArgs f m blk
  -> ChainDbArgs f m blk
ensureValidateAll args =
  args { cdbImmDbArgs = (cdbImmDbArgs args) {
           ImmutableDB.immValidationPolicy = ImmutableDB.ValidateAllChunks
           }
       , cdbVolDbArgs = (cdbVolDbArgs args) {
           VolatileDB.volValidationPolicy = VolatileDB.ValidateAll
           }
       }

completeChainDbArgs
  :: forall m blk. (ConsensusProtocol (BlockProtocol blk), IOLike m)
  => ResourceRegistry m
  -> CheckInFuture m blk
  -> TopLevelConfig blk
  -> ExtLedgerState blk ValuesMK
     -- ^ Initial ledger
  -> ImmutableDB.ChunkInfo
  -> (blk -> Bool)
     -- ^ Check integrity
  -> (RelativeMountPoint -> SomeHasFS m)
  -> Complete LedgerDbFlavorArgs m
  -> Incomplete ChainDbArgs m blk
     -- ^ A set of incomplete arguments, possibly modified wrt @defaultArgs@
  -> Complete ChainDbArgs m blk
completeChainDbArgs
  registry
  cdbsCheckInFuture
  cdbsTopLevelConfig
  initLedger
  immChunkInfo
  checkIntegrity
  mkFS
  flavorArgs
  defArgs
  = defArgs {
      cdbImmDbArgs = (cdbImmDbArgs defArgs) {
            ImmutableDB.immChunkInfo
          , ImmutableDB.immCheckIntegrity = checkIntegrity
          , ImmutableDB.immRegistry       = registry
          , ImmutableDB.immCodecConfig    = configCodec cdbsTopLevelConfig
          , ImmutableDB.immHasFS          = mkFS $ RelativeMountPoint "immutable"
          }
      , cdbVolDbArgs = (cdbVolDbArgs defArgs) {
            VolatileDB.volHasFS          = mkFS $ RelativeMountPoint "volatile"
          , VolatileDB.volCheckIntegrity = checkIntegrity
          , VolatileDB.volCodecConfig    = configCodec cdbsTopLevelConfig
          }
      , cdbLgrDbArgs = (cdbLgrDbArgs defArgs) {
            LedgerDB.lgrGenesis    = pure initLedger
          , LedgerDB.lgrHasFS      = mkFS $ RelativeMountPoint "ledger"
          , LedgerDB.lgrConfig     = LedgerDB.configLedgerDb cdbsTopLevelConfig
          , LedgerDB.lgrFlavorArgs = flavorArgs
          , LedgerDB.lgrRegistry   = registry
          }
      , cdbsArgs = (cdbsArgs defArgs) {
            cdbsCheckInFuture
          , cdbsRegistry       = registry
          , cdbsTopLevelConfig
          }
      }

updateTracer ::
  Tracer m (TraceEvent blk)
  -> ChainDbArgs f m blk
  -> ChainDbArgs f m blk
updateTracer trcr args =
  args {
      cdbImmDbArgs = (cdbImmDbArgs args) { ImmutableDB.immTracer = TraceImmutableDBEvent >$< trcr }
    , cdbVolDbArgs = (cdbVolDbArgs args) { VolatileDB.volTracer  = TraceVolatileDBEvent  >$< trcr }
    , cdbLgrDbArgs = (cdbLgrDbArgs args) { LedgerDB.lgrTracer    = TraceLedgerDBEvent    >$< trcr }
    , cdbsArgs     = (cdbsArgs args)     { cdbsTracer            =                           trcr }
  }

updateSnapshotInterval ::
  SnapshotInterval
  -> ChainDbArgs f m blk
  -> ChainDbArgs f m blk
updateSnapshotInterval si args = args { cdbLgrDbArgs = (cdbLgrDbArgs args) { LedgerDB.lgrSnapshotInterval = si } }

{-------------------------------------------------------------------------------
  Relative mount points
-------------------------------------------------------------------------------}

-- | A relative path for a 'MountPoint'
--
-- The root is determined by context.
newtype RelativeMountPoint = RelativeMountPoint FilePath
