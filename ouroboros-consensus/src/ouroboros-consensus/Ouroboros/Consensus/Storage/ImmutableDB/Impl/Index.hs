{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (
    -- * Index
    Index (..)
  , readEntry
  , readOffset
    -- * Cached index
  , CacheConfig (..)
  , cachedIndex
  ) where

import           Control.Tracer (Tracer)
import           Data.Functor.Identity (Identity (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Block (ConvertRawHash, IsEBB, StandardHash)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache
                     (CacheConfig (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache as Cache
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockSize)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
                     (TraceCacheEvent, WithBlockSize (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.FS.API (HasFS)
import           System.FS.API.Types (AllowExisting, Handle)

{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- | Bundle the operations on the primary and secondary index that touch the
-- files. This allows us to easily introduce an intermediary caching layer.
data Index m blk h = Index
  { -- | See 'Primary.readOffsets'
    readOffsets
      :: forall t. (HasCallStack, Traversable t)
      => ChunkNo
      -> t RelativeSlot
      -> m (t (Maybe SecondaryOffset))

    -- |  See 'Primary.readFirstFilledSlot'
  , readFirstFilledSlot
      :: HasCallStack
      => ChunkNo
      -> m (Maybe RelativeSlot)

    -- | See 'Primary.open'
  , openPrimaryIndex
      :: HasCallStack
      => ChunkNo
      -> AllowExisting
      -> m (Handle h)

    -- | See 'Primary.appendOffsets'
  , appendOffsets
      :: forall f. (HasCallStack, Foldable f)
      => Handle h
      -> f SecondaryOffset
      -> m ()

    -- | See 'Secondary.readEntries'
  , readEntries
      :: forall t. (HasCallStack, Traversable t)
      => ChunkNo
      -> t (IsEBB, SecondaryOffset)
      -> m (t (Secondary.Entry blk, BlockSize))

    -- | See 'Secondary.readAllEntries'
  , readAllEntries
      :: HasCallStack
      => SecondaryOffset
      -> ChunkNo
      -> (Secondary.Entry blk -> Bool)
      -> Word64
      -> IsEBB
      -> m [WithBlockSize (Secondary.Entry blk)]

    -- | See 'Secondary.appendEntry'
  , appendEntry
      :: HasCallStack
      => ChunkNo
      -> Handle h
      -> WithBlockSize (Secondary.Entry blk)
      -> m Word64

    -- | Close the index and stop any background threads.
    --
    -- Should be called when the ImmutableDB is closed.
  , close
      :: HasCallStack
      => m ()

    -- | Restart a closed index using the given chunk as the current chunk,
    -- drop all previously cached information.
    --
    -- NOTE: this will only used in the testsuite, when we need to truncate.
  , restart
      :: HasCallStack
      => ChunkNo
      -> m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "Index" (Index m blk h)

-- | See 'Primary.readOffset'.
readOffset ::
     Functor m
  => Index m blk h
  -> ChunkNo
  -> RelativeSlot
  -> m (Maybe SecondaryOffset)
readOffset index chunk slot = runIdentity <$>
    readOffsets index chunk (Identity slot)

-- | See 'Secondary.readEntry'.
readEntry ::
     Functor m
  => Index m blk h
  -> ChunkNo
  -> IsEBB
  -> SecondaryOffset
  -> m (Secondary.Entry blk, BlockSize)
readEntry index chunk isEBB slotOffset = runIdentity <$>
    readEntries index chunk (Identity (isEBB, slotOffset))

{------------------------------------------------------------------------------
  Cached index
------------------------------------------------------------------------------}

-- | Caches the current chunk's indices as well as a number of past chunk's
-- indices.
--
-- Spawns a background thread to expire past chunks from the cache that
-- haven't been used for a while.
cachedIndex ::
     forall m blk h.
     (IOLike m, ConvertRawHash blk, StandardHash blk, Typeable blk)
  => HasFS m h
  -> ResourceRegistry m
  -> Tracer m TraceCacheEvent
  -> CacheConfig
  -> ChunkInfo
  -> ChunkNo  -- ^ Current chunk
  -> m (Index m blk h)
cachedIndex hasFS registry tracer cacheConfig chunkInfo chunk = do
    cacheEnv <- Cache.newEnv
                  hasFS
                  registry
                  tracer
                  cacheConfig
                  chunkInfo
                  chunk
    return Index
      { readOffsets         = Cache.readOffsets         cacheEnv
      , readFirstFilledSlot = Cache.readFirstFilledSlot cacheEnv
      , openPrimaryIndex    = Cache.openPrimaryIndex    cacheEnv
      , appendOffsets       = Cache.appendOffsets       cacheEnv
      , readEntries         = Cache.readEntries         cacheEnv
      , readAllEntries      = Cache.readAllEntries      cacheEnv
      , appendEntry         = Cache.appendEntry         cacheEnv
      , close               = Cache.close               cacheEnv
      , restart             = Cache.restart             cacheEnv
      }
