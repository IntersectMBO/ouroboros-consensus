{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Configuration of the LedgerDB
module Ouroboros.Consensus.Storage.LedgerDB.Config (
    LedgerDbCfg (..)
  , configLedgerDb
    -- * DiskPolicy
  , DiskPolicy (..)
  , FlushFrequency (..)
  , QueryBatchSize (..)
  , SnapCounters (..)
  , SnapshotInterval (..)
  , defaultDiskPolicy
  ) where

import           Control.Monad.Class.MonadTime.SI
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks, OnlyCheckWhnf (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract (LedgerCfg)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  LedgerDB Config
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

configLedgerDb ::
     ConsensusProtocol (BlockProtocol blk)
  => TopLevelConfig blk
  -> LedgerDbCfg (ExtLedgerState blk)
configLedgerDb config = LedgerDbCfg {
      ledgerDbCfgSecParam    = configSecurityParam config
    , ledgerDbCfg            = ExtLedgerCfg config
    }

{-------------------------------------------------------------------------------
 DiskPolicy
-------------------------------------------------------------------------------}

data SnapCounters = SnapCounters {
    -- | When was the last time we made a snapshot
    prevSnapshotTime      :: !(Maybe Time)
    -- | How many blocks have we processed since the last snapshot
  , ntBlocksSinceLastSnap :: !Word64
  }

-- | Length of time, requested by the user, that has to pass after which
-- a snapshot is taken. It can be:
--
-- 1. either explicitly provided by user in seconds
-- 2. or default value can be requested - the specific DiskPolicy determines
--    what that is exactly, see `defaultDiskPolicy` as an example
data SnapshotInterval =
    DefaultSnapshotInterval
  | RequestedSnapshotInterval DiffTime
  deriving stock (Eq, Generic, Show)

-- | The number of diffs in the immutable part of the chain that we have to see
-- before we flush ledger state to disk. See 'onDiskShouldFlush'. It can be:
--
-- 1. either explicitly provided by a user in the number of diffs in the
--    immutable part of the chain.
-- 2. a default value, which is determined by a specific 'DiskPolicy'. See
-- 'defaultDiskPolicy' as an example.
--
-- INVARIANT: Should be at least 0.
data FlushFrequency =
    DefaultFlushFrequency
  | RequestedFlushFrequency Word64
  deriving stock (Show, Eq, Generic)

-- | The number of keys to read /at most/ in a backing store range query, as
-- requested by the user. It can be:
--
-- 1. either explicitly provided by a user in the number of keys to read from disk.
-- 2. a default value, which is determined by a specific 'DiskPolicy'. See
-- 'defaultDiskPolicy' as an example.
--
-- INVARIANT: Should be at least 1.
--
-- It is fine if the result of a range read contains less than this number of
-- keys, but it should never return more.
data QueryBatchSize =
    DefaultQueryBatchSize
  | RequestedQueryBatchSize Word64
  deriving stock (Show, Eq, Generic)

-- | On-disk policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data DiskPolicy = DiskPolicy {
      -- | How many snapshots do we want to keep on disk?
      --
      -- A higher number of on-disk snapshots is primarily a safe-guard against
      -- disk corruption: it trades disk space for reliability.
      --
      -- Examples:
      --
      -- * @0@: Delete the snapshot immediately after writing.
      --        Probably not a useful value :-D
      -- * @1@: Delete the previous snapshot immediately after writing the next
      --        Dangerous policy: if for some reason the deletion happens before
      --        the new snapshot is written entirely to disk (we don't @fsync@),
      --        we have no choice but to start at the genesis snapshot on the
      --        next startup.
      -- * @2@: Always keep 2 snapshots around. This means that when we write
      --        the next snapshot, we delete the oldest one, leaving the middle
      --        one available in case of truncation of the write. This is
      --        probably a sane value in most circumstances.
      onDiskNumSnapshots       :: Word

      -- | Should we write a snapshot of the ledger state to disk?
      --
      -- This function is passed two bits of information:
      --
      -- * The time since the last snapshot, or 'NoSnapshotTakenYet' if none was taken yet.
      --   Note that 'NoSnapshotTakenYet' merely means no snapshot had been taking yet
      --   since the node was started; it does not necessarily mean that none
      --   exist on disk.
      --
      -- * The distance in terms of blocks applied to the /oldest/ ledger
      --   snapshot in memory. During normal operation, this is the number of
      --   blocks written to the ImmutableDB since the last snapshot. On
      --   startup, it is computed by counting how many immutable blocks we had
      --   to reapply to get to the chain tip. This is useful, as it allows the
      --   policy to decide to take a snapshot /on node startup/ if a lot of
      --   blocks had to be replayed.
      --
      -- See also 'defaultDiskPolicy'
    , onDiskShouldTakeSnapshot :: Maybe DiffTime -> Word64 -> Bool

      -- | Based on the current length of the diff sequence in the
      -- 'DbChangelog', decide whether we should flush to the 'BackingStore'
      -- (note that we will always keep @k@ states, but we will keep more
      -- differences that have not yet been flushed to the disk).
      --
      -- Flushing means only applying part of the diffs to the backing store, in
      -- particular we /don't/ serialize the ledger state.
    , onDiskShouldFlush        :: Word64 -> Bool

      -- | Size of a batch of lookups used in range queries for ledger tables.
    , onDiskQueryBatchSize     :: Word64
    }
  deriving NoThunks via OnlyCheckWhnf DiskPolicy

-- | Default on-disk policy suitable to use with cardano-node
--
defaultDiskPolicy ::
     SecurityParam
  -> SnapshotInterval
  -> FlushFrequency
  -> QueryBatchSize
  -> DiskPolicy
defaultDiskPolicy
  (SecurityParam k)
  requestedInterval
  requestedFlushFrequency
  requestedQueryBatchSize =
    DiskPolicy {
        onDiskNumSnapshots
      , onDiskShouldTakeSnapshot
      , onDiskShouldFlush
      , onDiskQueryBatchSize
      }
  where
    onDiskNumSnapshots :: Word
    onDiskNumSnapshots = 2

    onDiskShouldTakeSnapshot ::
         Maybe DiffTime
      -> Word64
      -> Bool
    onDiskShouldTakeSnapshot Nothing blocksSinceLast =
      -- If users never leave their wallet running for long, this would mean
      -- that under some circumstances we would never take a snapshot
      -- So, on startup (when the 'time since the last snapshot' is `Nothing`),
      -- we take a snapshot as soon as there are @k@ blocks replayed.
      -- This means that even if users frequently shut down their wallet, we still
      -- take a snapshot roughly every @k@ blocks. It does mean the possibility of
      -- an extra unnecessary snapshot during syncing (if the node is restarted), but
      -- that is not a big deal.
      blocksSinceLast >= k

    onDiskShouldTakeSnapshot (Just timeSinceLast) blocksSinceLast =
         timeSinceLast >= snapshotInterval
      || substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast

    -- | We want to create a snapshot after a substantial amount of blocks were
    -- processed (hard-coded to 50k blocks). Given the fact that during bootstrap
    -- a fresh node will see a lot of blocks over a short period of time, we want
    -- to limit this condition to happen not more often then a fixed amount of
    -- time (here hard-coded to 6 minutes)
    substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast =
      let minBlocksBeforeSnapshot      = 50_000
          minTimeBeforeSnapshot        = 6 * secondsToDiffTime 60
      in    blocksSinceLast >= minBlocksBeforeSnapshot
         && timeSinceLast   >= minTimeBeforeSnapshot

    -- | Requested snapshot interval can be explicitly provided by the
    -- caller (RequestedSnapshotInterval) or the caller might request the default
    -- snapshot interval (DefaultSnapshotInterval). If the latter then the
    -- snapshot interval is defaulted to k * 2 seconds - when @k = 2160@ the interval
    -- defaults to 72 minutes.
    snapshotInterval = case requestedInterval of
      RequestedSnapshotInterval value -> value
      DefaultSnapshotInterval           -> secondsToDiffTime $ fromIntegral $ k * 2

    onDiskShouldFlush = case requestedFlushFrequency of
      RequestedFlushFrequency value -> (>= value)
      DefaultFlushFrequency         -> (>= 100)

    onDiskQueryBatchSize = case requestedQueryBatchSize of
      RequestedQueryBatchSize value -> value
      DefaultQueryBatchSize         -> 100_000
