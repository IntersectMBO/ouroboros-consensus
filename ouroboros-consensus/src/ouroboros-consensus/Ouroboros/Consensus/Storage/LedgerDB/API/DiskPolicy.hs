{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Consensus.Storage.LedgerDB.API.DiskPolicy (
    DiskPolicy (..)
  , SnapshotInterval (..)
  , defaultDiskPolicy
  ) where

import           Control.Monad.Class.MonadTime.SI
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Config

-- | Length of time that has to pass after which a snapshot is taken.
data SnapshotInterval =
    DefaultSnapshotInterval
  | RequestedSnapshotInterval DiffTime
  deriving stock (Eq, Generic, Show)

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
    }
  deriving NoThunks via OnlyCheckWhnf DiskPolicy

-- | Default on-disk policy suitable to use with cardano-node
--
defaultDiskPolicy ::
     SecurityParam
  -> SnapshotInterval
  -> DiskPolicy
defaultDiskPolicy
  (SecurityParam k)
  requestedInterval =
    DiskPolicy {
        onDiskNumSnapshots
      , onDiskShouldTakeSnapshot
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
