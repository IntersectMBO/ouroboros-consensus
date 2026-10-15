{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What the node relays to its downstream peers over LeiosNotify.
--
-- The node keeps one log of notifications, in the order it accepted them, and
-- every LeiosNotify server reads that log through its own 'NotifyCursor'. So a
-- notification is stored once however many peers it goes to, and a peer that
-- requests slowly merely lags behind in the log, rather than piling up a queue
-- of its own.
--
-- The log holds only what is worth relaying, and only while it is: whoever
-- appends an entry has already decided it is worth relaying, and the pruner
-- drops it once it no longer is. A peer that connects late starts at the log's
-- oldest entry, so it still receives everything worth relaying. A cursor that
-- falls behind pruning merely skips what was pruned.
module Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyCursor
  , NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  , newNotifyLog
  , nextNotifyLog
  , notifyLogPruner
  , notifyLogStart
  , pruneNotifyLog
  ) where

import Control.Monad (forever, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader
  , maxAnnouncementAgeSend
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadDelay (threadDelay)
  , STM
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVar
  , retry
  , writeTVar
  )

-- | A notification the node relays.
data NotifyEntry blk
  = -- | An announcement, with its election slot's wall-clock onset.
    NotifyAnnouncement !RelativeTime !(AnnouncingHeader blk)
  | -- | A vote, with the end of its window, when it stops being relayed.
    NotifyVote !RelativeTime !LeiosVote
  -- TODO: add offers
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (NotifyEntry blk)

-- | The node's log of notifications to relay.
data NotifyLog m blk = MkNotifyLog
  { notifyEntries :: !(StrictTVar m (Map NotifyCursor (NotifyEntry blk)))
  -- ^ Each entry, at the position it was appended at.
  , notifyEnd :: !(StrictTVar m NotifyCursor)
  -- ^ Where the next entry will be appended.
  }

-- | A position in the log, which only ever grows.
newtype NotifyCursor = MkNotifyCursor Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype NoThunks

-- | Where a downstream peer starts reading: before every entry, including the
-- ones already pruned.
notifyLogStart :: NotifyCursor
notifyLogStart = MkNotifyCursor 0

newNotifyLog :: (IOLike m, NoThunks (Header blk)) => m (NotifyLog m blk)
newNotifyLog = MkNotifyLog <$> newTVarIO Map.empty <*> newTVarIO notifyLogStart

appendNotifyLog :: IOLike m => NotifyLog m blk -> NotifyEntry blk -> STM m ()
appendNotifyLog notifyLog entry = do
  end <- readTVar (notifyEnd notifyLog)
  modifyTVar (notifyEntries notifyLog) (Map.insert end entry)
  writeTVar (notifyEnd notifyLog) (succCursor end)

-- | The first entry at or after the cursor, and the cursor just past it.
--
-- Retries until there is such an entry.
nextNotifyLog ::
  IOLike m =>
  NotifyLog m blk ->
  NotifyCursor ->
  STM m (NotifyEntry blk, NotifyCursor)
nextNotifyLog notifyLog cursor = do
  entries <- readTVar (notifyEntries notifyLog)
  case Map.lookupGE cursor entries of
    Nothing -> retry
    Just (at, entry) -> pure (entry, succCursor at)

-- | Prune every entry no longer worth relaying at the given time.
--
-- Leaves the log untouched if there is none, so as not to wake every server
-- reading it.
pruneNotifyLog :: IOLike m => NotifyLog m blk -> RelativeTime -> STM m ()
pruneNotifyLog notifyLog now = do
  entries <- readTVar (notifyEntries notifyLog)
  let kept = Map.filter (not . expired) entries
  unless (Map.size kept == Map.size entries) $
    writeTVar (notifyEntries notifyLog) kept
 where
  expired = \case
    NotifyAnnouncement onset _anc -> diffRelTime now onset > maxAnnouncementAgeSend
    NotifyVote windowEnd _vote -> now > windowEnd

-- | Prune the log every second.
--
-- The tick is far below the margin between 'maxAnnouncementAgeSend', or a
-- vote's window, and what a downstream peer tolerates, so no entry is relayed
-- too late for it.
notifyLogPruner :: IOLike m => SystemTime m -> NotifyLog m blk -> m Void
notifyLogPruner systemTime notifyLog = forever $ do
  now <- systemTimeCurrent systemTime
  atomically $ pruneNotifyLog notifyLog now
  threadDelay 1

succCursor :: NotifyCursor -> NotifyCursor
succCursor (MkNotifyCursor n) = MkNotifyCursor (n + 1)
