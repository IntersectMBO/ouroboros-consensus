{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What the node relays to its downstream peers over LeiosNotify.
--
-- The node keeps one log of notifications, in the order it appended them, and
-- every LeiosNotify server reads that log through its own 'NotifyCursor'. So a
-- notification is stored once however many peers it goes to, and a peer that
-- requests slowly merely lags behind in the log, rather than piling up a queue
-- of its own.
--
-- Whoever appends a notification has already decided it is worth relaying
-- now, and says until when; the pruner drops it then. A peer that connects
-- late starts at the log's oldest notification, so it still receives
-- everything worth relaying, and a notification is never worth relaying longer
-- than the announcement it is about, so a peer always receives the
-- announcement first. A cursor that falls behind pruning merely skips what was
-- pruned.
module Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyCursor
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
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader
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
data Notification blk
  = NotifyAnnouncement !(AnnouncingHeader blk)
  | NotifyVote !LeiosVote
  -- TODO: add offers
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (Notification blk)

-- | A notification, with until when it is worth relaying.
data NotifyEntry blk = MkNotifyEntry
  { notifyUntil :: !RelativeTime
  -- ^ After this, the notification is of no use to a downstream peer: a vote's
  -- deadline, or when an announcement is too old to relay. Either leaves a
  -- grace, for transmission and queueing, before the peer would reject it.
  , notification :: !(Notification blk)
  }
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (NotifyEntry blk)

-- | The node's log of notifications to relay.
data NotifyLog m blk = MkNotifyLog
  { notifyEntries :: !(StrictTVar m (Map NotifyCursor (Notification blk)))
  -- ^ Each notification, at the position it was appended at.
  , notifyEnd :: !(StrictTVar m NotifyCursor)
  -- ^ Where the next notification will be appended.
  , notifyExpiry :: !(StrictTVar m (Map RelativeTime [NotifyCursor]))
  -- ^ The position of each notification, by until when it is worth relaying,
  -- so that pruning visits only what expires.
  }

-- | A position in the log, which only ever grows.
newtype NotifyCursor = MkNotifyCursor Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype NoThunks

-- | Where a downstream peer starts reading: before every notification,
-- including the ones already pruned.
notifyLogStart :: NotifyCursor
notifyLogStart = MkNotifyCursor 0

newNotifyLog :: (IOLike m, NoThunks (Header blk)) => m (NotifyLog m blk)
newNotifyLog =
  MkNotifyLog
    <$> newTVarIO Map.empty
    <*> newTVarIO notifyLogStart
    <*> newTVarIO Map.empty

appendNotifyLog :: IOLike m => NotifyLog m blk -> NotifyEntry blk -> STM m ()
appendNotifyLog notifyLog (MkNotifyEntry expires n) = do
  end <- readTVar (notifyEnd notifyLog)
  modifyTVar (notifyEntries notifyLog) (Map.insert end n)
  modifyTVar (notifyExpiry notifyLog) (Map.insertWith (<>) expires [end])
  writeTVar (notifyEnd notifyLog) (succCursor end)

-- | The first notification at or after the cursor, and the cursor just past
-- it.
--
-- Retries until there is such a notification.
nextNotifyLog ::
  IOLike m =>
  NotifyLog m blk ->
  NotifyCursor ->
  STM m (Notification blk, NotifyCursor)
nextNotifyLog notifyLog cursor = do
  entries <- readTVar (notifyEntries notifyLog)
  case Map.lookupGE cursor entries of
    Nothing -> retry
    Just (at, n) -> pure (n, succCursor at)

-- | Prune every notification no longer worth relaying at the given time.
--
-- Leaves the notifications untouched if there is none, so as not to wake every
-- server reading them.
pruneNotifyLog :: IOLike m => NotifyLog m blk -> RelativeTime -> STM m ()
pruneNotifyLog notifyLog now = do
  (expired, unexpired) <- Map.spanAntitone (< now) <$> readTVar (notifyExpiry notifyLog)
  unless (Map.null expired) $ do
    writeTVar (notifyExpiry notifyLog) unexpired
    modifyTVar (notifyEntries notifyLog) $
      (`Map.withoutKeys` Set.fromList (concat (Map.elems expired)))

-- | Prune the log every second.
--
-- The tick is far below the margin between until when a notification is worth
-- relaying and what a downstream peer tolerates, so none is relayed too late
-- for it.
notifyLogPruner :: IOLike m => SystemTime m -> NotifyLog m blk -> m Void
notifyLogPruner systemTime notifyLog = forever $ do
  now <- systemTimeCurrent systemTime
  atomically $ pruneNotifyLog notifyLog now
  threadDelay 1

succCursor :: NotifyCursor -> NotifyCursor
succCursor (MkNotifyCursor n) = MkNotifyCursor (n + 1)
