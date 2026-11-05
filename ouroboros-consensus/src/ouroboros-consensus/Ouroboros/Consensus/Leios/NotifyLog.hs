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
-- Whoever appends a notification says until when it is worth relaying; the
-- pruner drops it then, and the log does not take one it was pruned past
-- already. So the node relays none longer than the pruning cadence after
-- that. A peer that connects
-- late starts at the log's oldest notification, so it still receives
-- everything worth relaying, and a notification is never worth relaying longer
-- than the announcement it is about, so a peer always receives the
-- announcement first. A cursor that falls behind pruning merely skips what was
-- pruned.
module Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyCursor
  , NotifyLog
  , appendNotifyLog
  , newNotifyLog
  , nextNotifyLog
  , notifyLogStart
  , pruneNotifyLog
  ) where

import Control.Monad (unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTVar
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
  -- TODO: add offers. 'storeForgedEb' does not wait for the writes,
  -- so offer a self-forged EB only after the LeiosDB sends 'AcquiredEb', and
  -- its txs only after 'AcquiredEbTxs'.
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (Notification blk)

-- | The node's log of notifications to relay.
data NotifyLog m blk = MkNotifyLog
  { notifyEntries :: !(StrictTVar m (Map NotifyCursor (Notification blk)))
  -- ^ Each notification, at the position it was appended at.
  , notifyEnd :: !(StrictTVar m NotifyCursor)
  -- ^ Where the next notification will be appended.
  , notifyExpiry :: !(StrictTVar m (Map RelativeTime (Set NotifyCursor)))
  -- ^ The position of each notification, by until when it is worth relaying,
  -- so that pruning visits only what expires.
  , notifyPrunedAt :: !(StrictTVar m RelativeTime)
  -- ^ When the log was last pruned.
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
    <*> newTVarIO (RelativeTime 0)

-- | Append a notification worth relaying until the given time, unless the log
-- was pruned past it already.
--
-- After that time, the notification is of no use to a downstream peer: a vote's
-- deadline, or when an announcement is too old to relay. Either leaves a
-- grace, for transmission and queueing, before the peer would reject it.
appendNotifyLog ::
  IOLike m =>
  NotifyLog m blk -> Notification blk -> RelativeTime -> STM m ()
appendNotifyLog notifyLog n expires = do
  prunedAt <- readTVar (notifyPrunedAt notifyLog)
  unless (expires < prunedAt) $ do
    end <- readTVar (notifyEnd notifyLog)
    modifyTVar (notifyEntries notifyLog) (Map.insert end n)
    modifyTVar (notifyExpiry notifyLog) (Map.insertWith Set.union expires (Set.singleton end))
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
  modifyTVar (notifyPrunedAt notifyLog) (max now)
  (expired, unexpired) <- Map.spanAntitone (< now) <$> readTVar (notifyExpiry notifyLog)
  unless (Map.null expired) $ do
    writeTVar (notifyExpiry notifyLog) unexpired
    modifyTVar (notifyEntries notifyLog) (`Map.withoutKeys` Set.unions expired)

succCursor :: NotifyCursor -> NotifyCursor
succCursor (MkNotifyCursor n) = MkNotifyCursor (n + 1)
