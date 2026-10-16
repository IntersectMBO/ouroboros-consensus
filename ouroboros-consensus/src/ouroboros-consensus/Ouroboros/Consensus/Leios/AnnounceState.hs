{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | The announcements the node has accepted, from whichever peer.
--
-- Each LeiosNotify client already holds its own upstream peer to at most two
-- announcements per election. But the node relays what it accepts from all its
-- peers, and a downstream peer holds the node to the same: no repeat, and no
-- third announcement of an election. So the node decides once, here, whether
-- an announcement is new, and only then relays it.
module Ouroboros.Consensus.Leios.AnnounceState
  ( AnnounceState
  , newAnnounceState
  , onValidAnnouncement
  , pruneAnnounceState
  ) where

import Control.Monad (when)
import Data.Void (Void)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (HasHeader, Header, SlotNo)
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Protocol (OCINStaleness (FreshOCIN))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (ancElectionId)
  , ValidAnnouncement (..)
  , maxAnnouncementAgeSend
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )

-- | The node's announcements, per election, and the log it relays them in.
data AnnounceState m blk = MkAnnounceState
  { announced :: !(StrictTVar m (Announcements.State (AnnouncingHeader blk)))
  , relayLog :: !(NotifyLog m blk)
  }

newAnnounceState ::
  (IOLike m, NoThunks (Header blk)) =>
  NotifyLog m blk ->
  m (AnnounceState m blk)
newAnnounceState notifyLog = MkAnnounceState <$> newTVarIO Announcements.empty <*> pure notifyLog

-- | Accept a valid announcement, and relay it if it is new and fresh enough.
--
-- An announcement signed by a revoked operational certificate is neither.
onValidAnnouncement ::
  (IOLike m, HasHeader (Header blk)) =>
  AnnounceState m blk ->
  ValidAnnouncement ->
  AnnouncingHeader blk ->
  STM m ()
onValidAnnouncement st valid anc =
  when (announcementStaleness valid == FreshOCIN) $ do
    seen <- readTVar (announced st)
    case Announcements.insert @_ @Void (ancElectionId anc) anc seen of
      Left _repeatedOrThird -> pure ()
      Right seen' -> do
        writeTVar (announced st) seen'
        when (announcementAge valid <= maxAnnouncementAgeSend) $
          appendNotifyLog (relayLog st) (NotifyAnnouncement (announcementOnset valid) anc)

-- | Forget every election before the given slot, which is the immutable tip's.
pruneAnnounceState ::
  IOLike m =>
  AnnounceState m blk ->
  SlotNo ->
  STM m ()
pruneAnnounceState st immTipSlot =
  modifyTVar (announced st) (snd . Announcements.prune immTipSlot)
