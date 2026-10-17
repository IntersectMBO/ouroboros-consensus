{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Ouroboros.Consensus.Leios.Types (ElectionId (electionSlot))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
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
  , prunedTo :: !(StrictTVar m SlotNo)
  -- ^ every election before this slot is forgotten, and ignored
  , relayLog :: !(NotifyLog m blk)
  }

newAnnounceState ::
  (IOLike m, NoThunks (Header blk)) =>
  NotifyLog m blk ->
  m (AnnounceState m blk)
newAnnounceState notifyLog =
  MkAnnounceState <$> newTVarIO Announcements.empty <*> newTVarIO 0 <*> pure notifyLog

-- | Accept a valid announcement, and relay it if it is new and fresh enough.
--
-- An announcement signed by a revoked operational certificate is neither, and
-- one of an election already pruned, while it was validated, is not new.
onValidAnnouncement ::
  (IOLike m, HasHeader (Header blk)) =>
  AnnounceState m blk ->
  ValidAnnouncement blk ->
  STM m ()
onValidAnnouncement st valid = do
  pruned <- readTVar prunedTo
  when (announcementStaleness == FreshOCIN && electionSlot electionId >= pruned) $ do
    seen <- readTVar announced
    case Announcements.insert @_ @Void electionId announcingHeader seen of
      Left _repeatedOrThird -> pure ()
      Right seen' -> do
        writeTVar announced seen'
        when (announcementAge <= maxAnnouncementAgeSend) $
          appendNotifyLog relayLog (NotifyAnnouncement announcementOnset announcingHeader)
 where
  electionId = ancElectionId announcingHeader

  MkAnnounceState{prunedTo, announced, relayLog} = st

  MkValidAnnouncement
    { announcingHeader
    , announcementOnset
    , announcementAge
    , announcementStaleness
    } = valid

-- | Forget every election before the given slot, which is the immutable tip's.
pruneAnnounceState ::
  IOLike m =>
  AnnounceState m blk ->
  SlotNo ->
  STM m ()
pruneAnnounceState st immTipSlot = do
  modifyTVar (announced st) (snd . Announcements.prune immTipSlot)
  writeTVar (prunedTo st) immTipSlot
