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
  , hasRelayedAnnouncement
  , newAnnounceState
  , onValidAnnouncement
  , pruneAnnounceState
  ) where

import Control.Monad (when)
import qualified Data.Foldable as Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, Header, SlotNo)
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Protocol (OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types (RbHash)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (ancElectionId)
  , ValidAnnouncement (..)
  , announcingRbHash
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
  , relayed :: !(StrictTVar m (Set RbHash))
  -- ^ The announcing blocks of the announcements appended to the log, which
  -- votes name.
  , relayLog :: !(NotifyLog m blk)
  }

newAnnounceState ::
  (IOLike m, NoThunks (Header blk)) =>
  NotifyLog m blk ->
  m (AnnounceState m blk)
newAnnounceState notifyLog =
  MkAnnounceState <$> newTVarIO Announcements.empty <*> newTVarIO Set.empty <*> pure notifyLog

-- | Accept a valid announcement, and relay it if it is new and fresh enough.
--
-- An announcement signed by a revoked operational certificate is neither.
onValidAnnouncement ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
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
        when (announcementAge valid <= maxAnnouncementAgeSend) $ do
          modifyTVar (relayed st) (Set.insert (announcingRbHash anc))
          appendNotifyLog (relayLog st) (NotifyAnnouncement (announcementOnset valid) anc)

-- | Whether the node relays the announcement of this announcing block, so that
-- it can relay votes for it.
hasRelayedAnnouncement :: IOLike m => AnnounceState m blk -> RbHash -> STM m Bool
hasRelayedAnnouncement st rbHash = Set.member rbHash <$> readTVar (relayed st)

-- | Forget every election before the given slot, which is the immutable tip's.
pruneAnnounceState ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  AnnounceState m blk ->
  SlotNo ->
  STM m ()
pruneAnnounceState st immTipSlot = do
  (pruned, kept) <- Announcements.prune immTipSlot <$> readTVar (announced st)
  writeTVar (announced st) kept
  modifyTVar (relayed st) $ \rbHashes ->
    Foldable.foldl' (\acc anc -> Set.delete (announcingRbHash anc) acc) rbHashes pruned
