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
  , acceptedAnnouncementAge
  , hasAnnouncement
  , isEquivocated
  , newAnnounceState
  , onValidAnnouncement
  , pruneAnnounceState
  ) where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( ConvertRawHash
  , GetHeader
  , HasHeader
  , Header
  , HeaderHash
  , SlotNo
  , blockSlot
  , headerHash
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (addRelTime)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Protocol (OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types (ElectionId (electionSlot), RankingBlockHash)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
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
  , ages :: !(StrictTVar m (Map (SlotNo, HeaderHash blk) NominalDiffTime))
  -- ^ How long after its election slot began the node accepted each of
  -- 'announced', by the slot and hash of its announcing block.
  , prunedTo :: !(StrictTVar m SlotNo)
  -- ^ every election before this slot is forgotten, and ignored
  , relayLog :: !(NotifyLog m blk)
  }

newAnnounceState ::
  (IOLike m, NoThunks (Header blk), HasHeader (Header blk)) =>
  NotifyLog m blk ->
  m (AnnounceState m blk)
newAnnounceState notifyLog =
  MkAnnounceState
    <$> newTVarIO Announcements.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> pure notifyLog

-- | Accept a valid announcement, and relay it if it is new and fresh enough,
-- until 'maxAnnouncementAgeSend' after its election slot began.
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
        modifyTVar ages $ Map.insert (announcementKey announcingHeader) announcementAge
        when (announcementAge <= maxAnnouncementAgeSend) $
          appendNotifyLog
            relayLog
            (NotifyAnnouncement announcingHeader)
            (addRelTime maxAnnouncementAgeSend announcementOnset)
 where
  electionId = ancElectionId announcingHeader

  MkAnnounceState{prunedTo, announced, ages, relayLog} = st

  MkValidAnnouncement
    { announcingHeader
    , announcementOnset
    , announcementAge
    , announcementStaleness
    } = valid

-- | Whether the node accepted the announcement of this announcing block, in an
-- election of the given slot, so that it can relay votes for it.
--
-- It accepts but does not relay one too old to relay, but then the votes for
-- it are past their deadline too, so the node relays none of them either.
hasAnnouncement ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  AnnounceState m blk ->
  SlotNo ->
  RankingBlockHash ->
  STM m Bool
hasAnnouncement st slot rbHash =
  any ((== rbHash) . announcingRbHash) . Announcements.inSlot slot <$> readTVar (announced st)

-- | Whether the node accepted another announcement of the same election.
isEquivocated ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  AnnounceState m blk ->
  AnnouncingHeader blk ->
  STM m Bool
isEquivocated st anc =
  any
    (\other -> ancElectionId other == ancElectionId anc && announcingRbHash other /= announcingRbHash anc)
    . Announcements.inSlot (electionSlot (ancElectionId anc))
    <$> readTVar (announced st)

-- | How long after its election slot began the node accepted the announcement
-- of this announcing block, if it did.
acceptedAnnouncementAge ::
  (IOLike m, HasHeader (Header blk)) =>
  AnnounceState m blk ->
  AnnouncingHeader blk ->
  STM m (Maybe NominalDiffTime)
acceptedAnnouncementAge st anc = Map.lookup (announcementKey anc) <$> readTVar (ages st)

announcementKey :: HasHeader (Header blk) => AnnouncingHeader blk -> (SlotNo, HeaderHash blk)
announcementKey anc = (blockSlot (ancHeader anc), headerHash (ancHeader anc))

-- | Forget every election before the given slot, which is the immutable tip's.
pruneAnnounceState ::
  IOLike m =>
  AnnounceState m blk ->
  SlotNo ->
  STM m ()
pruneAnnounceState st immTipSlot = do
  modifyTVar (announced st) (snd . Announcements.prune immTipSlot)
  modifyTVar (ages st) (snd . Map.spanAntitone ((< immTipSlot) . fst))
  writeTVar (prunedTo st) immTipSlot
