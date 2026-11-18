{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The announcements the node has accepted, from whichever peer.
--
-- Each LeiosNotify client already holds its own upstream peer to at most two
-- announcements per election. But the node relays what it accepts from all its
-- peers, and a downstream peer holds the node to the same: no repeat, and no
-- third announcement of an election. So the node decides once, here, whether
-- an announcement is new, and only then relays it.
module Ouroboros.Consensus.Leios.AnnounceState
  ( AnnounceState (ageLimits)
  , acceptedAnnouncementAge
  , hasAnnouncement
  , isEquivocated
  , newAnnounceState
  , onValidAnnouncement
  , WhetherNewAnnouncement (..)
  , WhetherEquivocationProof (..)
  , pruneAnnounceState
  ) where

import Control.Monad (when)
import Data.Function (on)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( ConvertRawHash
  , GetHeader
  , HasHeader
  , Header
  , RealPoint (..)
  , SlotNo
  , headerHash
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (addRelTime)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Protocol (OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (electionSlot)
  , RankingBlockHash
  , toRankingBlockHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  , LeiosAnnouncementAgeLimits
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
  { announced :: !(StrictTVar m (Announcements.State (Accepted blk)))
  , prunedTo :: !(StrictTVar m SlotNo)
  -- ^ every election before this slot is forgotten, and ignored
  , relayLog :: !(NotifyLog m blk)
  , ageLimits :: !LeiosAnnouncementAgeLimits
  -- ^ how old an announcement the node relays or accepts
  }

-- | An announcement the node accepted, and how long after its election slot
-- began it did.
data Accepted blk = MkAccepted
  { acceptedHeader :: !(AnnouncingHeader blk)
  , acceptedAge :: !NominalDiffTime
  }
  deriving Generic

instance NoThunks (Header blk) => NoThunks (Accepted blk)

-- | The same announcement, whenever it arrived.
instance HasHeader (Header blk) => Eq (Accepted blk) where
  (==) = (==) `on` acceptedHeader

newAnnounceState ::
  (IOLike m, NoThunks (Header blk)) =>
  LeiosAnnouncementAgeLimits ->
  NotifyLog m blk ->
  m (AnnounceState m blk)
newAnnounceState limits notifyLog =
  MkAnnounceState
    <$> newTVarIO Announcements.empty
    <*> newTVarIO 0
    <*> pure notifyLog
    <*> pure limits

-- | Whether the node accepted an announcement it had not accepted before.
data WhetherNewAnnouncement = NewAnnouncement !WhetherEquivocationProof | NotNewAnnouncement

-- | Whether a new announcement is the second of its election, which proves
-- that the election's electee equivocated.
data WhetherEquivocationProof = EquivocationProof | NotEquivocationProof

-- | Accept a valid announcement, and relay it if it is new and fresh enough,
-- until 'maxAnnouncementAgeSend' after its election slot began.
--
-- An announcement signed by a revoked operational certificate is neither, and
-- one of an election already pruned, while it was validated, is not new.
--
-- Once this accepts an announcement, no copy of it is new again, from any
-- peer. So a caller that acts on a new announcement received from a peer
-- rather than from the forge must commit to doing so within this same
-- transaction: if it acted afterwards, and its thread died in between, as when
-- its peer disconnects, the announcement would stay accepted but never be
-- acted on, and nothing would act on it later.
onValidAnnouncement ::
  (IOLike m, HasHeader (Header blk)) =>
  AnnounceState m blk ->
  ValidAnnouncement blk ->
  STM m WhetherNewAnnouncement
onValidAnnouncement st valid = do
  pruned <- readTVar prunedTo
  if announcementStaleness /= FreshOCIN || electionSlot electionId < pruned
    then pure NotNewAnnouncement
    else do
      seen <- readTVar announced
      case Announcements.insert @_ @Void electionId (MkAccepted announcingHeader announcementAge) seen of
        Left _repeatedOrThird -> pure NotNewAnnouncement
        Right seen' -> do
          writeTVar announced seen'
          when (announcementAge <= maxAnnouncementAgeSend ageLimits) $
            appendNotifyLog
              relayLog
              (NotifyAnnouncement announcingHeader)
              (addRelTime (maxAnnouncementAgeSend ageLimits) announcementOnset)
          pure $ NewAnnouncement $ case Map.lookup electionId (Announcements.live seen) of
            Nothing -> NotEquivocationProof
            Just _first -> EquivocationProof
 where
  electionId = ancElectionId announcingHeader

  MkAnnounceState{prunedTo, announced, relayLog, ageLimits} = st

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
  any ((== rbHash) . announcingRbHash . acceptedHeader) <$> acceptedInSlot st slot

-- | Whether the node accepted the announcement of this announcing block, and
-- another announcement of the same election.
isEquivocated ::
  forall m blk.
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  AnnounceState m blk ->
  RealPoint blk ->
  STM m Bool
isEquivocated st (RealPoint slot hash) = do
  inSlot <- map acceptedHeader <$> acceptedInSlot st slot
  pure $ case filter ((== rbHash) . announcingRbHash) inSlot of
    anc : _ ->
      any (\other -> ancElectionId other == ancElectionId anc && announcingRbHash other /= rbHash) inSlot
    [] -> False
 where
  rbHash = toRankingBlockHash (Proxy @blk) hash

-- | How long after its election slot began the node accepted the announcement
-- of this announcing block, if it did.
acceptedAnnouncementAge ::
  (IOLike m, HasHeader (Header blk)) =>
  AnnounceState m blk ->
  RealPoint blk ->
  STM m (Maybe NominalDiffTime)
acceptedAnnouncementAge st (RealPoint slot hash) =
  fmap acceptedAge . find ((== hash) . headerHash . ancHeader . acceptedHeader)
    <$> acceptedInSlot st slot

-- | The announcements the node accepted in elections of the given slot.
acceptedInSlot :: IOLike m => AnnounceState m blk -> SlotNo -> STM m [Accepted blk]
acceptedInSlot st slot = Announcements.inSlot slot <$> readTVar (announced st)

-- | Forget every election before the given slot, which is the immutable tip's.
pruneAnnounceState ::
  IOLike m =>
  AnnounceState m blk ->
  SlotNo ->
  STM m ()
pruneAnnounceState st immTipSlot = do
  modifyTVar (announced st) (snd . Announcements.prune immTipSlot)
  writeTVar (prunedTo st) immTipSlot
