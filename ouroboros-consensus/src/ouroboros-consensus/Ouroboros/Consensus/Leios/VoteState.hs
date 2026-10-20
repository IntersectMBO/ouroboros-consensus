{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The votes the node has accepted, from whichever peer.
--
-- Each LeiosNotify client already holds its own upstream peer to one vote per
-- seat in an election. But the node relays what it accepts from all its peers,
-- and a downstream peer holds the node to the same, and to having relayed the
-- announcement a vote is for. So the node decides once, here, whether a vote
-- is new and valid, and only then relays it.
module Ouroboros.Consensus.Leios.VoteState
  ( VoteState
  , newVoteState
  , onAcceptedVote
  , voteRelay

    -- * The votes kept
  , Votes
  , maxVoteElections
  , readVotes
  , recordVote
  , seenVote
  ) where

import Cardano.Crypto.DSIGN.Class (verifyDSIGN)
import Cardano.Crypto.Leios
  ( LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId
  , leiosSignContext
  , resolveLeiosSeat
  )
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Monad (forever, unless, when)
import Control.Monad.Class.MonadTimer.SI (MonadTimer, registerDelay)
import Data.Either (isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Void (Void)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, Header, SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, hasAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote (..))
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , ExnLeiosNotifyInvalidVotes (ExnLeiosInvalidVote)
  , VoteRelayWindow (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTVar
  , atomically
  , check
  , modifyTVar
  , newTVarIO
  , readTVar
  , throwIO
  , writeTVar
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)

-- | The node's votes, and where it learns which announcements it relays and
-- relays the votes.
data VoteState m blk = MkVoteState
  { votes :: !(StrictTVar m Votes)
  , held :: !(StrictTVar m (Map RelativeTime [NotifyEntry blk]))
  -- ^ The votes accepted before their window opened, by when it opens, the
  -- latest accepted first; see 'voteRelay'.
  , announceState :: !(AnnounceState m blk)
  -- ^ Which announcements the node relays, as it relays only votes for those.
  , relayLog :: !(NotifyLog m blk)
  -- ^ Where the node relays the votes it adds.
  }

newVoteState ::
  (IOLike m, NoThunks (Header blk)) =>
  AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState announceState' notifyLog =
  MkVoteState
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure announceState'
    <*> pure notifyLog

-- | Accept a vote, and relay it if it is the first valid vote of its seat in
-- its election, for an announcement the node relays, and its window has not
-- closed; if its window has not opened yet, 'voteRelay' relays it then.
--
-- Throws 'ExnLeiosInvalidVote' if the vote is invalid, to disconnect the peer
-- that sent it. The signature is verified between two transactions, so that a
-- retry does not repeat a BLS verification.
onAcceptedVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  VoteState m blk -> SystemTime m -> AcceptedVote -> m ()
onAcceptedVote st systemTime accepted = do
  received <- systemTimeCurrent systemTime
  -- A vote no longer worth relaying is not worth verifying either.
  worthVerifying <-
    if received > relayUntil voteRelayWindow
      then pure False
      else atomically $ do
        seen <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
        -- NOTE: The client only checked that its peer announced it, but the
        -- node does not relay a third announcement of an election, nor one of
        -- a stale OCIN, so it must not relay their votes either.
        announced <-
          hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
        pure (not seen && announced)
  when worthVerifying $ do
    unless (validVote voteCommittee acceptedVote) $
      throwIO $
        ExnLeiosInvalidVote (voteAnnouncingRbHash acceptedVote) (voteSeatId acceptedVote)
    now <- systemTimeCurrent systemTime
    atomically $ do
      -- Another peer may have delivered the same vote meanwhile.
      seen <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
      unless seen $ do
        recordVote st voteElectionSlot acceptedVote
        if now < relayFrom voteRelayWindow
          then holdVote st (relayFrom voteRelayWindow) entry
          else relay entry
 where
  MkAcceptedVote{acceptedVote, voteElectionSlot, voteCommittee, voteRelayWindow} = accepted

  entry = MkNotifyEntry (relayUntil voteRelayWindow) (NotifyVote acceptedVote)

  relay = appendNotifyLog (relayLog st)

-- | Relay each held vote once its window opens by our clock, as a downstream
-- peer whose clock is behind ours could reject it earlier.
voteRelay ::
  (IOLike m, MonadTimer m) =>
  VoteState m blk -> SystemTime m -> m Void
voteRelay st systemTime = forever $ do
  now <- systemTimeCurrent systemTime
  next <- atomically $ do
    (opened, stillHeld) <- Map.spanAntitone (<= now) <$> readTVar (held st)
    unless (Map.null opened) $ do
      writeTVar (held st) stillHeld
      mapM_ (appendNotifyLog (relayLog st)) (concatMap reverse (Map.elems opened))
    pure (fst <$> Map.lookupMin stillHeld)
  opens <- case next of
    Nothing -> pure (pure False)
    Just at -> LazySTM.readTVar <$> registerDelay (nominalDelay (diffRelTime at now))
  -- Until then, or until a vote is held whose window opens sooner.
  atomically $ do
    isOpen <- opens
    soonest <- fmap fst . Map.lookupMin <$> readTVar (held st)
    check (isOpen || maybe False (\at -> maybe True (at <) next) soonest)

-- | Hold a vote back until its window opens; see 'voteRelay'.
holdVote ::
  IOLike m =>
  VoteState m blk -> RelativeTime -> NotifyEntry blk -> STM m ()
holdVote st opens entry = modifyTVar (held st) (Map.insertWith (<>) opens [entry])

-- | Whether the vote's seat has a key in the committee, and the signature
-- verifies under it.
validVote :: LeiosCommittee -> LeiosVote -> Bool
validVote committee MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature} =
  case resolveLeiosSeat committee voteSeatId of
    Just LeiosSeat{seatVKey = SJust vk} ->
      isRight $ verifyDSIGN leiosSignContext vk voteAnnouncingRbHash voteSignature
    _ -> False

{-------------------------------------------------------------------------------
  The votes kept
-------------------------------------------------------------------------------}

-- | The first valid vote of each seat, per election slot, for the latest
-- 'maxVoteElections' election slots the node has votes for.
type Votes = Map SlotNo (Map LeiosSeatId LeiosVote)

-- | How many election slots the node keeps votes for: far more than the
-- elections whose votes are still within their window.
--
-- TODO: magic number
maxVoteElections :: Int
maxVoteElections = 128

readVotes ::
  IOLike m =>
  VoteState m blk -> STM m Votes
readVotes st = readTVar (votes st)

-- | Whether the node may already have seen a vote of this seat in this
-- election slot: it has one, or, once full, the slot is older than all it
-- keeps, so it may have forgotten one.
seenVote ::
  IOLike m =>
  VoteState m blk -> SlotNo -> LeiosSeatId -> STM m Bool
seenVote st slot seat = seen <$> readVotes st
 where
  seen vs = case Map.lookup slot vs of
    Just seats -> Map.member seat seats
    Nothing -> Map.size vs >= maxVoteElections && maybe False ((>= slot) . fst) (Map.lookupMin vs)

-- | Keep a vote the node has not 'seenVote', forgetting the oldest election
-- slot if there are then more than 'maxVoteElections'.
recordVote ::
  IOLike m =>
  VoteState m blk -> SlotNo -> LeiosVote -> STM m ()
recordVote st slot vote = modifyTVar (votes st) insertVote
 where
  insertVote vs
    | Map.size vs' > maxVoteElections = Map.deleteMin vs'
    | otherwise = vs'
   where
    vs' = Map.insertWith Map.union slot (Map.singleton (voteSeatId vote) vote) vs
