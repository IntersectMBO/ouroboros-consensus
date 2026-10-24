{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The votes the node has accepted, from whichever peer.
--
-- Each LeiosNotify client already holds its own upstream peer to one vote per
-- seat in an election. But the node relays what it accepts from all its peers,
-- and a downstream peer holds the node to the same, and to having relayed the
-- announcement a vote is for. So the node decides once, here, whether a vote
-- is new and valid, and only then relays it.
--
-- A vote is relayed only to be aggregated into a certificate, which the
-- earliest block that can carry it needs in the first slot at or after the
-- vote's deadline, the end of its diffusion period ('relayUntil'). The node
-- neither keeps nor relays a vote it has only after its deadline. Such a vote
-- could still complete a certificate for a later block, but a quorum that late
-- is no evidence that the endorser block's data diffused in time, so the node
-- deliberately gives up that throughput rather than endanger safety.
module Ouroboros.Consensus.Leios.VoteState
  ( VoteState
  , newVoteState
  , TraceLeiosVoteState (..)
  , AddVoteResult (..)
  , addVote
  , voteRelay
  , votePruner

    -- * The votes kept
  , Votes (..)
  , SlotVotes (..)
  , readVotes
  , recordVote
  , pruneVotes
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
import Control.Tracer (Tracer, traceWith)
import Data.Either (isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, Header, SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
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
  , VoteRelayWindow (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadDelay (threadDelay)
  , STM
  , StrictTVar
  , atomically
  , check
  , modifyTVar
  , newTVarIO
  , readTVar
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
  , tracer :: !(Tracer m TraceLeiosVoteState)
  }

newVoteState ::
  (IOLike m, NoThunks (Header blk)) =>
  Tracer m TraceLeiosVoteState -> AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState tracer' announceState' notifyLog =
  MkVoteState
    <$> newTVarIO (MkVotes (RelativeTime 0) Map.empty)
    <*> newTVarIO Map.empty
    <*> pure announceState'
    <*> pure notifyLog
    <*> pure tracer'

-- | What the node's 'VoteState' traces.
data TraceLeiosVoteState
  = -- | The node verified the signature of a vote, and has yet to add it.
    TraceLeiosVoteVerified !LeiosVote
  | -- | The node had a vote only after its deadline, so it neither keeps nor
    -- relays it. Many of these mean the diffusion period is too short for the
    -- network, or that the node verifies votes too slowly.
    TraceLeiosVoteArrivedTooLate !LeiosVote
  deriving Show

-- | What 'addVote' did with a vote.
data AddVoteResult
  = -- | The node had it only after its deadline.
    ArrivedTooLate
  | -- | We already saw a vote of its seat in its election.
    AlreadyKnown
  | -- | We did not relay the announcement it is for.
    NotAnnounced
  | -- | Its seat has no key in the committee, or its signature does not verify
    -- under it.
    VoteInvalid
  | -- | The node keeps the vote and relays it, once its window opens if it has
    -- not yet; see 'voteRelay'.
    Added
  deriving stock (Eq, Show)

-- | Add a vote, if it is the first valid vote of its seat in its election, for
-- an announcement the node relays, and the node has it by its deadline.
--
-- The signature is verified between two transactions, so that a retry does
-- not repeat a BLS verification. As verifying takes time, the second checks
-- the deadline again.
addVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  VoteState m blk -> SystemTime m -> AcceptedVote -> m AddVoteResult
addVote st systemTime accepted = do
  received <- systemTimeCurrent systemTime
  seen <- atomically $ seenVote st voteElectionSlot (voteSeatId acceptedVote)
  -- NOTE: The client only checked that its peer announced it, but the node
  -- does not relay a third announcement of an election, nor one of a stale
  -- OCIN, so it must not relay their votes either.
  announced <-
    atomically $
      hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
  result <-
    if
      | seen -> pure AlreadyKnown
      | received > relayUntil voteRelayWindow -> pure ArrivedTooLate
      | not announced -> pure NotAnnounced
      | not (validVote voteCommittee acceptedVote) -> pure VoteInvalid
      | otherwise -> do
          traceWith (tracer st) (TraceLeiosVoteVerified acceptedVote)
          now <- systemTimeCurrent systemTime
          atomically $ do
            MkVotes{votesPrunedAt} <- readVotes st
            seenMeanwhile <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
            if
              -- Another peer may have delivered the same vote meanwhile.
              | seenMeanwhile -> pure AlreadyKnown
              -- The pruner may have forgotten its election slot already, and
              -- with it whether the node relayed the vote, if 'now' is stale.
              | now > relayUntil voteRelayWindow || relayUntil voteRelayWindow < votesPrunedAt ->
                  pure ArrivedTooLate
              | otherwise -> do
                  recordVote st accepted
                  if now < relayFrom voteRelayWindow
                    then holdVote st (relayFrom voteRelayWindow) entry
                    else relay entry
                  pure Added
  when (result == ArrivedTooLate) $
    traceWith (tracer st) (TraceLeiosVoteArrivedTooLate acceptedVote)
  pure result
 where
  MkAcceptedVote
    { acceptedVote
    , voteElectionSlot
    , voteCommittee
    , voteRelayWindow
    } = accepted

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

-- | The first valid vote of each seat, per election slot, for every election
-- slot whose deadline had not passed when the votes were last pruned.
data Votes = MkVotes
  { votesPrunedAt :: !RelativeTime
  -- ^ When the votes were last pruned. The node may have forgotten whether it
  -- has a vote whose deadline is before this.
  , votesBySlot :: !(Map SlotNo SlotVotes)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The votes of one election slot, and their deadline.
data SlotVotes = MkSlotVotes
  { slotRelayUntil :: !RelativeTime
  , slotVotes :: !(Map LeiosSeatId LeiosVote)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

readVotes ::
  IOLike m =>
  VoteState m blk -> STM m Votes
readVotes st = readTVar (votes st)

-- | Whether the node has a vote of this seat in this election slot.
seenVote ::
  IOLike m =>
  VoteState m blk -> SlotNo -> LeiosSeatId -> STM m Bool
seenVote st slot seat =
  maybe False (Map.member seat . slotVotes) . Map.lookup slot . votesBySlot <$> readVotes st

-- | Keep a vote the node has not 'seenVote'.
recordVote ::
  IOLike m =>
  VoteState m blk -> AcceptedVote -> STM m ()
recordVote st MkAcceptedVote{acceptedVote, voteElectionSlot, voteRelayWindow} =
  modifyTVar (votes st) $ \vs ->
    vs{votesBySlot = Map.insertWith keepFirst voteElectionSlot slotVotes' (votesBySlot vs)}
 where
  slotVotes' =
    MkSlotVotes
      (relayUntil voteRelayWindow)
      (Map.singleton (voteSeatId acceptedVote) acceptedVote)

  keepFirst new old = old{slotVotes = Map.union (slotVotes old) (slotVotes new)}

-- | Forget the votes of every election slot whose deadline has passed at the
-- given time.
--
-- 'addVote' then finds any later copy of their votes 'ArrivedTooLate', before
-- it asks whether the node has seen it.
pruneVotes ::
  IOLike m =>
  VoteState m blk -> RelativeTime -> STM m ()
pruneVotes st now =
  modifyTVar (votes st) $ \MkVotes{votesPrunedAt, votesBySlot} ->
    MkVotes
      (max now votesPrunedAt)
      (Map.filter ((now <=) . slotRelayUntil) votesBySlot)

-- | Prune the votes every second; see 'pruneVotes'.
votePruner :: IOLike m => VoteState m blk -> SystemTime m -> m Void
votePruner st systemTime = forever $ do
  now <- systemTimeCurrent systemTime
  atomically $ pruneVotes st now
  threadDelay 1
