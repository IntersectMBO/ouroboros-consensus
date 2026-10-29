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
  , votePruner

    -- * The votes kept
  , currentVotes
  , seenOrForgottenVote
  , recordVote
  , pruneVotes
  ) where

import Cardano.Crypto.DSIGN.Class (verifyDSIGN)
import Cardano.Crypto.Leios
  ( LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId
  , leiosSignContext
  , resolveLeiosSeat
  )
import Control.Monad (forever, when)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, SlotNo)
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
  , InvalidVoteReason (..)
  , VoteRelayWindow (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadDelay (threadDelay)
  , MonadSTM
  , STM
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVar
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)

-- | The node's votes, and where it learns which announcements it relays and
-- relays the votes.
data VoteState m blk = MkVoteState
  { votes :: !(StrictTVar m Votes)
  , announceState :: !(AnnounceState m blk)
  -- ^ Which announcements the node relays, as it relays only votes for those.
  , relayLog :: !(NotifyLog m blk)
  -- ^ Where the node relays the votes it adds.
  , tracer :: !(Tracer m TraceLeiosVoteState)
  }

newVoteState ::
  IOLike m =>
  Tracer m TraceLeiosVoteState -> AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState tracer' announceState' notifyLog =
  MkVoteState
    <$> newTVarIO (MkVotes (RelativeTime 0) Map.empty)
    <*> pure announceState'
    <*> pure notifyLog
    <*> pure tracer'

-- | What the node's 'VoteState' traces, for each vote it adds or does not, so
-- these are frequent. What 'addVote' did with a vote is up to its caller to
-- trace.
data TraceLeiosVoteState
  = -- | The node starts to add a vote, and to check whether it should.
    TraceLeiosVoteAdding !LeiosVote
  | -- | The vote arrived before its window opened, so the node waits this long
    -- before adding it.
    TraceLeiosVoteAwaitingWindow !LeiosVote !NominalDiffTime
  | -- | The node validated a vote, its seat and signature, and has yet to add it.
    TraceLeiosVoteValidated !LeiosVote
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
    VoteInvalid !InvalidVoteReason
  | -- | The node keeps the vote and relays it.
    Added
  deriving stock (Eq, Show)

-- | Add a vote, if it is the first valid vote of its seat in its election, for
-- an announcement the node relays, and the node has it by its deadline.
--
-- NOTE: This will wait until the vote's window is open.
--
-- The signature is verified between two transactions, so that a retry does
-- not repeat a BLS verification. As verifying takes time, the second checks
-- the deadline again.
addVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  VoteState m blk -> SystemTime m -> AcceptedVote -> m AddVoteResult
addVote st systemTime accepted = do
  traceWith (tracer st) (TraceLeiosVoteAdding acceptedVote)
  received <- systemTimeCurrent systemTime
  -- Wait for the window of a vote that 'checkLeiosVotes' accepted early.
  -- NOTE: 'checkLeiosVotes' just rejected votes earlier than the
  -- clock skew, so this waits at most 'InFutureCheck.ClockSkew'.
  let early = relayFrom voteRelayWindow `diffRelTime` received
  when (early >= 0) $ do
    traceWith (tracer st) (TraceLeiosVoteAwaitingWindow acceptedVote early)
    threadDelay (nominalDelay early)
  seen <- atomically $ seenOrForgottenVote st accepted
  -- NOTE: The client only checked that its peer announced it, but the node
  -- does not relay a third announcement of an election, nor one of a stale
  -- OCIN, so it must not relay their votes either.
  announced <-
    atomically $
      hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
  if
    | seen -> pure AlreadyKnown
    | received > relayUntil voteRelayWindow -> pure ArrivedTooLate
    | not announced -> pure NotAnnounced
    -- NOTE: Verifying a signature is expensive, so this comes last.
    | Left reason <- validateVote voteCommittee acceptedVote -> pure (VoteInvalid reason)
    | otherwise -> do
        traceWith (tracer st) (TraceLeiosVoteValidated acceptedVote)
        now <- systemTimeCurrent systemTime
        atomically $ do
          -- Another peer may have delivered the same vote meanwhile, or the
          -- pruner may have forgotten its election already
          seenMeanwhile <- seenOrForgottenVote st accepted
          if
            | seenMeanwhile -> pure AlreadyKnown
            | now > relayUntil voteRelayWindow -> pure ArrivedTooLate
            | otherwise -> do
                recordVote st accepted
                appendNotifyLog (relayLog st) $
                  MkNotifyEntry (relayUntil voteRelayWindow) (NotifyVote acceptedVote)
                pure Added
 where
  MkAcceptedVote
    { acceptedVote
    , voteElectionSlot
    , voteCommittee
    , voteRelayWindow
    } = accepted

-- | Whether the vote's seat has a key in the committee, and the signature
-- verifies under it, or why not.
validateVote :: LeiosCommittee -> LeiosVote -> Either InvalidVoteReason ()
validateVote committee MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature} =
  case resolveLeiosSeat committee voteSeatId of
    Just LeiosSeat{seatVKey = SJust vk} ->
      first (BadSignature . Text.pack) $
        verifyDSIGN leiosSignContext vk voteAnnouncingRbHash voteSignature
    Just LeiosSeat{seatVKey = SNothing} -> Left SeatHasNoKey
    Nothing -> Left SeatNotResolvable

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

-- | Whether the node has a vote of this seat in this election, or may have had
-- one and forgotten it, because it pruned the votes past its deadline.
seenOrForgottenVote ::
  MonadSTM m =>
  VoteState m blk -> AcceptedVote -> STM m Bool
seenOrForgottenVote st accepted = do
  MkVotes{votesPrunedAt, votesBySlot} <- readTVar (votes st)
  pure $
    relayUntil voteRelayWindow < votesPrunedAt
      || maybe
        False
        (Map.member voteSeatId . slotVotes)
        (Map.lookup voteElectionSlot votesBySlot)
 where
  MkAcceptedVote
    { acceptedVote = MkLeiosVote{voteSeatId}
    , voteElectionSlot
    , voteRelayWindow
    } = accepted

-- | The votes the node keeps, by election slot and seat.
currentVotes ::
  IOLike m =>
  VoteState m blk -> STM m (Map SlotNo (Map LeiosSeatId LeiosVote))
currentVotes st = Map.map slotVotes . votesBySlot <$> readTVar (votes st)

-- | Keep a vote the node has not seen yet.
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
