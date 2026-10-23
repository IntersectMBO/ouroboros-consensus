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
module Ouroboros.Consensus.Leios.VoteState
  ( VoteState
  , HookOnlyForTesting (..)
  , newVoteState
  , AddVoteResult (..)
  , addVote
  , voteRelay

    -- * The votes kept
  , Votes
  , ElectionVotes (..)
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
import Control.Monad (forever, unless)
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
  , hookOnlyForTesting :: !(Tracer m HookOnlyForTesting)
  }

-- | Where a test can intervene in 'addVote', eg to let time pass. The node
-- passes 'Control.Tracer.nullTracer'.
data HookOnlyForTesting
  = -- | 'addVote' verified a vote, and has yet to record it.
    VoteVerifiedHookOnlyForTesting

newVoteState ::
  (IOLike m, NoThunks (Header blk)) =>
  Tracer m HookOnlyForTesting -> AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState hook announceState' notifyLog =
  MkVoteState
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure announceState'
    <*> pure notifyLog
    <*> pure hook

-- | What 'addVote' did with a vote.
data AddVoteResult
  = -- | Its window had closed, so it was not worth verifying.
    Expired
  | -- | We already saw a vote of its seat in its election.
    AlreadyKnown
  | -- | We did not relay the announcement it is for.
    NotAnnounced
  | -- | Its seat has no key in the committee, or its signature does not verify
    -- under it.
    VoteInvalid !LeiosVote
  | -- | The node keeps the vote and relays it, once its window opens if it has
    -- not yet; see 'voteRelay'.
    Added !LeiosVote
  deriving stock (Eq, Show)

-- | Add a vote, if it is the first valid vote of its seat in its election, for
-- an announcement the node relays, and its window has not closed.
--
-- The signature is verified between two transactions, so that a retry does
-- not repeat a BLS verification.
addVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  VoteState m blk -> SystemTime m -> AcceptedVote -> m AddVoteResult
addVote st systemTime accepted = do
  received <- systemTimeCurrent systemTime
  -- A vote no longer worth relaying is not worth verifying either.
  if received > relayUntil voteRelayWindow
    then pure Expired
    else do
      seen <- atomically $ seenVote st voteElectionSlot (voteSeatId acceptedVote)
      -- NOTE: The client only checked that its peer announced it, but the
      -- node does not relay a third announcement of an election, nor one of a
      -- stale OCIN, so it must not relay their votes either.
      announced <-
        atomically $
          hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
      if
        | seen -> pure AlreadyKnown
        | not announced -> pure NotAnnounced
        | not (validVote voteCommittee acceptedVote) -> pure (VoteInvalid acceptedVote)
        | otherwise -> do
            traceWith (hookOnlyForTesting st) VoteVerifiedHookOnlyForTesting
            -- NOTE: We do not check the window again. Verifying takes about a
            -- millisecond, and peers allow a grace after the window.
            now <- systemTimeCurrent systemTime
            atomically $ do
              -- Another peer may have delivered the same vote meanwhile.
              seenMeanwhile <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
              if seenMeanwhile
                then pure AlreadyKnown
                else do
                  recordVote st now accepted
                  if now < relayFrom voteRelayWindow
                    then holdVote st (relayFrom voteRelayWindow) entry
                    else relay entry
                  pure (Added acceptedVote)
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
-- slot whose votes are still worth relaying.
type Votes = Map SlotNo ElectionVotes

-- | The votes of one election slot, and until when they are worth relaying.
data ElectionVotes = MkElectionVotes
  { electionRelayUntil :: !RelativeTime
  , electionVotes :: !(Map LeiosSeatId LeiosVote)
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
seenVote st slot seat = maybe False (Map.member seat . electionVotes) . Map.lookup slot <$> readVotes st

-- | Keep a vote the node has not 'seenVote', and forget every election slot
-- whose votes are no longer worth relaying at the given time.
--
-- Forgetting them is safe, as 'addVote' finds any later copy of their votes
-- 'Expired' before it asks whether the node has seen it.
recordVote ::
  IOLike m =>
  VoteState m blk -> RelativeTime -> AcceptedVote -> STM m ()
recordVote st now MkAcceptedVote{acceptedVote, voteElectionSlot, voteRelayWindow} =
  modifyTVar (votes st) $
    Map.filter ((now <=) . electionRelayUntil) . Map.insertWith keepFirst voteElectionSlot election
 where
  election =
    MkElectionVotes
      (relayUntil voteRelayWindow)
      (Map.singleton (voteSeatId acceptedVote) acceptedVote)

  keepFirst new old = old{electionVotes = Map.union (electionVotes old) (electionVotes new)}
