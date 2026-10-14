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

    -- * The votes kept
  , Votes
  , maxVoteElections
  , recordVote
  , wantsVote
  ) where

import Cardano.Crypto.DSIGN.Class (verifyDSIGN)
import Cardano.Crypto.Leios
  ( LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId
  , resolveLeiosSeat
  )
import Control.Monad (unless, when)
import Data.Either (isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemTime (..), diffRelTime)
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, hasRelayedAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  , expiredNotifyEntry
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote (..))
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , ExnLeiosNotifyInvalidVotes (ExnLeiosInvalidVote)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadDelay (threadDelay)
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVar
  , throwIO
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)

-- | The node's votes, and where it learns which announcements it relays and
-- relays the votes.
data VoteState m blk = MkVoteState
  { votes :: !(StrictTVar m Votes)
  , announceState :: !(AnnounceState m blk)
  , relayLog :: !(NotifyLog m blk)
  }

newVoteState :: IOLike m => AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState announceState' notifyLog =
  MkVoteState <$> newTVarIO Map.empty <*> pure announceState' <*> pure notifyLog

-- | Accept a vote, and relay it if it is the first valid vote of its seat in
-- its election, the node relays the announcement it is for, and it has not
-- expired; see 'expiredNotifyEntry'.
--
-- Waits until the vote's window opens to relay a vote that arrived early.
--
-- Throws 'ExnLeiosInvalidVote' if the vote is invalid, to disconnect the peer
-- that sent it. The signature is verified between two transactions, so that a
-- retry does not repeat a BLS verification.
onAcceptedVote :: IOLike m => VoteState m blk -> SystemTime m -> AcceptedVote -> m ()
onAcceptedVote st systemTime accepted =
  -- a peer accepts a late vote only within its grace, which relaying would eat into
  whenUnexpired $ do
    worthVerifying <- atomically $ do
      wanted <- isWanted
      relayedAnnouncement <-
        hasRelayedAnnouncement (announceState st) (voteAnnouncingRbHash acceptedVote)
      pure (wanted && relayedAnnouncement)
    when worthVerifying $ do
      unless (validVote voteCommittee acceptedVote) $
        throwIO $
          ExnLeiosInvalidVote (voteAnnouncingRbHash acceptedVote) (voteSeatId acceptedVote)
      -- a peer whose clock is behind ours would reject it, or one relaying it
      now <- systemTimeCurrent systemTime
      when (now < voteWindowStart) $
        threadDelay (nominalDelay (diffRelTime voteWindowStart now))
      whenUnexpired $ atomically $ do
        -- another peer may have delivered the same vote meanwhile
        wanted <- isWanted
        when wanted $ do
          modifyTVar (votes st) (recordVote voteElectionSlot acceptedVote)
          appendNotifyLog (relayLog st) entry
 where
  whenUnexpired act = do
    now <- systemTimeCurrent systemTime
    unless (expiredNotifyEntry now entry) act

  MkAcceptedVote
    { acceptedVote
    , voteElectionSlot
    , voteElectionOnset
    , voteWindowStart
    , voteWindowEnd
    , voteCommittee
    } =
      accepted

  entry = NotifyVote voteElectionOnset voteWindowEnd acceptedVote

  isWanted = wantsVote voteElectionSlot (voteSeatId acceptedVote) <$> readTVar (votes st)

-- | Whether the vote's seat has a key in the committee, and the signature
-- verifies under it.
validVote :: LeiosCommittee -> LeiosVote -> Bool
validVote committee MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature} =
  case resolveLeiosSeat committee voteSeatId of
    Just LeiosSeat{seatVKey = SJust vk} ->
      isRight $ verifyDSIGN () vk voteAnnouncingRbHash voteSignature
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

-- | Whether the node has no vote of this seat in this election slot yet, and
-- would keep one: once full, it keeps no slot older than all it has.
wantsVote :: SlotNo -> LeiosSeatId -> Votes -> Bool
wantsVote slot seat vs = case Map.lookup slot vs of
  Just seats -> Map.notMember seat seats
  Nothing -> Map.size vs < maxVoteElections || maybe True ((< slot) . fst) (Map.lookupMin vs)

-- | Record a vote 'wantsVote' wants, forgetting the oldest election slot if
-- there are then more than 'maxVoteElections'.
recordVote :: SlotNo -> LeiosVote -> Votes -> Votes
recordVote slot vote vs
  | Map.size vs' > maxVoteElections = Map.deleteMin vs'
  | otherwise = vs'
 where
  vs' = Map.insertWith Map.union slot (Map.singleton (voteSeatId vote) vote) vs
