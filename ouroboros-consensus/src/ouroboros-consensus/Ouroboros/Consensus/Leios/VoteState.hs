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
import Control.Monad (unless, when)
import Data.Either (isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, SlotNo)
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, hasAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote (..))
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , ExnLeiosNotifyInvalidVotes (ExnLeiosInvalidVote)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVar
  , throwIO
  )

-- | The node's votes, and where it learns which announcements it relays and
-- relays the votes.
data VoteState m blk = MkVoteState
  { votes :: !(StrictTVar m Votes)
  , announceState :: !(AnnounceState m blk)
  -- ^ Which announcements the node relays, as it relays only votes for those.
  , relayLog :: !(NotifyLog m blk)
  -- ^ Where the node relays the votes it adds.
  }

newVoteState :: IOLike m => AnnounceState m blk -> NotifyLog m blk -> m (VoteState m blk)
newVoteState announceState' notifyLog =
  MkVoteState <$> newTVarIO Map.empty <*> pure announceState' <*> pure notifyLog

-- | Accept a vote, and relay it if it is the first valid vote of its seat in
-- its election and the node relays the announcement it is for.
--
-- Throws 'ExnLeiosInvalidVote' if the vote is invalid, to disconnect the peer
-- that sent it. The signature is verified between two transactions, so that a
-- retry does not repeat a BLS verification.
onAcceptedVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) => VoteState m blk -> AcceptedVote -> m ()
onAcceptedVote st MkAcceptedVote{acceptedVote, voteElectionSlot, voteDiffusionEnd, voteCommittee} = do
  worthVerifying <- atomically $ do
    seen <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
    announced <-
      hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
    pure (not seen && announced)
  when worthVerifying $ do
    unless (validVote voteCommittee acceptedVote) $
      throwIO $
        ExnLeiosInvalidVote (voteAnnouncingRbHash acceptedVote) (voteSeatId acceptedVote)
    atomically $ do
      -- Another peer may have delivered the same vote meanwhile.
      seen <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
      unless seen $ do
        recordVote st voteElectionSlot acceptedVote
        appendNotifyLog (relayLog st) (NotifyVote voteDiffusionEnd acceptedVote)
 where

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
