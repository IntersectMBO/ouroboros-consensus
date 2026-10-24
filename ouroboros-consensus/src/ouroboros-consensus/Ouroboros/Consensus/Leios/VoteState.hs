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
--
-- So the node aggregates the votes for an announcement into its certificate as
-- soon as they reach quorum, which is by their deadline or never. It keeps the
-- certificate until the announcing block is immutable, long after it forgot
-- the votes, for whichever block comes to certify it.
module Ouroboros.Consensus.Leios.VoteState
  ( VoteState
  , newVoteState
  , TraceLeiosVoteState (..)
  , AddVoteResult (..)
  , addVote
  , votePruner

    -- * Certificates
  , getCertificate
  , pruneCertificates

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
  ( AggregationError
  , LeiosCert
  , LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId
  , LeiosSignature
  , Weight
  , aggregateLeiosCert
  , leiosSignContext
  , resolveLeiosSeat
  )
import Control.Monad (forever, when)
import Control.Tracer (Tracer, traceWith)
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, hasAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyEntry (..)
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RankingBlockHash)
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
  , modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )

-- | The node's votes, and where it learns which announcements it relays and
-- relays the votes.
data VoteState m blk = MkVoteState
  { votes :: !(StrictTVar m Votes)
  , announceState :: !(AnnounceState m blk)
  -- ^ Which announcements the node relays, as it relays only votes for those.
  , relayLog :: !(NotifyLog m blk)
  -- ^ Where the node relays the votes it adds.
  , certificates :: !(StrictTVar m (Map RankingBlockHash (SlotNo, LeiosCert)))
  -- ^ The certificate of each announcement whose votes reached quorum, with
  -- its election slot; see 'pruneCertificates'.
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
    <*> newTVarIO Map.empty
    <*> pure tracer'

-- | What the node's 'VoteState' traces.
data TraceLeiosVoteState
  = -- | The node verified the signature of a vote, and has yet to add it.
    TraceLeiosVoteVerified !LeiosVote
  | -- | The node had a vote only after its deadline, so it neither keeps nor
    -- relays it. Many of these mean the diffusion period is too short for the
    -- network, or that the node verifies votes too slowly.
    TraceLeiosVoteArrivedTooLate !LeiosVote
  | -- | The votes for this announcement reached quorum, and the node aggregated
    -- them into its certificate.
    TraceLeiosCertified !RankingBlockHash !LeiosCert
  | -- | The node could not aggregate the votes for this announcement, though
    -- they reached quorum.
    TraceLeiosAggregationFailed !RankingBlockHash !AggregationError
  | -- | The deadline of the votes for this announcement passed with only this
    -- much weight voting for it, short of quorum. Many of these, for
    -- announcements of valid endorser blocks, mean the diffusion period is too
    -- short for the network.
    TraceLeiosQuorumMissed !RankingBlockHash !Weight
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
  | -- | The node keeps the vote and relays it.
    Added
  deriving stock (Eq, Show)

-- | Add a vote, if it is the first valid vote of its seat in its election, for
-- an announcement the node relays, and the node has it by its deadline.
--
-- The vote's window must have opened; see 'awaitVoteRelayWindow'.
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
  (result, quorum) <-
    if
      | seen -> pure (AlreadyKnown, Nothing)
      | received > relayUntil voteRelayWindow -> pure (ArrivedTooLate, Nothing)
      | not announced -> pure (NotAnnounced, Nothing)
      | not (validVote voteCommittee acceptedVote) -> pure (VoteInvalid, Nothing)
      | otherwise -> do
          traceWith (tracer st) (TraceLeiosVoteVerified acceptedVote)
          now <- systemTimeCurrent systemTime
          atomically $ do
            MkVotes{votesPrunedAt} <- readVotes st
            seenMeanwhile <- seenVote st voteElectionSlot (voteSeatId acceptedVote)
            if
              -- Another peer may have delivered the same vote meanwhile.
              | seenMeanwhile -> pure (AlreadyKnown, Nothing)
              -- The pruner may have forgotten its election slot already, and
              -- with it whether the node relayed the vote, if 'now' is stale.
              | now > relayUntil voteRelayWindow || relayUntil voteRelayWindow < votesPrunedAt ->
                  pure (ArrivedTooLate, Nothing)
              | otherwise -> do
                  quorum <- recordVote st accepted
                  appendNotifyLog (relayLog st) $
                    MkNotifyEntry (relayUntil voteRelayWindow) (NotifyVote acceptedVote)
                  pure (Added, quorum)
  when (result == ArrivedTooLate) $
    traceWith (tracer st) (TraceLeiosVoteArrivedTooLate acceptedVote)
  -- Aggregated outside the transaction, which reaches quorum only once.
  for_ quorum $ \signatures ->
    case aggregateLeiosCert voteCommittee signatures of
      Left err -> traceWith (tracer st) (TraceLeiosAggregationFailed rbHash err)
      Right cert -> do
        atomically $ modifyTVar (certificates st) (Map.insert rbHash (voteElectionSlot, cert))
        traceWith (tracer st) (TraceLeiosCertified rbHash cert)
  pure result
 where
  MkAcceptedVote
    { acceptedVote
    , voteElectionSlot
    , voteCommittee
    , voteRelayWindow
    } = accepted

  rbHash = voteAnnouncingRbHash acceptedVote

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
  , slotWeights :: !(Map RankingBlockHash Weight)
  -- ^ The weight of the votes for each announcement.
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

-- | Keep a vote the node has not 'seenVote'. If the votes for its announcement
-- thereby reach quorum, return their signatures.
recordVote ::
  IOLike m =>
  VoteState m blk -> AcceptedVote -> STM m (Maybe (Map LeiosSeatId LeiosSignature))
recordVote st MkAcceptedVote{acceptedVote, voteElectionSlot, voteCommittee, voteQuorum, voteRelayWindow} = do
  vs <- readTVar (votes st)
  let before = Map.lookup voteElectionSlot (votesBySlot vs)
      after = maybe slotVotes' (keepFirst slotVotes') before
  writeTVar (votes st) vs{votesBySlot = Map.insert voteElectionSlot after (votesBySlot vs)}
  pure $
    if weightOf before < voteQuorum && voteQuorum <= weightOf (Just after)
      then
        Just $
          Map.fromList
            [ (voteSeatId v, voteSignature v)
            | v <- Map.elems (slotVotes after)
            , voteAnnouncingRbHash v == rbHash
            ]
      else Nothing
 where
  rbHash = voteAnnouncingRbHash acceptedVote

  slotVotes' =
    MkSlotVotes
      (relayUntil voteRelayWindow)
      (Map.singleton (voteSeatId acceptedVote) acceptedVote)
      (Map.singleton rbHash seatWeight')

  seatWeight' = maybe 0 seatWeight (resolveLeiosSeat voteCommittee (voteSeatId acceptedVote))

  weightOf = maybe 0 (Map.findWithDefault 0 rbHash . slotWeights)

  keepFirst new old =
    old
      { slotVotes = Map.union (slotVotes old) (slotVotes new)
      , slotWeights = Map.unionWith (+) (slotWeights old) (slotWeights new)
      }

-- | Forget the votes of every election slot whose deadline has passed at the
-- given time, and return them.
--
-- 'addVote' then finds any later copy of their votes 'ArrivedTooLate', before
-- it asks whether the node has seen it.
pruneVotes ::
  IOLike m =>
  VoteState m blk -> RelativeTime -> STM m (Map SlotNo SlotVotes)
pruneVotes st now = do
  MkVotes{votesPrunedAt, votesBySlot} <- readTVar (votes st)
  let (unexpired, expired) = Map.partition ((now <=) . slotRelayUntil) votesBySlot
  writeTVar (votes st) (MkVotes (max now votesPrunedAt) unexpired)
  pure expired

-- | Prune the votes every second; see 'pruneVotes'.
votePruner :: IOLike m => VoteState m blk -> SystemTime m -> m Void
votePruner st systemTime = forever $ do
  now <- systemTimeCurrent systemTime
  missed <- atomically $ do
    expired <- pruneVotes st now
    certified <- readTVar (certificates st)
    pure
      [ (rbHash, weight)
      | slotVotes' <- Map.elems expired
      , (rbHash, weight) <- Map.toList (slotWeights slotVotes')
      , Map.notMember rbHash certified
      ]
  for_ missed $ \(rbHash, weight) -> traceWith (tracer st) (TraceLeiosQuorumMissed rbHash weight)
  threadDelay 1

{-------------------------------------------------------------------------------
  Certificates
-------------------------------------------------------------------------------}

-- | The certificate of this announcement, if the votes for it reached quorum by
-- their deadline.
getCertificate ::
  IOLike m =>
  VoteState m blk -> RankingBlockHash -> STM m (Maybe LeiosCert)
getCertificate st rbHash = fmap snd . Map.lookup rbHash <$> readTVar (certificates st)

-- | Forget the certificates of every election slot before the given slot, the
-- immutable tip's.
--
-- The announcing block of such a certificate is immutable, and so is the block
-- after it, which certified the announcement or did not. Until then, a
-- certificate is kept for whichever block comes to certify it, however late.
pruneCertificates ::
  IOLike m =>
  VoteState m blk -> SlotNo -> STM m ()
pruneCertificates st immTipSlot =
  modifyTVar (certificates st) (Map.filter ((immTipSlot <=) . fst))
