{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
-- vote's deadline, the end of its diffusion period ('voteUntil'). The node
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
  , ValidatedVote (..)
  , validateVote
  , votePruner

    -- * Certificates
  , getCertificate
  , pruneCertificates

    -- * The votes kept
  , currentVotes
  , recordVote
  , pruneVotes
  ) where

import Cardano.Crypto.DSIGN.Class (verifyDSIGN)
import Cardano.Crypto.Leios
  ( AggregationError
  , LeiosCert
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
import Data.Foldable (for_)
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
  , NotifyLog
  , appendNotifyLog
  )
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RankingBlockHash)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , InvalidVoteReason (..)
  , VoteWindow (..)
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
  , writeTVar
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
  = -- | The node had it only after its deadline, or after it forgot its
    -- election.
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
-- not repeat a BLS verification. As verifying takes time, 'recordVote' checks
-- again whether the node has seen or forgotten the vote. The relay log stops
-- relaying it after its deadline.
addVote ::
  (IOLike m, ConvertRawHash blk, GetHeader blk) =>
  VoteState m blk -> SystemTime m -> AcceptedVote -> m AddVoteResult
addVote st systemTime accepted = do
  traceWith (tracer st) (TraceLeiosVoteAdding acceptedVote)
  received <- systemTimeCurrent systemTime
  -- Wait for the window of a vote that 'checkLeiosVotes' accepted early.
  -- NOTE: 'checkLeiosVotes' just rejected votes earlier than the
  -- clock skew, so this waits at most 'InFutureCheck.ClockSkew'.
  let early = voteFrom voteWindow `diffRelTime` received
  when (early >= 0) $ do
    traceWith (tracer st) (TraceLeiosVoteAwaitingWindow acceptedVote early)
    threadDelay (nominalDelay early)
  seen <- atomically $ hasSeenVote st accepted
  -- NOTE: The client only checked that its peer announced it, but the node
  -- does not relay a third announcement of an election, nor one of a stale
  -- OCIN, so it must not relay their votes either.
  announced <-
    atomically $
      hasAnnouncement (announceState st) voteElectionSlot (voteAnnouncingRbHash acceptedVote)
  (result, quorum) <-
    if
      | received > voteUntil voteWindow -> pure (ArrivedTooLate, Nothing)
      | not announced -> pure (NotAnnounced, Nothing)
      | seen -> pure (AlreadyKnown, Nothing)
      | otherwise ->
          -- NOTE: Verifying a signature is expensive, so this comes last.
          case validateVote accepted of
            Left reason -> pure (VoteInvalid reason, Nothing)
            Right validVote -> do
              traceWith (tracer st) (TraceLeiosVoteValidated acceptedVote)
              atomically $ do
                recordVote st validVote >>= \case
                  Left notAdded -> pure (notAdded, Nothing)
                  Right quorum -> do
                    relayVote
                    pure (Added, quorum)
  -- Aggregated outside the transaction, which reaches quorum only once.
  for_ quorum $ \signatures ->
    case aggregateLeiosCert voteCommittee signatures of
      Left err -> traceWith (tracer st) (TraceLeiosAggregationFailed rbHash err)
      Right cert -> do
        atomically $ modifyTVar (certificates st) (Map.insert rbHash (voteElectionSlot, cert))
        traceWith (tracer st) (TraceLeiosCertified rbHash cert)
  pure result
 where
  relayVote =
    appendNotifyLog
      (relayLog st)
      (NotifyVote acceptedVote)
      (voteUntil voteWindow)

  MkAcceptedVote
    { acceptedVote
    , voteElectionSlot
    , voteCommittee
    , voteWindow
    } = accepted

  rbHash = voteAnnouncingRbHash acceptedVote

-- | A vote whose seat has a key in the committee, and whose signature verifies
-- under it.
data ValidatedVote = MkValidatedVote
  { validatedVote :: !LeiosVote
  , validatedElectionSlot :: !SlotNo
  , validatedSeatWeight :: !Weight
  -- ^ The weight of the vote's seat.
  , validatedQuorum :: !Weight
  -- ^ See 'acceptedQuorum'.
  , voteValidUntil :: !RelativeTime
  -- ^ Until when the vote is relayed and considered valid.
  }

-- | Validate a vote against its election's committee, or say why it is invalid.
validateVote :: AcceptedVote -> Either InvalidVoteReason ValidatedVote
validateVote acceptedVote =
  case resolveLeiosSeat voteCommittee voteSeatId of
    Nothing -> Left SeatNotResolvable
    Just LeiosSeat{seatVKey = SNothing} -> Left SeatHasNoKey
    Just LeiosSeat{seatVKey = SJust vk, seatWeight} ->
      case verifyDSIGN leiosSignContext vk voteAnnouncingRbHash voteSignature of
        Left err -> Left . BadSignature $ Text.pack err
        Right () -> Right $ mkValidated seatWeight
 where
  MkAcceptedVote
    { acceptedVote = vote@MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature}
    , voteElectionSlot
    , voteCommittee
    , acceptedQuorum
    , voteWindow
    } = acceptedVote

  mkValidated seatWeight =
    MkValidatedVote
      { validatedVote = vote
      , validatedElectionSlot = voteElectionSlot
      , validatedSeatWeight = seatWeight
      , validatedQuorum = acceptedQuorum
      , voteValidUntil = voteUntil voteWindow
      }

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

-- | Whether the node has a vote of this seat in this election.
hasSeenVote ::
  MonadSTM m =>
  VoteState m blk -> AcceptedVote -> STM m Bool
hasSeenVote st accepted = do
  MkVotes{votesBySlot} <- readTVar (votes st)
  pure $
    maybe
      False
      (Map.member voteSeatId . slotVotes)
      (Map.lookup voteElectionSlot votesBySlot)
 where
  MkAcceptedVote
    { acceptedVote = MkLeiosVote{voteSeatId}
    , voteElectionSlot
    } = accepted

-- | The votes the node keeps, by election slot and seat.
currentVotes ::
  IOLike m =>
  VoteState m blk -> STM m (Map SlotNo (Map LeiosSeatId LeiosVote))
currentVotes st = Map.map slotVotes . votesBySlot <$> readTVar (votes st)

-- | Keep a vote the node has neither seen nor forgotten, as it was pruned past
-- the vote's deadline already. If the votes for its announcement reach quorum,
-- return their signatures.
recordVote ::
  IOLike m =>
  VoteState m blk ->
  ValidatedVote ->
  STM m (Either AddVoteResult (Maybe (Map LeiosSeatId LeiosSignature)))
recordVote st validatedVote = do
  vs <- readTVar (votes st)
  let before = Map.lookup validatedElectionSlot (votesBySlot vs)
      after = maybe slotVotes' (keepFirst slotVotes') before
  if
    | voteValidUntil < votesPrunedAt vs -> pure (Left ArrivedTooLate)
    | maybe False (Map.member (voteSeatId vote) . slotVotes) before -> pure (Left AlreadyKnown)
    | otherwise -> do
        writeTVar (votes st) vs{votesBySlot = Map.insert validatedElectionSlot after (votesBySlot vs)}
        pure . Right $
          if weightOf before < validatedQuorum && validatedQuorum <= weightOf (Just after)
            then
              Just $
                Map.fromList
                  [ (voteSeatId v, voteSignature v)
                  | v <- Map.elems (slotVotes after)
                  , voteAnnouncingRbHash v == rbHash
                  ]
            else Nothing
 where
  MkValidatedVote
    { validatedVote = vote
    , validatedElectionSlot
    , validatedSeatWeight
    , validatedQuorum
    , voteValidUntil
    } = validatedVote

  rbHash = voteAnnouncingRbHash vote

  slotVotes' =
    MkSlotVotes
      voteValidUntil
      (Map.singleton (voteSeatId vote) vote)
      (Map.singleton rbHash validatedSeatWeight)

  weightOf = maybe 0 (Map.findWithDefault 0 rbHash . slotWeights)

  keepFirst new old =
    old
      { slotVotes = Map.union (slotVotes old) (slotVotes new)
      , slotWeights = Map.unionWith (+) (slotWeights old) (slotWeights new)
      }

-- | Forget the votes of every election slot whose deadline has passed at the
-- given time, and return them.
--
-- 'recordVote' then finds any later copy of their votes 'ArrivedTooLate'.
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
