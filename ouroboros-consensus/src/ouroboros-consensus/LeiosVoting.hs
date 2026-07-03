{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Leios voting: cast a vote on each acquired EB, governed by the voting
-- committee selected by the active era from its ledger state.
module LeiosVoting
  ( module LeiosVoting
  , HasLeiosVoting (..)
  ) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTChan, retry)
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( HasLeiosVoting (..)
  , LeiosNotVotedReason (..)
  , LeiosPoint (..)
  , LeiosSigningKey
  , RbHash (..)
  , TraceLeiosKernel (..)
  , getLeiosVoterId
  , signLeiosVote
  )
import LeiosVoteState (AddVoteResult (..), LeiosVoteState (..))
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , WithOrigin (..)
  )
import Ouroboros.Consensus.BlockchainTime
  ( BlockchainTime (..)
  , CurrentSlot (..)
  )
import Ouroboros.Consensus.HeaderValidation
  ( HasAnnTip
  , HeaderState
  , annTipHash
  , headerStateChainDep
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Extended (headerState, ledgerState)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , protocolStateLeiosAnnouncement
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, STM, atomically)

-- * Voting timing constants

--
-- These are stubs for the equivocation-safety and vote-deadline gates
-- described in the Leios protocol specification. Real values should come
-- from the protocol parameters once wired in.

-- | Number of slots after an EB's announcement before its voters are
-- allowed to cast a vote. Serves as the equivocation-detection window: if
-- a peer equivocates by announcing two different EBs on the same slot, we
-- want to observe the second announcement (and drop the vote) before
-- committing. Stub for '3 * L_hdr'.
lHdrWaitSlots :: Word64
lHdrWaitSlots = 3

-- | Number of slots after the vote-eligibility slot ('announcedSlot +
-- lHdrWaitSlots') during which votes are still accepted. Stub for
-- 'L_vote'.
lVoteWindowSlots :: Word64
lVoteWindowSlots = 4

-- * Voting loop

-- | Long-running thread, that issues votes if we have a voting key and are part
-- of the committee.
runLeiosVoting ::
  forall m blk.
  ( IOLike m
  , HasLeiosVoting blk
  , ResolveLeiosBlock blk
  , ConvertRawHash blk
  , HasAnnTip blk
  ) =>
  Tracer m TraceLeiosKernel ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosDbHandle m ->
  LeiosVoteState m ->
  Maybe LeiosSigningKey ->
  m ()
runLeiosVoting tracer chainDB btime leiosDB voteState = \case
  Nothing ->
    traceWith tracer $
      MkTraceLeiosKernel
        "runLeiosVoting: disabled because no topLevelConfigVotingKey"
  Just sk -> do
    let vk = deriveVerKeyDSIGN sk
        signVote = signLeiosVote sk
        LeiosVoteState{addVote} = voteState
    chan <- subscribeEbNotifications leiosDB
    let getNextClosure =
          atomically (readTChan chan) >>= \case
            AcquiredEb{} -> getNextClosure
            AcquiredEbTxs point -> pure point

    -- Per notification: (1) wait for the equivocation window, (2) snapshot
    -- committee + tip's announcement, (3) check the vote deadline, (4)
    -- sign + add the vote.
    --
    -- TODO: This loop is synchronous — the L_hdr wait in (1) blocks the
    -- loop from processing further acquisitions until the current point's
    -- window has passed. Fine when acquisitions arrive at most one per
    -- slot, but a real implementation should be reactive: enqueue each
    -- acquisition with its deadlines, react on chain-extension and
    -- slot-advance events, drain eligible acquisitions concurrently.
    -- That refactor also gives a natural home for equivocation-driven
    -- cancellation.
    forever $ do
      point <- getNextClosure
      let SlotNo aw = pointSlotNo point
          earliestVoteSlot = SlotNo (aw + lHdrWaitSlots)
          deadlineSlot = SlotNo (aw + lHdrWaitSlots + lVoteWindowSlots)
          notVoted r = traceWith tracer TraceLeiosNotVoted{ebPoint = point, reason = r}

      -- (1) Wait for the equivocation-detection window to elapse.
      atomically $ awaitSlot btime earliestVoteSlot

      -- (2) Snapshot everything voting needs in one STM read from the
      -- ledger state: the current slot for the deadline check, the
      -- committee membership for our voter id, and — from the header
      -- state — the currently-selected chain's pending EB announcement
      -- plus the tip's hash (via 'tipAnnouncerFor'). We vote iff the
      -- pending announcement matches the EB whose closure we just
      -- acquired.
      --
      -- TODO: check only once per era whether we are part of the committee?
      (currentSlot, extLedger) <- atomically $ do
        s <- knownSlot btime
        extLedger <- ChainDB.getCurrentLedger chainDB
        pure (s, extLedger)
      let mVoterId = getLeiosCommittee (ledgerState extLedger) >>= getLeiosVoterId vk
          mAnnouncer = tipAnnouncerFor @blk (headerState extLedger) point
      case (currentSlot > deadlineSlot, mAnnouncer, mVoterId) of
        (True, _, _) -> notVoted TooLate
        (_, Nothing, _) -> notVoted ChainTipDoesNotAnnounce
        (_, _, Nothing) -> notVoted NotOnCommittee
        (_, Just rbHash, Just voterId) -> do
          let vote = signVote voterId rbHash
          addVote vote >>= \case
            Added weight mCert -> do
              traceWith tracer TraceLeiosVoted{vote, weight}
              traceWith tracer TraceLeiosVoteAcquired{vote}
              -- Trace certification whenever the tally crosses
              -- 'minCertificationThreshold'. May fire more than once
              -- per point if subsequent votes also come in; consumers
              -- (e.g. ThreadNet's 'propCertifying') dedupe.
              case mCert of
                Just _ -> traceWith tracer TraceLeiosCertified{rbHash}
                Nothing -> pure ()
            err ->
              error $ "runLeiosVoting: unexpected error on addVote: " <> show err

-- | Block until the wall-clock slot reaches the given target. Uses STM
-- 'retry' so it wakes exactly when 'BlockchainTime's slot TVar advances.
awaitSlot :: IOLike m => BlockchainTime m -> SlotNo -> STM m ()
awaitSlot btime target =
  getCurrentSlot btime >>= \case
    CurrentSlot s | s >= target -> pure ()
    _ -> retry

-- | Read the current wall-clock slot, retrying until it is known.
knownSlot :: IOLike m => BlockchainTime m -> STM m SlotNo
knownSlot btime =
  getCurrentSlot btime >>= \case
    CurrentSlot s -> pure s
    CurrentSlotUnknown -> retry

-- | The 'RbHash' of the currently-selected chain's tip iff its most
-- recently applied announcing header announces the given EB and is
-- itself the tip (i.e. the announcer directly extends our selection).
-- Read entirely from the 'HeaderState' — no fragment access needed.
tipAnnouncerFor ::
  forall blk.
  ( ResolveLeiosBlock blk
  , ConvertRawHash blk
  , HasAnnTip blk
  ) =>
  HeaderState blk ->
  LeiosPoint ->
  Maybe RbHash
tipAnnouncerFor hs point = do
  (announcedPoint, _) <- protocolStateLeiosAnnouncement @blk (headerStateChainDep hs)
  NotOrigin tip <- Just (headerStateTip hs)
  -- 'protocolStateLeiosAnnouncement' returns the pending announcement
  -- keyed by the tip's slot; equality with the acquired point (which
  -- carries the announcer's slot + EB hash) means the tip is the
  -- announcer.
  if announcedPoint == point
    then Just (MkRbHash (toRawHash (Proxy @blk) (annTipHash tip)))
    else Nothing
