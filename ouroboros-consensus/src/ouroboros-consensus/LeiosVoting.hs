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
import Control.Concurrent.Class.MonadSTM.Strict
  ( modifyTVar
  , newTVar
  , readTChan
  , readTVar
  , retry
  , writeTVar
  )
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
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
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , atomically
  , orElse
  )

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

    -- 'pendingVar' holds EB closures whose voting windows we still owe
    -- a decision on. The 'Ord LeiosPoint' instance orders by slot then
    -- hash, so 'Set.minView' yields the earliest upcoming deadline.
    -- A burst of closures arriving in the same slot no longer
    -- serialises voting through the L_hdr wait for the first one:
    -- each is enqueued as it arrives and drained the moment its
    -- window opens.
    pendingVar <- atomically $ newTVar (Set.empty :: Set LeiosPoint)

    let
      -- Enqueue a fresh acquisition. Retries until the channel has
      -- one available; ignores 'AcquiredEb' (no txs closure yet).
      takeAcquisition :: STM m ()
      takeAcquisition =
        readTChan chan >>= \case
          AcquiredEb{} -> pure ()
          AcquiredEbTxs point -> modifyTVar pendingVar (Set.insert point)

      -- Take the earliest pending point whose L_hdr wait has
      -- elapsed. Retries if the set is empty or the earliest deadline
      -- hasn't opened yet.
      takeReady = do
        pending <- readTVar pendingVar
        case Set.minView pending of
          Nothing -> retry
          Just (point, rest) -> do
            let SlotNo aw = pointSlotNo point
                earliestVoteSlot = SlotNo (aw + lHdrWaitSlots)
            s <- knownSlot btime
            if s < earliestVoteSlot
              then retry
              else do
                writeTVar pendingVar rest
                extLedger <- ChainDB.getCurrentLedger chainDB
                pure (point, s, extLedger)

    -- Wake on whichever fires first: a ready pending, or a new
    -- acquisition. 'orElse' gives priority to voting over ingesting,
    -- so we can't stall a due vote behind a chan drain.
    forever $ do
      mWork <-
        atomically $
          (Just <$> takeReady) `orElse` (Nothing <$ takeAcquisition)
      case mWork of
        Nothing -> pure ()
        Just (point, currentSlot, extLedger) -> do
          let SlotNo aw = pointSlotNo point
              deadlineSlot = SlotNo (aw + lHdrWaitSlots + lVoteWindowSlots)
              notVoted r = traceWith tracer TraceLeiosNotVoted{ebPoint = point, reason = r}
              mVoterId = getLeiosCommittee (ledgerState extLedger) >>= getLeiosVoterId vk
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
