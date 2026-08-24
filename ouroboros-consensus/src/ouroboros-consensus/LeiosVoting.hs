{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.Except (runExcept)
import Control.Tracer (Tracer, traceWith)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word64)
import LeiosDemoDb
  ( LeiosDbConnection
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , withLeiosDb
  )
import LeiosDemoTypes
  ( HasLeiosVoting (..)
  , LeiosNotVotedReason (..)
  , LeiosPoint (..)
  , LeiosSigningKey
  , RbHash (..)
  , SerializedEbBody
  , TraceLeiosKernel (..)
  , getLeiosSeatId
  , signLeiosVote
  )
import LeiosTxCache (LeiosTxCache (..))
import LeiosVoteState (AddVoteResult (..), LeiosVoteState (..))
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , Point
  , WithOrigin (..)
  )
import Ouroboros.Consensus.BlockchainTime
  ( BlockchainTime (..)
  , CurrentSlot (..)
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HeaderValidation
  ( HasAnnTip
  , HeaderState
  , annTipHash
  , headerStateChainDep
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , LedgerConfig
  , applyChainTick
  , ledgerTipPoint
  )
import Ouroboros.Consensus.Ledger.Extended (headerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , LedgerSupportsMempool (..)
  , WhetherToIntervene (DoNotIntervene)
  )
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ReadOnlyForker (..)
  , ResolveLeiosBlock (..)
  , ledgerStateReadOnlyForker
  , protocolStateLeiosAnnouncement
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , atomically
  , bracket
  , orElse
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (SpecificPoint))

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
  , LedgerSupportsMempool blk
  ) =>
  Tracer m TraceLeiosKernel ->
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosDbHandle m ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosVoteState m ->
  Maybe LeiosSigningKey ->
  m ()
runLeiosVoting tracer cfg chainDB btime leiosDB txCache voteState = \case
  Nothing ->
    traceWith tracer $
      MkTraceLeiosKernel
        "runLeiosVoting: disabled because no topLevelConfigVotingKey"
  -- A 'LeiosDbConnection' is not thread-safe, so this thread owns one for its
  -- lifetime, the way each forge-credentials thread does.
  Just sk -> withLeiosDb leiosDB $ \leiosConn -> do
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
              mVoterId = getLeiosCommittee (ledgerState extLedger) >>= getLeiosSeatId vk
              mAnnouncer = tipAnnouncerFor @blk (headerState extLedger) point
          case (currentSlot > deadlineSlot, mAnnouncer, mVoterId) of
            (True, _, _) -> notVoted TooLate
            (_, Nothing, _) -> notVoted ChainTipDoesNotAnnounce
            (_, _, Nothing) -> notVoted NotOnCommittee
            -- Only now, with every cheap check passed, is it worth paying for
            -- ledger work: we never validate an EB we would not vote for.
            (_, Just rbHash, Just voterId) -> do
              let announcerPoint = ledgerTipPoint (ledgerState extLedger)
              validateEbClosure
                (configLedger cfg)
                chainDB
                leiosConn
                txCache
                point
                announcerPoint
                >>= \case
                  EbClosureNoLedger -> notVoted ChainTipDoesNotAnnounce
                  EbClosureInvalid err -> notVoted $ EbTxsInvalid (Text.pack (show err))
                  EbClosureValid reapplied applied -> do
                    traceWith tracer TraceLeiosEbValidated{ebPoint = point, reapplied, applied}
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

-- * Validating the endorsed transactions

-- | The outcome of checking an EB's endorsed transactions.
data EbClosureVerdict blk
  = -- | Every tx applied, and the txs validated here have been recorded in the
    -- tx-cache. Carries how many were reapplied versus validated in full — the
    -- cache's hit rate.
    EbClosureValid !Int !Int
  | -- | A tx did not apply, so this EB must not be certified.
    EbClosureInvalid !(ApplyTxErr blk)
  | -- | The announcing RB's ledger state was gone before we could read it:
    -- chain-sel moved on. No vote, but nothing is wrong with the EB.
    EbClosureNoLedger

-- | Apply an EB's endorsed transactions to the announcing RB's ledger state,
-- which is the state they will meet if the EB is ever certified.
--
-- Each tx is validated in full, except where the LeiosTxCache reports it
-- already validated: then only the state-dependent checks re-run
-- ('LedgerSupportsMempool.reapplyTx' rather than 'applyTx'). That is where the
-- cache earns its keep — consecutive EBs overlap heavily, and a forger's own EB
-- is entirely pre-validated by its mempool.
validateEbClosure ::
  forall m blk.
  ( IOLike m
  , ResolveLeiosBlock blk
  , LedgerSupportsMempool blk
  ) =>
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosDbConnection m ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosPoint ->
  -- | The announcing RB, whose unticked ledger state the closure applies to.
  Point blk ->
  m (EbClosureVerdict blk)
validateEbClosure lcfg chainDB leiosConn txCache point announcerPoint =
  -- REVIEW: How expensive is it to open a forker - should we re-use one for the
  -- whole vote logic?
  bracket
    (ChainDB.openReadOnlyForkerAtPoint chainDB (SpecificPoint announcerPoint))
    (either (\_ -> pure ()) roforkerClose)
    $ \case
      Left _ -> pure EbClosureNoLedger
      Right extForker -> do
        let forker = ledgerStateReadOnlyForker extForker
        lsBase <- atomically $ roforkerGetLedgerState forker
        -- Load txs from disk
        closure <- resolveLeiosClosure leiosConn (pointEbHash point)
        -- Resolve their input UTxOs
        let keys = foldMap (getTransactionKeySets . snd) closure
        values <- roforkerReadTables forker keys
        -- Determine which txs we can just reapply (the cache hits)
        decided <- withLookupTx txCache $ \look -> mapM (decide look) closure
        let st0 = applyMempoolDiffs values keys (applyChainTick OmitLedgerEvents lcfg slot lsBase)
        goValidate st0 decided 0 0
 where
  -- The EB's slot is also its announcer's, so ticking to it mirrors what the
  -- apply path does when a later RB certifies this EB.
  slot = pointSlotNo point

  -- NOTE: 'assumeValidatedClosureTx' sits on the lookup that licenses it: the
  -- tag is the evidence that this tx was validated before, so the token is
  -- built here and nowhere else.
  decide look (txh, tx) =
    look txh >>= \case
      Just Right{} -> pure (txh, Right (assumeValidatedClosureTx tx))
      _ -> pure (txh, Left tx)

  -- Apply or reapply txs and tag the former as validated in the cache
  goValidate _ [] !reapplied !applied =
    pure $ EbClosureValid reapplied applied
  goValidate !st ((txh, decision) : rest) !reapplied !applied =
    case decision of
      Right vtx ->
        -- Already validated once, so only the state-dependent checks re-run. A
        -- failure here is state-dependent (a spent input, say) and says nothing
        -- about the static checks, so the tx keeps the tag it already has.
        case runExcept $ reapplyTx lcfg slot vtx st of
          Left err -> pure $ EbClosureInvalid err
          Right st' -> goValidate st' rest (reapplied + 1) applied
      Left tx ->
        case runExcept $ applyTx lcfg DoNotIntervene slot tx st of
          Left err -> pure $ EbClosureInvalid err
          Right (st', _vtx) -> do
            recordValidated txh
            goValidate (applyDiffs st st') rest reapplied (applied + 1)

  recordValidated txh =
    withLockedInsertAppliedTx txCache $ \w0 step -> step w0 txh ()

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
