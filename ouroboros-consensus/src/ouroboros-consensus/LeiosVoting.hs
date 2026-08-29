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
import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict
  ( modifyTVar
  , newTVar
  , readTChan
  , readTVar
  )
import Control.Monad (filterM, foldM, forever, when)
import qualified Control.Monad.Class.MonadSTM.Internal as TVar
import Control.Monad.Class.MonadTimer (MonadTimer, registerDelay)
import Control.Monad.Class.MonadTimer.SI (diffTimeToMicrosecondsAsInt)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
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
  , prettyLeiosPoint
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
  ( RelativeTime
  , SystemTime (..)
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import Ouroboros.Consensus.HeaderValidation
  ( HasAnnTip
  , HeaderState
  , annTipHash
  , headerStateChainDep
  , headerStateTip
  )
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , EmptyMK
  , KeysMK
  , LedgerConfig
  , LedgerState
  , LedgerTables
  , ValuesMK
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Extended (headerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , LedgerSupportsMempool (..)
  , WhetherToIntervene (Intervene)
  )
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ReadOnlyForker (..)
  , ReadOnlyForker'
  , ResolveLeiosBlock (..)
  , ledgerStateReadOnlyForker
  , protocolStateLeiosAnnouncement
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , atomically
  , bracket
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))

-- * Voting timing constants

--
-- These are stubs for the equivocation-safety and vote-deadline gates
-- described in the Leios protocol specification. Real values should come
-- from the protocol parameters once wired in.

-- Validation is linear in the closure -- a full ~13.5k-tx closure takes ~1.5s
-- even when every tx is a tx-cache hit -- so doing it inside the vote window
-- spends the window on work that could have been done before it opened. A devnet
-- run showed exactly that: as closures filled up, the time from votable to
-- validated grew past the window and voting stopped entirely. Two independent
-- ways to stop paying for it here:
--
-- TODO: validate as soon as the whole closure has arrived, rather than waiting
-- for the vote window to open.
--
-- TODO: validate the closure as it streams in, rather than waiting for all of
-- it. Subsumes the above, and is considerably more involved.

-- | How long after its announcing slot /begins/ before an EB's voters may cast
-- a vote. Serves as the equivocation-detection window: if a peer equivocates by
-- announcing two different EBs on the same slot, we want to observe the second
-- announcement (and drop the vote) before issuing our vote. Stub for
-- '3 * L_hdr'.
--
-- TODO: read from the ledger state, once the Leios protocol parameters are
-- there; a constant cannot follow an on-chain change.
lHdrWait :: NominalDiffTime
lHdrWait = 3

-- | How long after 'lHdrWait' votes are still accepted. Stub for 'L_vote'.
--
-- TODO: read from the ledger state, as for 'lHdrWait'.
lVoteWindow :: NominalDiffTime
lVoteWindow = 4

-- * Vote timers

-- | A vote timer per acquired EB, armed at the wall-clock moment that EB's
-- voting window opens.
--
-- There is no priority queue: the wall clock does the prioritising, since
-- whichever timer fires first is by construction the EB whose window opens
-- first.
data VoteTimers m = VoteTimers
  { scheduleVoteTime :: LeiosPoint -> m ()
  -- ^ Arm a timer for a newly acquired EB. One whose window is already open
  -- clamps to a near-zero delay and fires immediately, so "already open" is not
  -- a separate case for the caller.
  , waitNextVoteTime :: STM m (LeiosPoint, RelativeTime)
  -- ^ Block until an armed EB's window opens, yielding it and the moment after
  -- which a vote for it would be too late. It is disarmed in the same
  -- transaction, so each EB's window opens exactly once.
  }

newVoteTimers ::
  forall m blk.
  ( IOLike m
  , MonadTimer m
  , HasHardForkHistory blk
  ) =>
  Tracer m TraceLeiosKernel ->
  LedgerConfig blk ->
  ChainDB m blk ->
  SystemTime m ->
  m (VoteTimers m)
newVoteTimers tracer lcfg chainDB systemTime = do
  -- An armed timer, and the deadline it was armed against, per acquired EB.
  pendingVotes <- atomically $ newTVar Map.empty
  pure
    VoteTimers
      { scheduleVoteTime = \point -> do
          extLedger <- atomically $ ChainDB.getCurrentLedger chainDB
          case slotOnset lcfg (ledgerState extLedger) (pointSlotNo point) of
            -- Cannot happen for an EB announced on our own chain: its slot is
            -- at or behind the tip whose summary we just read. Drop it rather
            -- than arm a timer we cannot place, and say so.
            Left horizon ->
              traceWith tracer . MkTraceLeiosKernel $
                "newVoteTimers: no wall-clock onset for "
                  <> prettyLeiosPoint point
                  <> ": "
                  <> show horizon
            Right announced -> do
              now <- systemTimeCurrent systemTime
              let voteAt = addRelTime lHdrWait announced
                  voteDeadline = addRelTime lVoteWindow voteAt
                  voteIn = voteAt `diffRelTime` now
              traceWith tracer $
                TraceLeiosVoteScheduled
                  { ebPoint = point
                  , voteIn
                  , deadlineIn = voteDeadline `diffRelTime` now
                  }
              timer <-
                registerDelay
                  . diffTimeToMicrosecondsAsInt
                  $ nominalDelay voteIn
              atomically
                . modifyTVar pendingVotes
                . Map.insert point
                $ (timer, voteDeadline)
      , -- Reading every armed timer puts them all in this transaction's read
        -- set, so it wakes on any of them, and 'Map.toList' is in point order,
        -- so simultaneous firings break towards the earlier slot.
        waitNextVoteTime = do
          armed <- readTVar pendingVotes
          filterM (\(_, (timer, _)) -> TVar.readTVar timer) (Map.toList armed) >>= \case
            [] -> TVar.retry
            (point, (_, deadline)) : _ -> do
              modifyTVar pendingVotes (Map.delete point)
              pure (point, deadline)
      }

-- | When a slot begins, in wall-clock terms.
--
-- Both gates are durations -- the protocol parameters they stub for are diff
-- times, not slot counts -- so they are measured from the announcing slot's
-- onset rather than counted against the current slot.
slotOnset ::
  HasHardForkHistory blk =>
  LedgerConfig blk ->
  LedgerState blk mk ->
  SlotNo ->
  Either Qry.PastHorizonException RelativeTime
slotOnset lcfg lst slot =
  fst <$> Qry.runQuery (Qry.slotToWallclock slot) (hardForkSummary lcfg lst)

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
  , HasHardForkHistory blk
  , MonadTimer m
  ) =>
  Tracer m TraceLeiosKernel ->
  LedgerConfig blk ->
  ChainDB m blk ->
  SystemTime m ->
  LeiosDbHandle m ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosVoteState m ->
  Maybe LeiosSigningKey ->
  m ()
runLeiosVoting tracer lcfg chainDB systemTime leiosDB txCache voteState = \case
  Nothing ->
    traceWith tracer $
      MkTraceLeiosKernel
        "runLeiosVoting: disabled because no topLevelConfigVotingKey"
  Just sk ->
    -- A 'LeiosDbConnection' is not thread-safe, so this thread owns one for its
    -- lifetime, the way each forge-credentials thread does.
    withLeiosDb leiosDB $ \leiosConn -> do
      chan <- subscribeEbNotifications leiosDB
      let waitAcquiredEbTxs =
            readTChan chan >>= \case
              AcquiredEb{} -> waitAcquiredEbTxs
              AcquiredEbTxs point -> pure point

      VoteTimers{scheduleVoteTime, waitNextVoteTime} <-
        newVoteTimers tracer lcfg chainDB systemTime

      forever $
        atomically ((Left <$> waitAcquiredEbTxs) <|> (Right <$> waitNextVoteTime))
          >>= \case
            Left point -> scheduleVoteTime point
            Right (point, deadline) ->
              goVote leiosConn sk point deadline >>= \case
                Left reason -> traceWith tracer TraceLeiosNotVoted{ebPoint = point, reason}
                Right () -> pure ()
 where
  -- Decide whether to vote for an acquired EB and, if we may, cast the
  -- vote. Every way of not voting leaves via 'throwE', and the reason is
  -- traced once, here.
  --
  -- Validation runs /before/ the deadline is checked, deliberately: applying
  -- a closure takes real time, so whether we are still inside the vote
  -- window has to be judged by the clock as it reads when we are ready to
  -- sign, not by a reading taken before the work. The cheap checks come
  -- first, so an EB we would not vote for is never validated.
  goVote ::
    LeiosDbConnection m ->
    -- \| Our voting key, to find our committee seat and sign votes.
    LeiosSigningKey ->
    -- \| The leios point of the EB to vote on.
    LeiosPoint ->
    -- \| The moment after which a vote is too late.
    RelativeTime ->
    m (Either LeiosNotVotedReason ())
  goVote leiosConn sk point deadline = do
    let vk = deriveVerKeyDSIGN sk
    withForkerAt VolatileTip $ \case
      Nothing ->
        -- Should not happen: VolatileTip target never fails to open. It's safe
        -- to interpret this non-typed corner case the same way as if we were
        -- not on the announcing chain.
        pure $ Left ChainTipDoesNotAnnounce
      Just forker -> runExceptT $ do
        -- One consistent view of the selection: the header state that has
        -- to announce this EB and the ledger state its closure has to apply
        -- to come from the same forker.
        extLs <- lift $ atomically $ roforkerGetLedgerState forker
        let ls = ledgerState extLs
            hs = headerState extLs
            readTables = roforkerReadTables (ledgerStateReadOnlyForker forker)
        rbHash <-
          tipAnnouncerFor @blk hs point ?>= ChainTipDoesNotAnnounce
        seatId <-
          (getLeiosCommittee ls >>= getLeiosSeatId vk) ?>= NotOnCommittee

        lift (validateEbClosure lcfg leiosConn txCache readTables point ls) >>= \case
          EbClosureInvalid err ->
            -- TODO: Text in error
            throwE . EbTxsInvalid . Text.pack $ show err
          EbClosureValid reapplied applied ->
            lift $ traceWith tracer TraceLeiosEbValidated{ebPoint = point, reapplied, applied}

        now <- lift $ systemTimeCurrent systemTime
        when (now > deadline) $
          throwE TooLate

        let vote = signLeiosVote sk seatId rbHash
        lift $
          addVote vote
            >>= \case
              Added weight mCert -> do
                traceWith tracer TraceLeiosVoted{vote, weight}
                traceWith tracer TraceLeiosVoteAcquired{vote}
                -- Trace certification whenever the tally crosses
                -- 'minCertificationThreshold'. May fire more than once per
                -- point if subsequent votes also come in; consumers (e.g.
                -- ThreadNet's 'propCertifying') dedupe.
                case mCert of
                  Just _ -> traceWith tracer TraceLeiosCertified{rbHash}
                  Nothing -> pure ()
              err ->
                -- TODO: Make this a NotVoted error / trace
                error $ "runLeiosVoting: unexpected error on addVote: " <> show err

  LeiosVoteState{addVote} = voteState

  -- The forker at a target, for as long as the continuation needs it, or
  -- 'Nothing' when chain-sel has already moved off it.
  withForkerAt :: Target (Point blk) -> (Maybe (ReadOnlyForker' m blk) -> m a) -> m a
  withForkerAt tgt k =
    bracket
      (ChainDB.openReadOnlyForkerAtPoint chainDB tgt)
      (either (\_ -> pure ()) roforkerClose)
      (either (\_ -> k Nothing) (k . Just))

-- | The outcome of checking an EB's endorsed transactions.
data EbClosureVerdict blk
  = -- | Every tx applied, and the txs validated here have been recorded in the
    -- tx-cache. Carries how many were reapplied versus validated in full — the
    -- cache's hit rate.
    EbClosureValid !Int !Int
  | -- | A tx did not apply, so this EB must not be certified. Any valid prefix
    -- of txs is recorded as valid in the tx-cache.
    EbClosureInvalid !(ApplyTxErr blk)

-- | Apply an EB's endorsed transactions to the announcing RB's ledger state,
-- which is the state they will meet if the EB is ever certified.
--
-- Each tx is validated in full, except where the LeiosTxCache reports it
-- already validated: then only the state-dependent checks re-run
-- ('LedgerSupportsMempool.reapplyTx' rather than 'applyTx').
validateEbClosure ::
  forall m blk.
  ( IOLike m
  , ResolveLeiosBlock blk
  , LedgerSupportsMempool blk
  ) =>
  LedgerConfig blk ->
  LeiosDbConnection m ->
  LeiosTxCache m () () SerializedEbBody ->
  -- | Read the ledger tables the closure's txs need, as
  -- 'resolveAndApplyLeiosClosure' does on the apply path.
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LeiosPoint ->
  -- | The announcing RB's unticked ledger state, which the closure applies to.
  LedgerState blk EmptyMK ->
  m (EbClosureVerdict blk)
validateEbClosure lcfg leiosConn txCache resolveValues point lsBase = do
  -- Load txs from disk
  closure <- resolveLeiosClosure leiosConn (pointEbHash point)
  -- Resolve their input UTxOs
  -- TODO: This collects ALL inputs, but we would just need the inputs of the
  -- transitive closure. That is, any chained txs would only require inputs not
  -- internal to the chain.
  let keys = foldMap (getTransactionKeySets . snd) closure
  values <- resolveValues keys
  -- Determine which txs we can just reapply (the cache hits)
  decided <- withLookupTx txCache $ \look -> mapM (decide look) closure
  -- TODO: Temporarily use the Mempool API until we have a dedicated one
  let st0 = applyMempoolDiffs values keys (applyChainTick OmitLedgerEvents lcfg slot lsBase)
  goValidate st0 decided 0 0 []
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

  -- Apply or reapply txs, accumulating the ones we newly validated so the cache
  -- is written once for the whole closure rather than once per tx.
  goValidate _ [] !reapplied !applied newly = do
    recordValidated newly
    pure $ EbClosureValid reapplied applied
  goValidate !st ((txh, decision) : rest) !reapplied !applied newly =
    case decision of
      Right vtx ->
        -- Already validated once, so only the state-dependent checks re-run. A
        -- failure here is state-dependent (a spent input, say) and says nothing
        -- about the static checks, so the tx keeps the tag it already has.
        case runExcept $ reapplyTx lcfg slot vtx st of
          Left err -> abandon newly err
          Right st' -> goValidate st' rest (reapplied + 1) applied newly
      Left tx ->
        -- 'Intervene', despite the mempool's framing of it as a courtesy to
        -- local clients. What it actually selects is whether the era's applyTx
        -- may rewrite the tx's 'isValid' flag: 'DoNotIntervene' forces the flag
        -- true and, on a tag mismatch, retries with it false, so a Plutus tx
        -- whose scripts fail would be validated here as a collateral-only tx
        -- and voted for. The apply path does not do that -- it runs with
        -- 'ValidateNone' and branches on the /declared/ flag -- so it would
        -- consume the tx's whole input set instead. Rewriting the flag here
        -- would mean voting for a closure the chain never applies, which is
        -- exactly the trust 'applyLeiosClosure' cites the certificate for.
        -- 'Intervene' leaves the flag alone and lets the mismatch escape as an
        -- error, so we fail closed on precisely those txs.
        case runExcept $ applyTx lcfg Intervene slot tx st of
          Left err -> abandon newly err
          Right (st', _vtx) ->
            goValidate (applyDiffs st st') rest reapplied (applied + 1) (txh : newly)

  -- A closure that fails part-way still validated everything before the failure,
  -- against a real ledger state, so those tags are kept: the work holds
  -- regardless of the tx that sank the EB.
  abandon newly err = do
    recordValidated newly
    pure $ EbClosureInvalid err

  -- One lock acquisition per closure. Order is irrelevant -- each entry is an
  -- independent tag -- so the accumulator is not reversed.
  recordValidated newly
    | null newly = pure ()
    | otherwise =
        withLockedInsertAppliedTx txCache $ \w0 step ->
          foldM (\w txh -> step w txh ()) w0 newly

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

(?>=) :: Monad m => Maybe a -> e -> ExceptT e m a
(?>=) Nothing e = throwE e
(?>=) (Just x) _ = pure x
