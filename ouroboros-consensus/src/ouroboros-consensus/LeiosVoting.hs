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
  , tryReadTChan
  )
import Control.Monad (forM_, forever)
import qualified Control.Monad.Class.MonadSTM.Internal as TVar
import Control.Monad.Class.MonadTimer (MonadTimer, registerDelay)
import Control.Monad.Class.MonadTimer.SI (diffTimeToMicrosecondsAsInt)
import Control.Monad.Except (runExcept)
import Control.Tracer (Tracer, traceWith)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
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
  , LeiosSeatId
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
  , WithOrigin (..)
  )
import Ouroboros.Consensus.BlockchainTime
  ( RelativeTime
  , SystemTime (..)
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
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
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (SpecificPoint))

-- * Voting timing constants

--
-- These are stubs for the equivocation-safety and vote-deadline gates
-- described in the Leios protocol specification. Real values should come
-- from the protocol parameters once wired in.

-- | How long after its announcing slot /begins/ before an EB's voters may cast
-- a vote. Serves as the equivocation-detection window: if a peer equivocates by
-- announcing two different EBs on the same slot, we want to observe the second
-- announcement (and drop the vote) before committing. Stub for '3 * L_hdr'.
lHdrWait :: NominalDiffTime
lHdrWait = 3

-- | How long after 'lHdrWait' votes are still accepted. Stub for 'L_vote'.
lVoteWindow :: NominalDiffTime
lVoteWindow = 4

-- | When a slot begins, in wall-clock terms.
--
-- Both gates are measured from here rather than counted in slots: the node's
-- notion of the current slot only advances as blocks are adopted, so slot
-- arithmetic would judge the deadline against a clock that stops whenever the
-- chain does.
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
  TopLevelConfig blk ->
  ChainDB m blk ->
  SystemTime m ->
  LeiosDbHandle m ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosVoteState m ->
  Maybe LeiosSigningKey ->
  m ()
runLeiosVoting tracer cfg chainDB systemTime leiosDB txCache voteState = \case
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

      -- Move whatever has already arrived into the pending set, blocking only
      -- when there is nothing pending to act on at all.
      ingest :: STM m ()
      ingest = do
        pending <- readTVar pendingVar
        if Set.null pending then takeAcquisition else drain
       where
        drain =
          tryReadTChan chan >>= \case
            Nothing -> pure ()
            Just AcquiredEb{} -> drain
            Just (AcquiredEbTxs point) -> do
              modifyTVar pendingVar (Set.insert point)
              drain

      -- Everything the node has to be asked about before we can decide, read
      -- as one consistent view of the selected chain.
      readTip = atomically $ ChainDB.getCurrentLedger chainDB

      -- Validate an EB's closure against the announcing RB's ledger state.
      -- Losing that state to chain-sel is not the EB's fault, so it reads as
      -- 'ChainTipDoesNotAnnounce' rather than a rejection.
      validateClosure extLedger point =
        -- REVIEW: How expensive is it to open a forker - should we re-use
        -- one for the whole vote logic?
        -- TODO: refactor: withForker announcerPoint $ \(ls, forker) ->
        bracket
          (ChainDB.openReadOnlyForkerAtPoint chainDB (SpecificPoint (ledgerTipPoint (ledgerState extLedger))))
          (either (\_ -> pure ()) roforkerClose)
          $ \case
            Left _ -> pure $ Left ChainTipDoesNotAnnounce
            Right extForker -> do
              let forker = ledgerStateReadOnlyForker extForker
              lsBase <- atomically $ roforkerGetLedgerState forker
              validateEbClosure
                (configLedger cfg)
                leiosConn
                txCache
                (roforkerReadTables forker)
                point
                lsBase
                >>= \case
                  -- XXX: Text in error
                  EbClosureInvalid err -> pure $ Left $ EbTxsInvalid $ Text.pack $ show err
                  EbClosureValid reapplied applied -> pure $ Right (reapplied, applied)

      -- Decide and, if we may, cast the vote.
      voteOn point extLedger deadline = do
        let notVoted r = traceWith tracer TraceLeiosNotVoted{ebPoint = point, reason = r}
        decideVote
          (getLeiosCommittee (ledgerState extLedger) >>= getLeiosSeatId vk)
          (tipAnnouncerFor @blk (headerState extLedger) point)
          (validateClosure extLedger point)
          (systemTimeCurrent systemTime)
          deadline
          >>= \case
            Left reason -> notVoted reason
            Right (rbHash, voterId, (reapplied, applied)) -> do
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

    forever $ do
      atomically ingest
      mNext <- atomically $ fmap fst . Set.minView <$> readTVar pendingVar
      forM_ mNext $ \point -> do
        extLedger <- readTip
        now <- systemTimeCurrent systemTime
        case slotOnset (configLedger cfg) (ledgerState extLedger) (pointSlotNo point) of
          Left horizon -> do
            -- Cannot happen for an EB announced on our own chain: its slot is
            -- at or behind the tip whose summary we just read. Drop it rather
            -- than spin on it, and say so.
            atomically $ modifyTVar pendingVar (Set.delete point)
            traceWith tracer . MkTraceLeiosKernel $
              "runLeiosVoting: no wall-clock onset for "
                <> prettyLeiosPoint point
                <> ": "
                <> show horizon
          Right onset -> do
            let votableAt = addRelTime lHdrWait onset
            if now < votableAt
              then do
                -- Wait until it is votable, but wake early if something
                -- arrives: an EB turning up late with an older slot must not
                -- have to sit out this one's wait. Never register a zero
                -- delay.
                varTimeout <-
                  registerDelay . max 1 . diffTimeToMicrosecondsAsInt . nominalDelay $
                    diffRelTime votableAt now
                atomically $
                  (TVar.check =<< TVar.readTVar varTimeout) `TVar.orElse` takeAcquisition
              else do
                atomically $ modifyTVar pendingVar (Set.delete point)
                voteOn point extLedger (addRelTime lVoteWindow votableAt)

-- * The vote decision

-- | Whether to vote for an acquired EB, and why not when we don't.
--
-- Validation runs /before/ the deadline is checked, deliberately: applying a
-- closure takes real time, so whether we are still inside the vote window has
-- to be judged by the clock as it reads when we are ready to sign, not by a
-- reading taken before the work. The cheap checks still come first, so an EB we
-- would not vote for is never validated.
decideVote ::
  Monad m =>
  -- | Our seat on the voting committee, if we hold one.
  Maybe LeiosSeatId ->
  -- | The announcing RB's hash, if our tip announces this EB.
  Maybe RbHash ->
  -- | Validate the EB's closure.
  m (Either LeiosNotVotedReason a) ->
  -- | Read the wall clock. Called after validation.
  m RelativeTime ->
  -- | The moment after which a vote is too late.
  RelativeTime ->
  m (Either LeiosNotVotedReason (RbHash, LeiosSeatId, a))
decideVote mSeat mAnnouncer validate readNow deadline =
  case (mAnnouncer, mSeat) of
    (Nothing, _) -> pure $ Left ChainTipDoesNotAnnounce
    (_, Nothing) -> pure $ Left NotOnCommittee
    (Just rbHash, Just seatId) ->
      validate >>= \case
        Left reason -> pure $ Left reason
        Right a -> do
          now <- readNow
          pure $
            if now > deadline
              then Left TooLate
              else Right (rbHash, seatId, a)

-- * Validating the endorsed transactions

-- | The outcome of checking an EB's endorsed transactions.
data EbClosureVerdict blk
  = -- | Every tx applied, and the txs validated here have been recorded in the
    -- tx-cache. Carries how many were reapplied versus validated in full — the
    -- cache's hit rate.
    EbClosureValid !Int !Int
  | -- | A tx did not apply, so this EB must not be certified.
    EbClosureInvalid !(ApplyTxErr blk)

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
  let keys = foldMap (getTransactionKeySets . snd) closure
  values <- resolveValues keys
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
