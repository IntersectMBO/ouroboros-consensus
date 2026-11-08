{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Voting for endorser blocks, with the BLS keys of the node's credentials.
--
-- The node votes for an endorser block once it has its closure, if the tip
-- of its selection announced it, the node accepted that announcement within
-- L_hdr and no other of the election, one of its keys holds a seat in the
-- election, and the closure applies to the tip's ledger state. It votes once
-- the window to vote opens, if by then it saw no other announcement of the
-- election, the tip still announces it, and the window has not closed.
--
-- Each endorser block gets a thread of its own, so a closure that takes long
-- to validate delays no other block's vote. And the closure is validated as
-- soon as it is complete, rather than once the window opens, which leaves
-- the window to the vote.
module Ouroboros.Consensus.Leios.Voting
  ( LeiosVoterEnv (..)
  , TraceLeiosVoting (..)
  , NotVotedReason (..)
  , runLeiosVoter
  ) where

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, signDSIGN)
import Cardano.Crypto.Leios
  ( LeiosSeatId
  , LeiosSigningKey
  , getLeiosSeatId
  , leiosSignContext
  )
import Cardano.Ledger.BaseTypes (unboundRational)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Except
  ( ExceptT (..)
  , liftEither
  , runExcept
  , runExceptT
  , throwError
  )
import Control.Monad.Trans.Class (lift)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import Data.Void (Void)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , ConvertRawHash
  , SlotNo
  , blockSlot
  , headerPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , LedgerConfig
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsEndorserBlocks (LedgerSupportsEndorserBlocks (..))
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , GenTx
  , LedgerSupportsMempool (..)
  , TxEbMeasureByteSize (..)
  , TxLimits (..)
  , TxMeasure (..)
  , WhetherToIntervene (Intervene)
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
import Ouroboros.Consensus.Ledger.Tables
  ( DiffMK (..)
  , HasLedgerTables (..)
  , LedgerTables
  , ValuesMK (..)
  , ltliftA2
  )
import Ouroboros.Consensus.Ledger.Tables.Diff (Delta (..), Diff (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables, restrictValuesMK)
import Ouroboros.Consensus.Leios.AnnounceState
  ( AnnounceState
  , acceptedAnnouncementAge
  , isEquivocated
  )
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosVote (..)
  , RankingBlockHash
  , deserialiseEndorserBlock
  , endorserBlockReferences
  , milliseconds32ToNominalDiffTime
  )
import Ouroboros.Consensus.Leios.VoteState (AddVoteResult (..), VoteState, addVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  , announcingRbHash
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , VoteWindow (..)
  , leiosVoteWindow
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ReadOnlyForker (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle
  , LeiosDbReader (..)
  , withReader
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadAsync (race)
  , MonadDelay (threadDelay)
  , STM
  , atomically
  , bracket
  , retry
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (SpecificPoint))

-- | What the node's voter needs.
data LeiosVoterEnv m blk = MkLeiosVoterEnv
  { voterConfig :: TopLevelConfig blk
  , voterChainDB :: ChainDB m blk
  , voterLeiosDb :: LeiosDbHandle m
  , voterAnnounceState :: AnnounceState m blk
  , voterVoteState :: VoteState m blk
  , voterSystemTime :: SystemTime m
  , voterTracer :: Tracer m TraceLeiosVoting
  }

-- | What the voter traces.
data TraceLeiosVoting
  = -- | The node applied the closure of an endorser block, and may vote for it.
    TraceLeiosEbValidated !LeiosPoint
  | -- | The node voted for the endorser block of an announcement.
    TraceLeiosVoted !LeiosVote
  | TraceLeiosNotVoted !LeiosPoint !NotVotedReason
  deriving Show

-- | Why the node did not vote for an endorser block.
data NotVotedReason
  = -- | The node has no ledger view of the election slot, or it has no Leios.
    NoLeiosLedgerView
  | -- | None of the node's BLS keys holds a seat in the election.
    NotOnCommittee
  | -- | The node accepted no announcement of the block from a peer within
    -- L_hdr after the election slot began.
    NotAccepted
  | -- | The node accepted the announcement of the block this long after the
    -- election slot began, later than L_hdr.
    AnnouncedLate !NominalDiffTime
  | -- | The tip of the node's selection did not announce the block by the end
    -- of the window to vote.
    TipDoesNotAnnounce
  | -- | The node holds no body of the block after all.
    BodyUnavailable
  | -- | The node has no complete closure of the block after all.
    ClosureUnavailable
  | -- | The block references more than an endorser block may hold, by what it
    -- claims of its transactions or by what they turn out to take.
    EbOverCapacity
  | -- | The closure does not apply to the tip's ledger state.
    EbInvalid !Text
  | -- | The node saw another announcement of the election.
    Equivocated
  | -- | The window to vote closed before the node was ready to.
    TooLate
  | -- | The node's 'VoteState' did not add the node's own vote.
    VoteNotAdded !AddVoteResult
  deriving (Eq, Show)

-- | Vote for each endorser block the given action yields, once the node has
-- its closure.
runLeiosVoter ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsEndorserBlocks blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  ) =>
  LeiosVoterEnv m blk ->
  [LeiosSigningKey] ->
  -- | The next point whose closure the node acquired
  STM m LeiosPoint ->
  m Void
runLeiosVoter env keys acquired =
  withRegistry $ \registry ->
    forever $ do
      point <- atomically acquired
      void . forkLinkedThread registry "Leios.voteFor" $
        voteFor env keys point >>= \case
          Left reason -> traceWith (voterTracer env) (TraceLeiosNotVoted point reason)
          Right votes -> for_ votes $ traceWith (voterTracer env) . TraceLeiosVoted

-- | Vote for an endorser block with every key that holds a seat in its
-- election.
--
-- Every way of not voting leaves via 'throwError', and the cheap checks come
-- first, so a block the node would not vote for is never validated. The
-- window is checked twice: when the closure arrives, which may be after the
-- window closed, and before signing, as validating takes real time.
voteFor ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsEndorserBlocks blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  ) =>
  LeiosVoterEnv m blk ->
  [LeiosSigningKey] ->
  LeiosPoint ->
  m (Either NotVotedReason [LeiosVote])
voteFor env keys point@MkLeiosPoint{pointSlotNo = slot} = runExceptT $ do
  immLedger <- lift $ atomically $ ChainDB.getImmutableLedger voterChainDB
  (view, onset) <- electionAt immLedger ?>= NoLeiosLedgerView
  let MkLeiosLedgerView{llvCommittee, llvQuorumStakeThreshold, llvAnnouncementPeriod, llvVotePeriod} = view
      window = leiosVoteWindow view onset
      lHdr = milliseconds32ToNominalDiffTime llvAnnouncementPeriod
      votingEnds = addRelTime (milliseconds32ToNominalDiffTime llvVotePeriod) (voteFrom window)
      seats =
        [ (key, seat)
        | key <- keys
        , Just seat <- [getLeiosSeatId (deriveVerKeyDSIGN key) llvCommittee]
        ]
  when (null seats) $ throwError NotOnCommittee
  notAfter votingEnds

  -- The selection may adopt the announcing block after the closure arrived.
  announcing <- lift (awaitBefore votingEnds tipAnnouncing) >>= (?>= TipDoesNotAnnounce)

  -- The announcing header must have arrived within L_hdr, with no other
  -- announcement of the election.
  age <-
    lift (awaitBefore (addRelTime lHdr onset) (acceptedAnnouncementAge voterAnnounceState announcing))
      >>= (?>= NotAccepted)
  when (age > lHdr) $ throwError (AnnouncedLate age)
  notEquivocated announcing

  ExceptT $ validateEndorserBlock lcfg voterChainDB voterLeiosDb point announcing
  lift $ traceWith voterTracer (TraceLeiosEbValidated point)

  lift $ waitUntil (voteFrom window)
  notAfter votingEnds
  stillAnnounces <- lift $ atomically $ (Just announcing ==) <$> tipAnnouncing
  unless stillAnnounces $ throwError TipDoesNotAnnounce
  notEquivocated announcing

  results <- lift $ for seats $ \(key, seat) -> do
    let vote = signVote key seat (announcingRbHash announcing)
    (,) vote
      <$> addVote
        voterVoteState
        voterSystemTime
        MkAcceptedVote
          { acceptedVote = vote
          , voteElectionSlot = slot
          , voteCommittee = llvCommittee
          , acceptedQuorum = unboundRational llvQuorumStakeThreshold
          , voteWindow = window
          }
  case [vote | (vote, Added) <- results] of
    [] | (_, result) : _ <- results -> throwError (VoteNotAdded result)
    votes -> pure votes
 where
  MkLeiosVoterEnv
    { voterConfig
    , voterChainDB
    , voterLeiosDb
    , voterAnnounceState
    , voterVoteState
    , voterSystemTime
    , voterTracer
    } = env

  lcfg = configLedger voterConfig

  -- The election's ledger view and when its slot began.
  electionAt immLedger = do
    view <-
      either (const Nothing) Just . runExcept $
        forecastFor (ledgerViewForecastAt lcfg (ledgerState immLedger)) slot
    leiosView <- getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
    (onset, _slotLength) <-
      either (const Nothing) Just $
        HardFork.runQuery (HardFork.slotToWallclock slot) (hardForkSummary lcfg (ledgerState immLedger))
    pure (leiosView, onset)

  -- The tip of the selection, if it announces the block.
  tipAnnouncing = do
    chain <- ChainDB.getCurrentChain voterChainDB
    pure $ case AF.head chain of
      Right tip
        | blockSlot tip == slot
        , Just announcing <- mkAnnouncingHeader voterConfig tip
        , announcementEbHash (ancAnnouncement announcing) == pointEbHash point ->
            Just announcing
      _ -> Nothing

  notAfter deadline = do
    now <- lift $ systemTimeCurrent voterSystemTime
    when (now > deadline) $ throwError TooLate

  notEquivocated announcing = do
    equivocated <- lift $ atomically $ isEquivocated voterAnnounceState announcing
    when equivocated $ throwError Equivocated

  -- What the transaction yields by the deadline. It is read once before
  -- racing the clock, which a deadline already past would otherwise win.
  awaitBefore deadline stm =
    atomically stm >>= \case
      Just x -> pure (Just x)
      Nothing ->
        either (const Nothing) Just
          <$> race (waitUntil deadline) (atomically (stm >>= maybe retry pure))

  waitUntil at = do
    now <- systemTimeCurrent voterSystemTime
    threadDelay (nominalDelay (diffRelTime at now))

-- | Check an endorser block against what one may hold, then apply its closure
-- to the ledger state of the block that announced it.
validateEndorserBlock ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsEndorserBlocks blk
  ) =>
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosDbHandle m ->
  LeiosPoint ->
  AnnouncingHeader blk ->
  m (Either NotVotedReason ())
validateEndorserBlock lcfg chainDB leiosDb point@MkLeiosPoint{pointSlotNo = slot} announcing =
  withReader leiosDb $ \reader -> runExceptT $ do
    body <- lift (lookupEbBody reader point) >>= (?>= BodyUnavailable)
    eb <-
      liftEither $
        first (EbInvalid . Text.pack . show) $
          deserialiseEndorserBlock (Proxy @DijkstraEra) body
    let references = endorserBlockReferences eb
    ExceptT $ withForkerAt (headerPoint (ancHeader announcing)) $ \case
      -- The selection moved off the announcing block meanwhile.
      Nothing -> pure (Left TipDoesNotAnnounce)
      Just forker -> runExceptT $ do
        extLedger <- lift $ atomically $ roforkerGetLedgerState forker
        let ticked = applyChainTick OmitLedgerEvents lcfg slot (ledgerState extLedger)
            capacity = ebCapacityTxMeasure lcfg ticked
            claimedBytes = sum [toInteger size | (_txHash, size) <- references]
        when (claimedBytes > toInteger (unByteSize32 (txEbMeasureByteSize capacity))) $
          throwError EbOverCapacity

        closure <- lift (lookupEbClosure reader point) >>= (?>= ClosureUnavailable)
        when (length closure /= length references) $ throwError ClosureUnavailable
        txs <- liftEither $ first EbInvalid $ traverse (decodeEbTx ticked) closure

        let txKeys = foldMap getTransactionKeySets txs
        values <- lift $ roforkerReadTables forker txKeys
        liftEither $ void $ applyClosure lcfg slot capacity (applyMempoolDiffs values txKeys ticked) txs
 where
  withForkerAt at k =
    bracket
      (ChainDB.openReadOnlyForkerAtPoint chainDB (SpecificPoint at))
      (either (const (pure ())) roforkerClose)
      (k . either (const Nothing) Just)

-- | Apply the transactions of a closure, one after the other, to the ledger
-- state of the block that announced it, ticked to its slot, as a certificate
-- for it would. And sum what they take of an endorser block's capacity, which
-- must hold them all. Returns how many it applied.
--
-- It applies with 'Intervene', so that the ledger does not flip a
-- transaction's validity flag, which applying a certified closure would not
-- either.
--
-- The closure's ledger table stays outside the state. Each transaction runs on
-- a state holding only its own inputs, as the mempool runs it, and its effect
-- is folded back into the table, so each step costs what the transaction
-- does, not what the closure does.
applyClosure ::
  forall blk.
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  SlotNo ->
  -- | What an endorser block may hold
  TxEbMeasure blk ->
  TickedLedgerState blk ValuesMK ->
  [GenTx blk] ->
  Either NotVotedReason Int
applyClosure cfg slot capacity st0 = go st0 (projectLedgerTables st0) Measure.zero 0
 where
  go ::
    TickedLedgerState blk mk ->
    LedgerTables blk ValuesMK ->
    TxEbMeasure blk ->
    Int ->
    [GenTx blk] ->
    Either NotVotedReason Int
  go _ _ _ !applied [] = Right applied
  go st values measured !applied (tx : txs) = do
    let stTx = st `withLedgerTables` ltliftA2 restrictValuesMK values (getTransactionKeySets tx)
    phase1 <- invalid $ txMeasurePhase1 cfg (forgetLedgerTables stTx) tx
    phase2 <- invalid $ txMeasurePhase2 cfg stTx tx
    let measured' = measured `Measure.plus` txEbMeasure (Proxy @blk) (TxMeasure phase1 phase2)
    unless (measured' Measure.<= capacity) $ Left EbOverCapacity
    (st', _vtx) <- invalid $ applyTx cfg Intervene slot tx stTx
    go st' (ltliftA2 applyDelta values (projectLedgerTables st')) measured' (applied + 1) txs

  invalid = first (EbInvalid . Text.pack . show) . runExcept

  -- One update per key the transaction changed, where 'applyDiffs' would
  -- merge over the whole table.
  applyDelta :: Ord k => ValuesMK k v -> DiffMK k v -> ValuesMK k v
  applyDelta (ValuesMK m) (DiffMK (Diff ds)) = ValuesMK (Map.foldlWithKey' step m ds)
   where
    step acc k (Insert v) = Map.insert k v acc
    step acc k Delete = Map.delete k acc

-- | Sign a vote for an announcement in a seat.
signVote :: LeiosSigningKey -> LeiosSeatId -> RankingBlockHash -> LeiosVote
signVote key seat rbHash =
  MkLeiosVote
    { voteAnnouncingRbHash = rbHash
    , voteSeatId = seat
    , voteSignature = signDSIGN leiosSignContext rbHash key
    }

-- | The value, or stop with the reason.
(?>=) :: Monad m => Maybe a -> e -> ExceptT e m a
Nothing ?>= reason = throwError reason
Just x ?>= _ = pure x
