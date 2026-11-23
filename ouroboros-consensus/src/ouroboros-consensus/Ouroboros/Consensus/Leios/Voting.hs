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
-- A closure is validated as soon as it is complete and the tip announces it,
-- rather than once the window opens, which leaves the window to the vote.
-- Validating stops once the tip announces another block or the window closes.
-- What it validated stays in the LeiosTxCache, so that validating it again
-- only reapplies it.
module Ouroboros.Consensus.Leios.Voting
  ( VotingEnv (..)
  , TraceLeiosVoting (..)
  , NotVotedReason (..)
  , runLeiosVoting
  ) where

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, signDSIGN)
import Cardano.Crypto.Leios
  ( LeiosSeatId
  , LeiosSigningKey
  , getLeiosSeatId
  , leiosSignContext
  )
import Cardano.Ledger.BaseTypes (unboundRational)
import Control.Applicative (empty, (<|>))
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Monad (mfilter, unless, void, when)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.Except
  ( ExceptT (..)
  , runExcept
  , runExceptT
  , throwError
  )
import Control.Monad.Trans.Class (lift)
import Control.ResourceRegistry (ResourceKey, ResourceRegistry, allocate, release, withRegistry)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (bimap)
import Data.Foldable (asum, for_)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import Data.Void (Void)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , BlockSupportsLeios (..)
  , BlockSupportsProtocol
  , ConvertRawHash
  , LeiosClosureInvalid (..)
  , LeiosClosureValidation (..)
  , RealPoint
  , SlotNo
  , blockSlot
  , headerRealPoint
  , realPointHash
  , realPointToPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
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
import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..), PackedEndorserBlock)
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosVote (..)
  , ProtVerParity
  , RankingBlockHash
  , SerialisedEndorserBlock (..)
  , milliseconds32ToNominalDiffTime
  , toRankingBlockHash
  )
import Ouroboros.Consensus.Leios.VoteState (AddVoteResult (..), VoteState, addVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
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
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadAsync (Async, async, cancel, waitSTM)
  , MonadDelay (threadDelay)
  , STM
  , atomically
  , bracket
  , check
  , retry
  )
import Ouroboros.Consensus.Util.Time (nominalDelay)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ

-- | What the node's voter needs.
data VotingEnv m blk = MkVotingEnv
  { votingConfig :: TopLevelConfig blk
  , votingChainDB :: ChainDB m blk
  , votingTxCache :: LeiosTxCache m () ProtVerParity PackedEndorserBlock
  -- ^ Which transactions the node validated already, and under which parity.
  , votingAnnounceState :: AnnounceState m blk
  , votingVoteState :: VoteState m blk
  , votingSystemTime :: SystemTime m
  , votingTracer :: Tracer m TraceLeiosVoting
  , votingSubscribeClosures :: m (STM m LeiosPoint)
  -- ^ Subscribe to the points whose closure the node acquires from now on.
  }

-- | What the voter traces.
data TraceLeiosVoting
  = -- | The node applied the closure of an endorser block, and may vote for it:
    -- it reapplied that many transactions it validated before, and validated
    -- the others in full.
    TraceLeiosEbValidated
      !LeiosPoint
      -- | reapplied
      !Int
      -- | validated in full
      !Int
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
  | -- | The node had already moved on to the election of this slot or a later
    -- one, or had voted in this block's election.
    Superseded !SlotNo
  deriving (Eq, Show)

-- | Vote with these keys for each endorser block whose closure the node
-- acquires from now on.
--
-- One loop keeps a vote round open for each acquired closure until its window
-- closes, and targets at most one of them: the one the tip announces. The
-- target is validated, and voted for once the window opens, in a thread of
-- its own, which the loop cancels when the target changes. The target's slot
-- never decreases, and the node votes at most once in an election, so it
-- never votes for competing blocks.
runLeiosVoting ::
  forall m blk.
  ( IOLike m
  , SI.MonadTimer m
  , LedgerSupportsProtocol blk
  , BlockSupportsLeios blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  ) =>
  VotingEnv m blk ->
  [LeiosSigningKey] ->
  m Void
runLeiosVoting env keys = do
  acquired <- votingSubscribeClosures
  withRegistry $ \registry ->
    let go voter =
          -- What happened next, the target's outcome first.
          atomically
            ( targetFinished voter
                <|> voteWindowEnded voter
                <|> (ClosureAcquired <$> acquired)
                <|> tipChanged voter
            )
            >>= step registry voter
            >>= go
     in go MkVoter{voterRounds = Map.empty, voterFloor = 0}
 where
  MkVotingEnv
    { votingConfig
    , votingChainDB
    , votingTxCache
    , votingAnnounceState
    , votingVoteState
    , votingSystemTime
    , votingTracer
    , votingSubscribeClosures
    } = env

  lcfg = configLedger votingConfig

  traceNotVoted point reason = traceWith votingTracer (TraceLeiosNotVoted point reason)

  -- A failure of the target's thread is rethrown.
  targetFinished MkVoter{voterRounds} =
    asum
      [ either (NotVoted point) (TargetVoted point) <$> waitSTM thread
      | (point, MkVoteRound{voteRoundTarget = Just (_, thread)}) <- Map.toList voterRounds
      ]

  voteWindowEnded MkVoter{voterRounds, voterFloor} =
    asum
      [ do
          LazySTM.readTVar voteRoundClosing >>= check
          NotVoted point <$> endedReason voterFloor point
      | (point, MkVoteRound{voteRoundClosing}) <- Map.toList voterRounds
      ]

  endedReason voterFloor point
    | pointSlotNo point < voterFloor = pure (Superseded voterFloor)
    | otherwise =
        announcedBy point >>= \case
          Nothing -> pure TipDoesNotAnnounce
          Just announcing ->
            maybe NotAccepted (const TooLate)
              <$> acceptedAnnouncementAge votingAnnounceState announcing

  -- The voter targets the block the tip announces, if it has a vote round
  -- for it and has not moved past its slot.
  tipChanged MkVoter{voterRounds, voterFloor} = do
    announced <- fmap fst <$> tipAnnouncement votingConfig votingChainDB
    let wanted = mfilter (\point -> Map.member point voterRounds && pointSlotNo point >= voterFloor) announced
    if wanted == targetOf voterRounds then empty else pure (TipChanged wanted)

  step :: ResourceRegistry m -> Voter m -> Event -> m (Voter m)
  step registry voter@MkVoter{voterRounds, voterFloor} = \case
    ClosureAcquired point
      | Map.member point voterRounds -> pure voter
      | otherwise -> do
          immLedger <- atomically $ ChainDB.getImmutableLedger votingChainDB
          case electionOf lcfg keys (pointSlotNo point) (ledgerState immLedger) of
            Left reason -> do
              traceNotVoted point reason
              pure voter
            Right election -> do
              closing <- delayUntil votingSystemTime (electionEnds election)
              pure voter{voterRounds = Map.insert point (MkVoteRound election closing Nothing) voterRounds}
    TargetVoted point votes -> do
      for_ votes $ traceWith votingTracer . TraceLeiosVoted
      closeVoteRound
        point
        -- No other vote in this election.
        voter{voterFloor = max voterFloor (pointSlotNo point + 1)}
    NotVoted point reason -> do
      traceNotVoted point reason
      closeVoteRound point voter
    TipChanged wanted -> do
      rounds <- traverse untarget voterRounds
      case wanted of
        Nothing -> pure voter{voterRounds = rounds}
        Just point -> do
          let start voteRound@MkVoteRound{voteRoundElection} = do
                target <- allocate registry (\_ -> async (voteFor point voteRoundElection)) cancel
                pure voteRound{voteRoundTarget = Just target}
          rounds' <- Map.alterF (traverse start) point rounds
          pure voter{voterRounds = rounds', voterFloor = max voterFloor (pointSlotNo point)}

  -- Cancel the vote round's target thread, if any.
  untarget :: VoteRound m -> m (VoteRound m)
  untarget voteRound@MkVoteRound{voteRoundTarget} = do
    for_ voteRoundTarget $ void . release . fst
    pure voteRound{voteRoundTarget = Nothing}

  closeVoteRound point voter@MkVoter{voterRounds} = do
    for_ (Map.lookup point voterRounds) untarget
    pure voter{voterRounds = Map.delete point voterRounds}

  -- Vote for the block with every key that holds a seat in its election.
  --
  -- Every way of not voting leaves via 'throwError', and the cheap checks come
  -- first, so a block the node would not vote for is never validated. The
  -- loop cancels this once the window ends or the tip moves on.
  voteFor :: LeiosPoint -> Election -> m (Either NotVotedReason [LeiosVote])
  voteFor
    point@MkLeiosPoint{pointSlotNo = slot}
    MkElection{electionView, electionWindow, electionSeats, electionLHdr} = runExceptT $ do
      announcing <- stm (announcedBy point) >>= (?>= TipDoesNotAnnounce)

      -- The announcement must have arrived within L_hdr, with no other
      -- announcement of the election.
      age <- stm $ acceptedAnnouncementAge votingAnnounceState announcing >>= maybe retry pure
      when (age > electionLHdr) $ throwError (AnnouncedLate age)
      notEquivocated announcing

      (reapplied, applied) <-
        ExceptT $ validateEndorserBlock lcfg votingChainDB votingTxCache point announcing
      lift $ traceWith votingTracer (TraceLeiosEbValidated point reapplied applied)

      lift $ do
        now <- systemTimeCurrent votingSystemTime
        threadDelay (nominalDelay (diffRelTime (voteFrom electionWindow) now))
      stillAnnounces <- stm $ (Just announcing ==) <$> announcedBy point
      unless stillAnnounces $ throwError TipDoesNotAnnounce
      notEquivocated announcing

      results <- lift $ for electionSeats $ \(key, seat) -> do
        let vote = signVote key seat (toRankingBlockHash (Proxy @blk) (realPointHash announcing))
        (,) vote
          <$> addVote
            votingVoteState
            votingSystemTime
            MkAcceptedVote
              { acceptedVote = vote
              , voteElectionSlot = slot
              , voteCommittee = llvCommittee electionView
              , acceptedQuorum = unboundRational (llvQuorumStakeThreshold electionView)
              , voteWindow = electionWindow
              }
      case partition ((== Added) . snd) results of
        ([], (_, result) : _) -> throwError (VoteNotAdded result)
        (added, _) -> pure (map fst added)

  notEquivocated announcing = do
    equivocated <- stm $ isEquivocated votingAnnounceState announcing
    when equivocated $ throwError Equivocated

  stm = lift . atomically

  -- The tip of the selection, if it announces the block.
  announcedBy point =
    tipAnnouncement votingConfig votingChainDB <&> \case
      Just (announced, announcing) | announced == point -> Just announcing
      _ -> Nothing

-- | The block the voter targets: the one whose vote round has a thread.
targetOf :: Map LeiosPoint (VoteRound m) -> Maybe LeiosPoint
targetOf rounds = listToMaybe [point | (point, MkVoteRound{voteRoundTarget = Just _}) <- Map.toList rounds]

-- | The election of a slot as the voter sees it: its ledger view, and when
-- the slot began.
electionAt ::
  forall blk mk.
  (LedgerSupportsProtocol blk, HasHardForkHistory blk) =>
  LedgerConfig blk ->
  SlotNo ->
  LedgerState blk mk ->
  Maybe (LeiosLedgerView, RelativeTime)
electionAt lcfg slot st = do
  view <-
    either (const Nothing) Just . runExcept $
      forecastFor (ledgerViewForecastAt lcfg st) slot
  leiosView <- getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
  (onset, _slotLength) <-
    either (const Nothing) Just $
      HardFork.runQuery (HardFork.slotToWallclock slot) (hardForkSummary lcfg st)
  pure (leiosView, onset)

-- | What the voter needs of the election of a slot, if the node can vote in
-- it with these keys.
electionOf ::
  (LedgerSupportsProtocol blk, HasHardForkHistory blk) =>
  LedgerConfig blk ->
  [LeiosSigningKey] ->
  SlotNo ->
  LedgerState blk mk ->
  Either NotVotedReason Election
electionOf lcfg keys slot st = do
  (view, onset) <- maybe (Left NoLeiosLedgerView) Right (electionAt lcfg slot st)
  let election = mkElection keys view onset
  when (null (electionSeats election)) $ Left NotOnCommittee
  pure election

-- | What the voter needs of an election, with the seats these keys hold.
mkElection :: [LeiosSigningKey] -> LeiosLedgerView -> RelativeTime -> Election
mkElection keys view onset =
  MkElection
    { electionView = view
    , electionWindow = window
    , electionSeats =
        [ (key, seat)
        | key <- keys
        , Just seat <- [getLeiosSeatId (deriveVerKeyDSIGN key) llvCommittee]
        ]
    , electionLHdr = milliseconds32ToNominalDiffTime llvAnnouncementPeriod
    , electionEnds = addRelTime (milliseconds32ToNominalDiffTime llvVotePeriod) (voteFrom window)
    }
 where
  MkLeiosLedgerView{llvCommittee, llvAnnouncementPeriod, llvVotePeriod} = view
  window = leiosVoteWindow view onset

-- | The block the tip of the selection announces, if any.
tipAnnouncement ::
  (IOLike m, BlockSupportsProtocol blk) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  STM m (Maybe (LeiosPoint, RealPoint blk))
tipAnnouncement cfg chainDB = do
  chain <- ChainDB.getCurrentChain chainDB
  pure $ case AF.head chain of
    Right tip
      | Just announcing <- mkAnnouncingHeader cfg tip ->
          Just
            ( MkLeiosPoint (blockSlot tip) (announcementEbHash (ancAnnouncement announcing))
            , headerRealPoint tip
            )
    _ -> Nothing

-- | A variable that turns true at a moment, at once if it passed.
delayUntil ::
  (IOLike m, SI.MonadTimer m) =>
  SystemTime m -> RelativeTime -> m (LazySTM.TVar m Bool)
delayUntil systemTime at = do
  now <- systemTimeCurrent systemTime
  SI.registerDelay (nominalDelay (diffRelTime at now))

-- | What the voter needs of an election, worked out once its closure arrives.
data Election = MkElection
  { electionView :: !LeiosLedgerView
  , electionWindow :: !VoteWindow
  , electionSeats :: ![(LeiosSigningKey, LeiosSeatId)]
  -- ^ The seats the node's keys hold.
  , electionLHdr :: !NominalDiffTime
  , electionEnds :: !RelativeTime
  -- ^ The end of the window to vote.
  }

-- | The voter's state between events.
data Voter m = MkVoter
  { voterRounds :: !(Map LeiosPoint (VoteRound m))
  -- ^ A vote round per acquired closure, until its window closes.
  , voterFloor :: !SlotNo
  -- ^ The voter targets no block below this slot.
  }

-- | A vote round for a block whose closure the node acquired.
data VoteRound m = MkVoteRound
  { voteRoundElection :: !Election
  , voteRoundClosing :: !(LazySTM.TVar m Bool)
  -- ^ Turns true at 'electionEnds'.
  , voteRoundTarget :: !(Maybe (ResourceKey m, Async m (Either NotVotedReason [LeiosVote])))
  -- ^ The thread voting for the block, while the voter targets it. One vote
  -- round at most has one.
  }

-- | What happened, for the voter to react to.
data Event
  = -- | The node acquired a closure.
    ClosureAcquired !LeiosPoint
  | -- | The node voted for the target.
    TargetVoted !LeiosPoint ![LeiosVote]
  | -- | The node will not vote for the block, and why.
    NotVoted !LeiosPoint !NotVotedReason
  | -- | The tip now announces this block to target, or none.
    TipChanged !(Maybe LeiosPoint)

-- | Validate the closure of an endorser block on the ledger state of the
-- block that announced it. Yields how many transactions it reapplied, and how
-- many it validated in full.
validateEndorserBlock ::
  forall m blk b.
  (IOLike m, BlockSupportsLeios blk) =>
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosTxCache m () ProtVerParity b ->
  LeiosPoint ->
  -- | The block that announced it
  RealPoint blk ->
  m (Either NotVotedReason (Int, Int))
validateEndorserBlock lcfg chainDB txCache point announcing =
  ChainDB.withLeiosReader chainDB $ \reader -> runExceptT $ do
    body <- lift (lookupEbBody reader point) >>= (?>= BodyUnavailable)
    txs <- lift (lookupEbClosure reader point) >>= (?>= ClosureUnavailable)
    ExceptT $ withForkerAt (realPointToPoint announcing) $ \case
      -- The selection moved off the announcing block meanwhile.
      Nothing -> pure (Left TipDoesNotAnnounce)
      Just forker -> do
        extLedger <- atomically $ roforkerGetLedgerState forker
        -- REVIEW: A bug of the ledger that a closure triggers throws here and,
        -- through the voter, takes the node down. Contain it per vote round, or
        -- by thread as for a peer's connection?
        MkLeiosClosureValidation{closureVerdict, closureReapplied, closureValidated} <-
          validateLeiosClosure
            lcfg
            (roforkerReadTables forker)
            wasValidated
            recordValidated
            (MkSerialisedEndorserBlock body txs)
            (ledgerState extLedger)
        pure $ bimap notVotedReason (const (closureReapplied, length closureValidated)) closureVerdict
 where
  withForkerAt at k =
    bracket
      (ChainDB.openReadOnlyForkerAtPoint chainDB (LSQ.SpecificPoint at))
      (either (const (pure ())) roforkerClose)
      (k . either (const Nothing) Just)

  wasValidated parity txHashes =
    withLookupTx txCache $ \lookupTx -> for txHashes $ fmap (== Just (Right parity)) . lookupTx

  recordValidated parity txHash =
    withLockedInsertAppliedTx txCache $ \w0 step -> step w0 txHash parity

  notVotedReason = \case
    ClosureOverCapacity -> EbOverCapacity
    ClosureIncomplete -> ClosureUnavailable
    ClosureInvalid reason -> EbInvalid reason

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
