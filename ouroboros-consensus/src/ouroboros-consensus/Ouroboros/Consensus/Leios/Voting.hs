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
  , LeiosVoterTxCache (..)
  , noLeiosVoterTxCache
  , TraceLeiosVoting (..)
  , NotVotedReason (..)
  , runLeiosVoter
  ) where

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, signDSIGN)
import Cardano.Crypto.Leios
  ( LeiosSeatId
  , LeiosSigningKey
  , getLeiosSeatId
  )
import Cardano.Ledger.BaseTypes (unboundRational)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Except
  ( ExceptT (..)
  , runExcept
  , runExceptT
  , throwError
  )
import Control.Monad.Trans.Class (lift)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Traversable (for)
import Data.Void (Void)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , BlockSupportsLeios (..)
  , ConvertRawHash
  , HasHeader
  , Header
  , LeiosClosureInvalid (..)
  , LeiosClosureValidation (..)
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
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
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
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosVote (..)
  , ProtVerParity
  , RankingBlockHash
  , SerialisedEndorserBlock (..)
  , TxHash
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
  , voterTxCache :: LeiosVoterTxCache m
  , voterAnnounceState :: AnnounceState m blk
  , voterVoteState :: VoteState m blk
  , voterSystemTime :: SystemTime m
  , voterTracer :: Tracer m TraceLeiosVoting
  }

-- | Which transactions the node validated already, and under which protocol
-- version parity, as the LeiosTxCache is to tell.
data LeiosVoterTxCache m = MkLeiosVoterTxCache
  { lookupValidatedTxs :: [TxHash] -> m [Maybe ProtVerParity]
  -- ^ For each transaction, the parity it was validated under, if it was.
  , insertValidatedTxs :: ProtVerParity -> [TxHash] -> m ()
  -- ^ Record that the node validated these transactions under this parity.
  }

-- | Knows of no validated transaction, and records none.
noLeiosVoterTxCache :: Applicative m => LeiosVoterTxCache m
noLeiosVoterTxCache =
  MkLeiosVoterTxCache
    { lookupValidatedTxs = pure . map (const Nothing)
    , insertValidatedTxs = \_parity _txHashes -> pure ()
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
  deriving (Eq, Show)

-- | Vote for each endorser block the given action yields, once the node has
-- its closure.
runLeiosVoter ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsLeios blk
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
  , BlockSupportsLeios blk
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

  (reapplied, applied) <-
    ExceptT $ validateEndorserBlock lcfg voterChainDB voterLeiosDb voterTxCache point announcing
  lift $ traceWith voterTracer (TraceLeiosEbValidated point reapplied applied)

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
    , voterTxCache
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

-- | Validate the closure of an endorser block on the ledger state of the
-- block that announced it. Yields how many transactions it reapplied, and how
-- many it validated in full.
validateEndorserBlock ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLeios blk
  , HasHeader (Header blk)
  ) =>
  LedgerConfig blk ->
  ChainDB m blk ->
  LeiosDbHandle m ->
  LeiosVoterTxCache m ->
  LeiosPoint ->
  AnnouncingHeader blk ->
  m (Either NotVotedReason (Int, Int))
validateEndorserBlock lcfg chainDB leiosDb txCache point announcing =
  withReader leiosDb $ \reader -> runExceptT $ do
    body <- lift (lookupEbBody reader point) >>= (?>= BodyUnavailable)
    txs <- lift (lookupEbClosure reader point) >>= (?>= ClosureUnavailable)
    ExceptT $ withForkerAt (headerPoint (ancHeader announcing)) $ \case
      -- The selection moved off the announcing block meanwhile.
      Nothing -> pure (Left TipDoesNotAnnounce)
      Just forker -> do
        extLedger <- atomically $ roforkerGetLedgerState forker
        MkLeiosClosureValidation{closureVerdict, closureParity, closureReapplied, closureValidated} <-
          validateLeiosClosure
            lcfg
            (roforkerReadTables forker)
            wasValidated
            (MkSerialisedEndorserBlock body txs)
            (ledgerState extLedger)
        -- Even a closure that fails validated what came before the failure.
        insertValidatedTxs txCache closureParity closureValidated
        pure $ bimap notVotedReason (const (closureReapplied, length closureValidated)) closureVerdict
 where
  withForkerAt at k =
    bracket
      (ChainDB.openReadOnlyForkerAtPoint chainDB (SpecificPoint at))
      (either (const (pure ())) roforkerClose)
      (k . either (const Nothing) Just)

  wasValidated parity txHashes = map (== Just parity) <$> lookupValidatedTxs txCache txHashes

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
    , voteSignature = signDSIGN () rbHash key
    }

-- | The value, or stop with the reason.
(?>=) :: Monad m => Maybe a -> e -> ExceptT e m a
Nothing ?>= reason = throwError reason
Just x ?>= _ = pure x
