{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Voting for endorser blocks, with the BLS keys of the node's credentials.
--
-- The node votes for an endorser block once it has its closure, if the tip
-- of its selection announced it, one of its keys holds a seat in the
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
import Control.Monad (forever, void)
import Control.Monad.Except (runExcept)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
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
  ( RelativeTime
  , SystemTime (..)
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , EmptyMK
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsEndorserBlocks (LedgerSupportsEndorserBlocks (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (LedgerSupportsMempool (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, isEquivocated)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosVote (..)
  , RankingBlockHash
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
  , VoteRelayWindow (..)
  , leiosVoteRelayWindow
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ReadOnlyForker (..))
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
  , voterClosure :: EndorserBlockHash -> m (Maybe [ByteString])
  -- ^ The transactions of an endorser block's closure, if the node has all.
  , voterAnnounceState :: AnnounceState m blk
  , voterVoteState :: VoteState m blk
  , voterSystemTime :: SystemTime m
  , voterTracer :: Tracer m TraceLeiosVoting
  }

-- | What the voter traces.
data TraceLeiosVoting
  = -- | The node applied the closure of an endorser block, and may vote for it.
    TraceLeiosEbValidated !EndorserBlockHash
  | -- | The node voted for the endorser block of an announcement.
    TraceLeiosVoted !LeiosVote
  | TraceLeiosNotVoted !EndorserBlockHash !NotVotedReason
  deriving Show

-- | Why the node did not vote for an endorser block.
data NotVotedReason
  = -- | The node has no ledger view of the election slot, or it has no Leios.
    NoLeiosLedgerView
  | -- | None of the node's BLS keys holds a seat in the election.
    NotOnCommittee
  | -- | The tip of the node's selection did not announce the block by the end
    -- of the window to vote.
    TipDoesNotAnnounce
  | -- | The node has no complete closure of the block after all.
    ClosureUnavailable
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
  , LeiosProtocol (BlockProtocol blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsEndorserBlocks blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  ) =>
  LeiosVoterEnv m blk ->
  [LeiosSigningKey] ->
  -- | The next endorser block whose closure the node acquired
  STM m EndorserBlockHash ->
  m Void
runLeiosVoter env keys acquired =
  withRegistry $ \registry ->
    forever $ do
      ebHash <- atomically acquired
      void . forkLinkedThread registry "Leios.voteFor" $
        voteFor env keys ebHash >>= \case
          Left reason -> traceWith (voterTracer env) (TraceLeiosNotVoted ebHash reason)
          Right votes -> for_ votes $ traceWith (voterTracer env) . TraceLeiosVoted

-- | Vote for an endorser block with every key that holds a seat in its
-- election.
voteFor ::
  forall m blk.
  ( IOLike m
  , LeiosProtocol (BlockProtocol blk)
  , LedgerSupportsProtocol blk
  , LedgerSupportsEndorserBlocks blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  ) =>
  LeiosVoterEnv m blk ->
  [LeiosSigningKey] ->
  EndorserBlockHash ->
  m (Either NotVotedReason [LeiosVote])
voteFor env keys ebHash = do
  immLedger <- atomically $ ChainDB.getImmutableLedger voterChainDB
  acquired <- systemTimeCurrent voterSystemTime
  -- The block that announced it is in the current slot at the latest, so its
  -- window to vote closes when that of the current slot does at the latest.
  case slotAt immLedger acquired >>= electionAt immLedger of
    Nothing -> pure (Left NoLeiosLedgerView)
    Just latest ->
      -- Wait for the selection to adopt the announcing block, if it has not yet.
      race (waitUntil (votingEnds latest)) (atomically tipAnnouncing) >>= \case
        Left () -> pure (Left TipDoesNotAnnounce)
        Right announcing -> voteIn immLedger announcing
 where
  MkLeiosVoterEnv
    { voterConfig
    , voterChainDB
    , voterClosure
    , voterAnnounceState
    , voterVoteState
    , voterSystemTime
    , voterTracer
    } = env

  voteIn immLedger announcing =
    case electionAt immLedger slot of
      Nothing -> pure (Left NoLeiosLedgerView)
      Just election -> voteInElection announcing slot election
   where
    slot = blockSlot (ancHeader announcing)

  voteInElection announcing slot election@(view, onset) = do
    now <- systemTimeCurrent voterSystemTime
    if
      | null seats -> pure (Left NotOnCommittee)
      | now > votingEnds election -> pure (Left TooLate)
      | otherwise ->
          validate announcing >>= \case
            Left reason -> pure (Left reason)
            Right () -> do
              traceWith voterTracer (TraceLeiosEbValidated ebHash)
              waitUntil (relayFrom window)
              cast
   where
    MkLeiosLedgerView{llvCommittee, llvQuorumStakeThreshold} = view

    seats = [(key, seat) | key <- keys, Just seat <- [getLeiosSeatId (deriveVerKeyDSIGN key) llvCommittee]]

    window = leiosVoteRelayWindow view onset

    cast = do
      now <- systemTimeCurrent voterSystemTime
      (stillAnnounces, equivocated) <-
        atomically $
          (,) <$> (Just announcing ==) <$> tipAnnouncing' <*> isEquivocated voterAnnounceState announcing
      if
        | now > votingEnds election -> pure (Left TooLate)
        | not stillAnnounces -> pure (Left TipDoesNotAnnounce)
        | equivocated -> pure (Left Equivocated)
        | otherwise -> do
            results <- for seats $ \(key, seat) -> do
              let vote = signVote key seat (announcingRbHash announcing)
              result <- addVote voterVoteState voterSystemTime (accepted vote)
              pure (vote, result)
            pure $ case [vote | (vote, Added) <- results] of
              [] | (_, result) : _ <- results -> Left (VoteNotAdded result)
              votes -> Right votes

    accepted vote =
      MkAcceptedVote
        { acceptedVote = vote
        , voteElectionSlot = slot
        , voteCommittee = llvCommittee
        , voteQuorum = unboundRational llvQuorumStakeThreshold
        , voteRelayWindow = window
        }

  -- The slot at the given time.
  slotAt immLedger at =
    either (const Nothing) (\(slot, _, _) -> Just slot) $
      HardFork.runQuery
        (HardFork.wallclockToSlot at)
        (hardForkSummary (configLedger voterConfig) (ledgerState immLedger))

  -- The voting period ends L_vote after the window to vote opens.
  votingEnds (view, onset) =
    addRelTime
      (milliseconds32ToNominalDiffTime (llvVotePeriod view))
      (relayFrom (leiosVoteRelayWindow view onset))

  -- The ledger view of the election in a slot, and when the slot began.
  electionAt :: ExtLedgerState blk EmptyMK -> SlotNo -> Maybe (LeiosLedgerView, RelativeTime)
  electionAt immLedger slot = do
    view <-
      either (const Nothing) Just . runExcept $
        forecastFor (ledgerViewForecastAt (configLedger voterConfig) (ledgerState immLedger)) slot
    leiosView <- getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
    (onset, _slotLength) <-
      either (const Nothing) Just $
        HardFork.runQuery
          (HardFork.slotToWallclock slot)
          (hardForkSummary (configLedger voterConfig) (ledgerState immLedger))
    pure (leiosView, onset)

  -- The tip of the selection, if it announces the block.
  tipAnnouncing = tipAnnouncing' >>= maybe retry pure

  tipAnnouncing' = do
    chain <- ChainDB.getCurrentChain voterChainDB
    pure $ case AF.head chain of
      Right tip
        | Just announcing <- mkAnnouncingHeader voterConfig tip
        , announcementEbHash (ancAnnouncement announcing) == ebHash ->
            Just announcing
      _ -> Nothing

  -- Apply the closure to the ledger state of the announcing block.
  validate announcing =
    voterClosure ebHash >>= \case
      Nothing -> pure (Left ClosureUnavailable)
      Just closure ->
        bracket
          (ChainDB.openReadOnlyForkerAtPoint voterChainDB (SpecificPoint (headerPoint (ancHeader announcing))))
          (either (const (pure ())) roforkerClose)
          $ \case
            -- The selection moved off the announcing block meanwhile.
            Left _ -> pure (Left TipDoesNotAnnounce)
            Right forker -> do
              extLedger <- atomically $ roforkerGetLedgerState forker
              let lcfg = configLedger voterConfig
                  slot = blockSlot (ancHeader announcing)
                  ticked = applyChainTick OmitLedgerEvents lcfg slot (ledgerState extLedger)
              case traverse (decodeEbTx ticked) closure of
                Left err -> pure (Left (EbInvalid err))
                Right txs -> do
                  let keySets = foldMap getTransactionKeySets txs
                  values <- roforkerReadTables forker keySets
                  pure $
                    either (Left . EbInvalid . Text.pack . show) Right . runExcept $
                      applyEbTxs lcfg slot txs (applyMempoolDiffs values keySets ticked)

  waitUntil at = do
    now <- systemTimeCurrent voterSystemTime
    threadDelay (nominalDelay (diffRelTime at now))

-- | Sign a vote for an announcement in a seat.
signVote :: LeiosSigningKey -> LeiosSeatId -> RankingBlockHash -> LeiosVote
signVote key seat rbHash =
  MkLeiosVote
    { voteAnnouncingRbHash = rbHash
    , voteSeatId = seat
    , voteSignature = signDSIGN leiosSignContext rbHash key
    }
