{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Per-block-type seam between the polymorphic kernel and Leios voting.
--
-- The voting loop ('runLeiosVoting') is generic in @blk@; the only era
-- knowledge it needs is "given the current ledger state, who is on the
-- voting committee?". That single decision is captured by the
-- 'HasLeiosVoting' class.
--
-- Blocks that are not Leios-enabled get a no-op method by writing an
-- empty instance:
--
-- > instance HasLeiosVoting MyBlock
--
-- The 'HardForkBlock' instance dispatches to the currently-active era.
module Ouroboros.Consensus.Leios.Voting
  ( -- * Era seam
    HasLeiosVoting (..)

    -- * Committee
  , LeiosCommittee (..)
  , emptyLeiosCommittee

    -- * Voting loop
  , runLeiosVoting
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Constraint (All)
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict (hcmap, hcollapse)
import qualified Data.SOP.Telescope as Telescope
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( LeiosPoint (..)
  , LeiosVote (..)
  , TraceLeiosKernel (..)
  , VoterId (..)
  )
import LeiosVoteState (LeiosVoteState (..))
import Ouroboros.Consensus.Config (VotingKey)
import Ouroboros.Consensus.HardFork.Combinator.Basics
  ( HardForkBlock
  , LedgerState (HardForkLedgerState)
  )
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  )
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerState)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Util.IOLike (IOLike, atomically)

-------------------------------------------------------------------------------
-- Committee
-------------------------------------------------------------------------------

-- | The set of voters eligible to participate in Leios voting for the
-- current era.
--
-- Function-shaped so per-era instances can define membership without
-- having to enumerate the universe (e.g. Conway can return @const True@
-- as a placeholder until proper committee selection from ledger state is
-- implemented).
newtype LeiosCommittee = LeiosCommittee
  { isCommitteeMember :: VoterId -> Bool
  }

-- | The empty committee. Returned by the default 'getLeiosCommittee' so
-- that non-Leios blocks silently abstain from voting.
emptyLeiosCommittee :: LeiosCommittee
emptyLeiosCommittee = LeiosCommittee (const False)

-------------------------------------------------------------------------------
-- Era seam
-------------------------------------------------------------------------------

-- | Era-specific knobs for the Leios voting loop.
class HasLeiosVoting blk where
  -- | The committee for the current ledger state. The voting loop checks
  -- 'isCommitteeMember' on each acquired EB and only votes if the operator's
  -- voter id is in the committee.
  --
  -- The default returns 'emptyLeiosCommittee'; this lets non-Leios blocks
  -- pick up the no-op behaviour with an empty instance declaration:
  --
  -- > instance HasLeiosVoting MyBlock
  getLeiosCommittee :: LedgerState blk EmptyMK -> LeiosCommittee
  getLeiosCommittee _ = emptyLeiosCommittee

-- | Dispatch to the active era of a hard-fork chain. Requires every era in
-- the @xs@ list to have a 'HasLeiosVoting' instance.
instance
  All HasLeiosVoting xs =>
  HasLeiosVoting (HardForkBlock xs)
  where
  getLeiosCommittee (HardForkLedgerState (HardForkState tele)) =
    hcollapse $
      hcmap
        (Proxy @HasLeiosVoting)
        (\(Current _ (Flip ls)) -> K (getLeiosCommittee ls))
        (Telescope.tip tele)

-------------------------------------------------------------------------------
-- Voting loop
-------------------------------------------------------------------------------

-- | The Leios voting loop, polymorphic in @blk@.
--
-- Subscribes to EB notifications, and for each acquired EB consults the
-- 'HasLeiosVoting' instance to look up the committee from the current
-- ledger state. Casts a vote when the operator's voter id is on the
-- committee; otherwise silently skips.
--
-- Returns immediately (after a trace) if no voting key is configured.
runLeiosVoting ::
  forall m blk.
  (IOLike m, HasLeiosVoting blk) =>
  Tracer m TraceLeiosKernel ->
  ChainDB m blk ->
  LeiosDbHandle m ->
  LeiosVoteState m ->
  Maybe VotingKey ->
  m ()
runLeiosVoting tracer chainDB leiosDB voteState = \case
  Nothing ->
    traceWith tracer $
      MkTraceLeiosKernel
        "runLeiosVoting: disabled because no topLevelConfigVotingKey"
  Just votingKey -> do
    -- FIXME: Lucky seed has two identical voter ids. Fix via proper committee
    -- selection from all registered keys.
    let me = MkVoterId . fromIntegral $ BS.head votingKey
        LeiosVoteState{addVote} = voteState
    chan <- subscribeEbNotifications leiosDB
    let getNext f =
          atomically (readTChan chan) >>= \case
            AcquiredEb{} -> pure ()
            AcquiredEbTxs point -> f point
    forever $ getNext $ \point -> do
      committee <- atomically $
        getLeiosCommittee . ledgerState <$> ChainDB.getCurrentLedger chainDB
      when (isCommitteeMember committee me) $ do
        -- TODO: validate EB closures against selected chain
        -- TODO: create vote (sign the eb hash)
        let vote =
              MkLeiosVote
                { electionId = point.pointSlotNo
                , voterId = me
                , ebHash = point.pointEbHash
                , voteSignature = True
                }
        addVote vote
        traceWith tracer TraceLeiosVoted{point, voter = me}
        traceWith tracer TraceLeiosVoteAcquired{point, voter = me}
