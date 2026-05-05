{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Leios voting: cast a vote on each acquired EB, governed by the voting
-- committee selected by the active era from its ledger state.
module LeiosVoting (module LeiosVoting) where

import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( Committee
  , LeiosVote (..)
  , TraceLeiosKernel (..)
  , VoterId (..)
  )
import LeiosVoteState (LeiosVoteState (..))
import Ouroboros.Consensus.Config (VotingKey)
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, LedgerState)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Util.IOLike (IOLike, atomically)

-- * Voting loop

-- | Long-running thread, that issues votes if we have a voting key and are part
-- of the committee.
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
      -- TODO: check only once per era whether we are part of the committee?
      mCommittee <-
        atomically $
          getLeiosCommittee . ledgerState <$> ChainDB.getCurrentLedger chainDB
      case mCommittee of
        Nothing -> pure ()
        Just _committee -> do
          -- TODO: gate on committee membership once 'Committee' is fleshed out
          -- TODO: validate EB closures against selected chain
          -- TODO: create vote (sign the eb hash)
          let vote =
                MkLeiosVote
                  { point
                  , voterId = me
                  , voteSignature = True
                  }
          -- Store vote in memory and notify downstream peers
          addVote vote
          traceWith tracer TraceLeiosVoted{vote}
          traceWith tracer TraceLeiosVoteAcquired{vote}

-- * Committee selection

class HasLeiosVoting blk where
  -- | The voting committee for the current ledger state, or 'Nothing' if
  -- the era does not participate in Leios voting.
  getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe Committee
  getLeiosCommittee _ = Nothing
