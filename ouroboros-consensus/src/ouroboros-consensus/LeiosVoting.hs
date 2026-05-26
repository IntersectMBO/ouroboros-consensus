{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Leios voting: cast a vote on each acquired EB, governed by the voting
-- committee selected by the active era from its ledger state.
module LeiosVoting (module LeiosVoting) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( Committee
  , LeiosSigningKey
  , TraceLeiosKernel (..)
  , getVoterId
  , signLeiosVote
  )
import LeiosVoteState (AddVoteResult (..), LeiosVoteState (..))
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
  Maybe LeiosSigningKey ->
  m ()
runLeiosVoting tracer chainDB leiosDB voteState = \case
  Nothing ->
    traceWith tracer $
      MkTraceLeiosKernel
        "runLeiosVoting: disabled because no topLevelConfigVotingKey"
  Just sk -> do
    let vk = deriveVerKeyDSIGN sk
    let signVote = signLeiosVote sk
    let LeiosVoteState{addVote} = voteState
    chan <- subscribeEbNotifications leiosDB
    let getNext f =
          atomically (readTChan chan) >>= \case
            AcquiredEb{} -> pure ()
            AcquiredEbTxs point -> f point

    -- Enter voting loop
    forever $ getNext $ \point -> do
      -- TODO: check only once per era whether we are part of the committee?
      ls <- getCurrentLedgerState
      case getLeiosCommittee ls >>= getVoterId vk of
        Nothing -> pure ()
        Just voterId -> do
          -- TODO: validate EB closures against selected chain
          let vote = signVote voterId point
          -- NOTE: Self-validation of vote could be skipped, but useful for
          -- determining and tracing the weight.
          addVote vote >>= \case
            Added weight -> do
              traceWith tracer TraceLeiosVoted{vote, weight}
              traceWith tracer TraceLeiosVoteAcquired{vote}
            err ->
              error $ "runLeiosVoting: unexpected error on addVote: " <> show err
 where
  getCurrentLedgerState =
    atomically $ ledgerState <$> ChainDB.getCurrentLedger chainDB

-- * Committee selection

class HasLeiosVoting blk where
  -- | The voting committee for the current ledger state, or 'Nothing' if
  -- the era does not participate in Leios voting.
  getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe Committee
  getLeiosCommittee _ = Nothing
