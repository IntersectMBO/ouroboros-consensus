{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Leios voting: cast a vote on each acquired EB, governed by the voting
-- committee selected by the active era from its ledger state.
module LeiosVoting
  ( module LeiosVoting
  , HasLeiosVoting (..)
  ) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN))
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( HasLeiosVoting (..)
  , LeiosSigningKey
  , TraceLeiosKernel (..)
  , getLeiosVoterId
  , signLeiosVote
  )
import LeiosVoteState (AddVoteResult (..), LeiosVoteState (..))
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
      case getLeiosCommittee ls >>= getLeiosVoterId vk of
        Nothing -> pure ()
        Just voterId -> do
          -- TODO: check if its not too late to vote before/after validation
          -- TODO: validate EB closures against selected chain
          let vote = signVote voterId point
          -- NOTE: Self-validation of vote could be skipped, but useful for
          -- determining and tracing the weight.
          addVote vote >>= \case
            Added weight mCert -> do
              traceWith tracer TraceLeiosVoted{vote, weight}
              traceWith tracer TraceLeiosVoteAcquired{vote}
              -- Trace certification whenever the tally for this point
              -- is past 'minCertificationThreshold'. May fire more than
              -- once per point if subsequent votes also come in here;
              -- consumers (e.g. ThreadNet's 'propCertifying') dedupe
              -- by point.
              case mCert of
                Just _ -> traceWith tracer TraceLeiosCertified{point}
                Nothing -> pure ()
            err ->
              error $ "runLeiosVoting: unexpected error on addVote: " <> show err
 where
  getCurrentLedgerState =
    atomically $ ledgerState <$> ChainDB.getCurrentLedger chainDB
