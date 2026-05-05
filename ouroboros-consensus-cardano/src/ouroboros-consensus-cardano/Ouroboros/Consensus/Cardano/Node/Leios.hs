{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cardano-specific Leios threads, hard-coded to
-- @CardanoBlock StandardCrypto@ (and ultimately @ConwayEra@). This module
-- is the wiring point for the voting thread today, and will host the
-- certifying / fetching threads as they are added.
--
-- 'cardanoLeiosNodeKernelHook' is intended to be passed via
-- 'initNodeKernelHook' on 'NodeKernelArgs' by both the production
-- @cardano-node@ and the ThreadNet test framework.
module Ouroboros.Consensus.Cardano.Node.Leios
  ( cardanoLeiosNodeKernelHook
  , runLeiosVoting
  ) where

import Cardano.Protocol.Crypto (StandardCrypto)
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever, unless, void)
import Control.ResourceRegistry (ResourceRegistry, forkLinkedThread)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( LeiosPoint (..)
  , LeiosVote (..)
  , TraceLeiosKernel (..)
  , VoterId (..)
  )
import LeiosVoteState (LeiosVoteState (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig (..), VotingKey)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Node.Tracers (leiosKernelTracer)
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Util.IOLike (IOLike, atomically)

-- | NodeKernel hook that wires up Cardano-specific Leios threads.
--
-- Currently spawns the Leios voting thread when 'topLevelConfigVotingKey' is
-- present. Future Leios threads (certifying, fetching, …) will be added here.
cardanoLeiosNodeKernelHook ::
  IOLike m =>
  ResourceRegistry m ->
  NodeKernel m addrNTN addrNTC (CardanoBlock StandardCrypto) ->
  m ()
cardanoLeiosNodeKernelHook registry kernel = do
  let tracer = leiosKernelTracer (getTracers kernel)
      cfg = getTopLevelConfig kernel
  case topLevelConfigVotingKey cfg of
    Nothing ->
      traceWith tracer $
        MkTraceLeiosKernel
          "cardanoLeiosNodeKernelHook: voting disabled because no topLevelConfigVotingKey"
    Just votingKey ->
      void $
        forkLinkedThread registry "Cardano.LeiosVoting" $
          runLeiosVoting
            tracer
            (getChainDB kernel)
            (leiosDB kernel)
            (leiosVoteState kernel)
            votingKey

-- | The Leios voting loop, hard-coded to @CardanoBlock StandardCrypto@.
--
-- The current implementation is era-agnostic at runtime; the concrete block
-- type is the eventual home for Conway-specific committee selection and
-- vote signing.
runLeiosVoting ::
  IOLike m =>
  Tracer m TraceLeiosKernel ->
  ChainDB m (CardanoBlock StandardCrypto) ->
  LeiosDbHandle m ->
  LeiosVoteState m ->
  VotingKey ->
  m ()
runLeiosVoting tracer chainDB leiosDB voteState votingKey = do
  -- TODO: derive from committee within ledger state, also move within loop
  -- (changes across epochs)
  -- FIXME: This is now a LedgerState (HardForkBlock CardanoEras) mk .. not easy to work with!
  _ls <- atomically $ ledgerState <$> ChainDB.getCurrentLedger chainDB
  -- FIXME: Lucky seed has two identical voter ids. Fix via proper committee
  -- selection from all registered keys.
  let me = MkVoterId . fromIntegral $ BS.head votingKey
      LeiosVoteState{addVote} = voteState
  chan <- subscribeEbNotifications leiosDB
  let getNext f =
        atomically (readTChan chan) >>= \case
          AcquiredEb{} -> pure ()
          AcquiredEbTxs point -> f point
  forever $ do
    -- TODO: poll available EBs instead? Otherwise we would not vote when we
    -- switch to a chain only later.
    getNext $ \point -> do
      let tooOld = const False -- TODO: check tip or wall clock time
      unless (tooOld point) $ do
        -- TODO: check whether already voted
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
