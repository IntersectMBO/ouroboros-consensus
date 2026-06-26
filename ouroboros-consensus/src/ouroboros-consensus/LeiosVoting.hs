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
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever)
import Control.Tracer (Tracer, traceWith)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import LeiosDemoDb (LeiosDbHandle (..), LeiosEbNotification (..))
import LeiosDemoTypes
  ( EbHash
  , HasLeiosVoting (..)
  , LeiosPoint (..)
  , LeiosSigningKey
  , RbHash (..)
  , TraceLeiosKernel (..)
  , getVoterId
  , signLeiosVote
  )
import LeiosVoteState (AddVoteResult (..), LeiosVoteState (..))
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , HasHeader
  , Header
  , StandardHash
  , headerHash
  )
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , headerLeiosAnnouncement
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, atomically)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- * Voting loop

-- | Long-running thread, that issues votes if we have a voting key and are part
-- of the committee.
runLeiosVoting ::
  forall m blk.
  ( IOLike m
  , HasLeiosVoting blk
  , ResolveLeiosBlock blk
  , ConvertRawHash blk
  , HasHeader (Header blk)
  , StandardHash blk
  , Typeable blk
  ) =>
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
            AcquiredEbTxs point mRbHash -> f point mRbHash

    -- Enter voting loop
    forever $ getNext $ \announcingPoint mNotifiedRbHash -> do
      -- We vote on the hash of the ranking block that announced this EB.
      --
      -- * If we forged the EB ourselves, the 'LeiosEbNotification' already carries that
      --   hash.
      -- * Otherwise, we've received an EB from a network peer, and we derive
      --   the announcing RB hash from our selected chain: the
      --   most recent header that announces this EB. If it is no
      --   longer avaliable (we switched to a different fork), we do not vote.
      (ls, mChainRbHash) <- atomically $ do
        extLedger <- ChainDB.getCurrentLedger chainDB
        frag <- ChainDB.getCurrentChain chainDB
        -- Note: here we will traverse the currently selected volatile chain fragment,
        --       i.e. at most k headers.
        let announcingRbHash = findAnnouncingRbHash frag (pointEbHash announcingPoint)
        pure (ledgerState extLedger, announcingRbHash)
      let mAnnouncingRbHash = maybe mChainRbHash Just mNotifiedRbHash
      -- TODO: check only once per era whether we are part of the committee?
      case (mAnnouncingRbHash, getLeiosCommittee ls >>= getVoterId vk) of
        (Just announcingRbHash, Just voterId) -> do
          -- TODO: check if its not too late to vote before/after validation
          -- TODO: validate EB closures against selected chain
          let vote = signVote voterId announcingRbHash
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
                Just _ ->
                  traceWith tracer TraceLeiosCertified{point = announcingPoint, rbHash = announcingRbHash}
                Nothing -> pure ()
            err ->
              error $ "runLeiosVoting: unexpected error on addVote: " <> show err
        _ ->
          -- do not vote if neither received a 'LeiosEbNotification' bearing and 'RbHash',
          -- nor could we find an 'RbHash' in an announcement on our current fork.
          -- TODO(geo2a): emit a trace here saying why we skipped the vote.
          pure ()


-- | The hash of the most recent ranking block on the given chain fragment that
-- announced the given EB, if any.
findAnnouncingRbHash ::
  forall blk.
  ( ResolveLeiosBlock blk
  , ConvertRawHash blk
  , HasHeader (Header blk)
  , StandardHash blk
  , Typeable blk
  ) =>
  AF.AnchoredFragment (Header blk) ->
  EbHash ->
  Maybe RbHash
findAnnouncingRbHash frag ebHash =
  rbHashOf <$> find announcesEb (AF.toNewestFirst frag)
 where
  announcesEb hdr =
    (pointEbHash . fst <$> headerLeiosAnnouncement @blk hdr) == Just ebHash
  rbHashOf hdr = MkRbHash (toRawHash (Proxy @blk) (headerHash hdr))
