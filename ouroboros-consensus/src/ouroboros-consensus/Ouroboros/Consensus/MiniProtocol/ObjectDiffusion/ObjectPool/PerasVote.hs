{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- votes from the 'PerasVoteDB' (or the 'ChainDB' which is wrapping the
-- 'PerasVoteDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
  ( makePerasVotePoolReaderFromVoteDB
  , makePerasVotePoolWriterFromVoteDB
  , makePerasVotePoolReaderFromChainDB
  , makePerasVotePoolWriterFromChainDB
  ) where

import Control.Monad (join)
import Data.Either (partitionEithers)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB.API as PerasVoteDB
import Ouroboros.Consensus.Util.IOLike

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

-------------------------------------------------------------------------------
-- Readers
-------------------------------------------------------------------------------

-- | Internal helper: create a pool reader from a @getVotesAfter@ function.
makePerasVotePoolReader ::
  IOLike m =>
  ( PerasVoteTicketNo ->
    STM m (Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  ) ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReader getVotesAfterSTM =
  ObjectPoolReader
    { oprObjectId = getPerasVoteId
    , oprZeroTicketNo = zeroPerasVoteTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        votesAfterLastKnownNoLimit <- getVotesAfterSTM lastKnown
        if Map.null votesAfterLastKnownNoLimit
          then pure Nothing
          else pure . Just $ do
            let votesAfterLastKnown = takeAscMap (fromIntegral limit) votesAfterLastKnownNoLimit
            pure $ Map.map (vpvVote . forgetArrivalTime) votesAfterLastKnown
    }

makePerasVotePoolReaderFromVoteDB ::
  IOLike m =>
  PerasVoteDB m blk ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromVoteDB perasVoteDB =
  makePerasVotePoolReader
    (PerasVoteDB.getVotesAfter perasVoteDB)

makePerasVotePoolReaderFromChainDB ::
  IOLike m =>
  ChainDB m blk ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromChainDB chainDB =
  makePerasVotePoolReader
    (ChainDB.getPerasVotesAfter chainDB)

-------------------------------------------------------------------------------
-- Writers
-------------------------------------------------------------------------------

-- | Create a pool writer directly from a 'PerasVoteDB'.
-- In particular, the result of 'addVote' is ignored, so any produced cert will
-- have to be handled manually by another mean. This function is mostly meant
-- for tests against the 'PerasVoteDB' in isolation; for actual production use,
-- see 'makePerasVotePoolWriterFromChainDB' which creates a pool writer from the
-- 'ChainDB' and thus properly handles the produced certs.
makePerasVotePoolWriterFromVoteDB ::
  (StandardHash blk, IOLike m) =>
  SystemTime m ->
  -- | This is needed for validating votes (since it is during the validation of
  -- votes that we give them a verified weight. In the future, we won't read it
  -- from the stake distr directly, but rather use the committee selection data)
  STM m PerasVoteStakeDistr ->
  PerasVoteDB m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromVoteDB systemTime getStakeDistrSTM perasVoteDB =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes ->
        processVotes
          systemTime
          (PerasVoteDB.getVoteIds perasVoteDB)
          -- TODO: in the future we won't need just the stake distribution for
          -- validating votes, but also the whole committee selection context
          -- (containing vote weights of committee members = voters)
          (\vote -> getStakeDistrSTM >>= \sd -> pure $ validatePerasVote mkPerasParams sd vote)
          (void . join . atomically . PerasVoteDB.addVote perasVoteDB)
          votes
    , opwHasObject = do
        voteIds <- PerasVoteDB.getVoteIds perasVoteDB
        pure $ \voteId -> Set.member voteId voteIds
    }

-- | Create a pool writer from the 'ChainDB'.
-- This properly handles the produced certs by letting the ChainDB take care
-- of them (see 'ChainDB.addPerasVoteWithAsyncCertHandling').
makePerasVotePoolWriterFromChainDB ::
  (StandardHash blk, IOLike m) =>
  SystemTime m ->
  -- | This is needed for validating votes (since its during the validation of
  -- votes that we give them a verified weight. In the future, we won't read it
  -- from the stake distr directly, but rather use the committee selection data)
  STM m PerasVoteStakeDistr ->
  ChainDB m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromChainDB systemTime getStakeDistrSTM chainDB =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes ->
        processVotes
          systemTime
          (ChainDB.getPerasVoteIds chainDB)
          -- TODO: in the future we won't need just the stake distribution for
          -- validating votes, but also the whole committee selection context
          -- (containing vote weights of committee members = voters)
          (\vote -> getStakeDistrSTM >>= \sd -> pure $ validatePerasVote mkPerasParams sd vote)
          -- We do not want to block the writer thread on waiting for ChainSel
          -- side-effects to complete, so we use the async version of adding
          -- votes to the ChainDB and ignore the returned promise.
          -- The async action (if any) is still launched and executed behind the
          -- scenes even though we drop the promise.
          (void . ChainDB.addPerasVoteWithAsyncCertHandling chainDB)
          votes
    , opwHasObject = do
        voteIds <- ChainDB.getPerasVoteIds chainDB
        pure $ \voteId -> Set.member voteId voteIds
    }

data PerasVoteInboundException
  = forall blk. PerasVoteValidationError [PerasValidationErr blk]

deriving instance Show PerasVoteInboundException

instance Exception PerasVoteInboundException

-- | Process a batch of inbound Peras votes received from a peer.
--
-- Votes whose ID is already present in the database (as determined by
-- @alreadyInDbSTM@) are silently skipped. The remaining votes are validated;
-- if /any/ vote in the batch fails validation, the entire batch is rejected
-- by throwing a 'PerasVoteInboundException' (which should make us disconnect
-- from the distant peer, see 'withPeer' bracket function from
-- `ouroboros-network`). Otherwise, each valid vote is timestamped with the
-- current wall-clock time and added to the database via @addVote@.
processVotes ::
  MonadSTM m =>
  SystemTime m ->
  STM m (Set (PerasVoteId blk)) ->
  (PerasVote blk -> STM m (Either (PerasValidationErr blk) (ValidatedPerasVote blk))) ->
  (WithArrivalTime (ValidatedPerasVote blk) -> m ()) ->
  [PerasVote blk] ->
  m ()
processVotes systemTime alreadyInDbSTM validateVote addVote votes = do
  validationResults <- atomically $ do
    alreadyInDb <- alreadyInDbSTM
    let votesNotAlreadyInDb = filter (not . (`Set.member` alreadyInDb) . getPerasVoteId) votes
    mapM validateVote votesNotAlreadyInDb
  now <- systemTimeCurrent systemTime
  case partitionEithers validationResults of
    -- All votes are valid => add them to the pool
    ([], validatedVotes) ->
      mapM_
        (addVote . WithArrivalTime now)
        validatedVotes
    -- Some votes are invalid => reject the whole batch
    --
    -- N.B. it has been requested in PR review
    -- https://github.com/IntersectMBO/ouroboros-consensus/pull/1768#discussion_r2747873186
    -- to gather all validation errors and report them together in the exception
    -- rather than just report the first error encountered.
    -- This assumes that vote validation is cheap, which may not be true in
    -- practice depending on the actual crypto/committee selection scheme.
    -- Hence we may revisit this to lazily abort validation upon the first error
    -- encountered.
    (errs, _) ->
      throw (PerasVoteValidationError errs)
