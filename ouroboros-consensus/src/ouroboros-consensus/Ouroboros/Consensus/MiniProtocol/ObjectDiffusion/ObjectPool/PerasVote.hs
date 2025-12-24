{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- votes from the 'PerasVoteDB' (or the 'ChainDB' which is wrapping the
-- 'PerasVoteDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
  ( makePerasVotePoolReaderFromVoteDB
  , makePerasVotePoolWriterFromVoteDB
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  , addArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB
  , PerasVoteSnapshot
  , PerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB.API as PerasVoteDB
import Ouroboros.Consensus.Util.IOLike

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

makePerasVotePoolReaderFromSnapshot ::
  IOLike m =>
  STM m (PerasVoteSnapshot blk) ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromSnapshot getVoteSnapshot =
  ObjectPoolReader
    { oprObjectId = getPerasVoteId
    , oprZeroTicketNo = PerasVoteDB.zeroPerasVoteTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        voteSnapshot <- getVoteSnapshot
        let votesAfterLastKnown =
              PerasVoteDB.getVotesAfter voteSnapshot lastKnown
        let loadVotesAfterLastKnown =
              pure $
                fmap
                  (vpvVote . forgetArrivalTime)
                  (takeAscMap (fromIntegral limit) votesAfterLastKnown)
        pure $
          if Map.null votesAfterLastKnown
            then Nothing
            else Just loadVotesAfterLastKnown
    }

makePerasVotePoolReaderFromVoteDB ::
  IOLike m =>
  PerasVoteDB m blk -> ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromVoteDB perasVoteDB =
  makePerasVotePoolReaderFromSnapshot (PerasVoteDB.getVoteSnapshot perasVoteDB)

makePerasVotePoolWriterFromVoteDB ::
  (StandardHash blk, IOLike m) =>
  -- TODO: We probably want to be able to fetch updated stake distribution throughout
  -- the lifetime of the writer
  -- But `StrictTVar m PerasVoteStakeDistr` might not be the best choice for that.
  StrictTVar m PerasVoteStakeDistr ->
  SystemTime m ->
  PerasVoteDB m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromVoteDB distrVar systemTime perasVoteDB =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes -> do
        distr <- readTVarIO distrVar
        addPerasVotes distr systemTime (PerasVoteDB.addVote perasVoteDB) votes
    , opwHasObject = do
        voteSnapshot <- PerasVoteDB.getVoteSnapshot perasVoteDB
        pure $ PerasVoteDB.containsVote voteSnapshot
    }

data PerasVoteInboundException
  = forall blk. PerasVoteValidationError (PerasValidationErr blk)

deriving instance Show PerasVoteInboundException

instance Exception PerasVoteInboundException

-- | Validate a list of 'PerasVote's, throwing a 'PerasVoteInboundException' if
-- any of them are invalid.
validatePerasVotes ::
  (StandardHash blk, MonadThrow m) =>
  PerasVoteStakeDistr ->
  [PerasVote blk] ->
  m [ValidatedPerasVote blk]
validatePerasVotes distr votes = do
  let perasParams = mkPerasParams
  -- TODO pass down 'BlockConfig' when all the plumbing is in place
  -- see https://github.com/tweag/cardano-peras/issues/73
  -- see https://github.com/tweag/cardano-peras/issues/120
  case traverse (validatePerasVote perasParams distr) votes of
    Left validationErr -> throw (PerasVoteValidationError validationErr)
    Right validatedVotes -> return validatedVotes

-- | Add a list of 'PerasVote's into an object pool.
--
-- NOTE: we first validate the votes, throwing an exception if any of
-- them are invalid. We then wrap them with their arrival time, and finally add
-- them to the pool using the provided adder function.
--
-- The order of the first two operations (i.e., validation and timestamping) are
-- rather arbitrary, and the abstract Peras protocol just assumes it can happen
-- "within" a slot.
addPerasVotes ::
  (StandardHash blk, MonadThrow m) =>
  PerasVoteStakeDistr ->
  SystemTime m ->
  (WithArrivalTime (ValidatedPerasVote blk) -> m a) ->
  [PerasVote blk] ->
  m ()
addPerasVotes distr systemTime adder votes = do
  validatePerasVotes distr votes
    >>= mapM (addArrivalTime systemTime)
    >>= mapM_ adder
