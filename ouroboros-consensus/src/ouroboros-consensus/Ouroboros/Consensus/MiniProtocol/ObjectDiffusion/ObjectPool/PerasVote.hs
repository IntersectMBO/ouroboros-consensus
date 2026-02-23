{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- votes from the 'PerasVoteDB' (or the 'ChainDB' which is wrapping the
-- 'PerasVoteDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
  ( makePerasVotePoolReaderFromVoteDB
  , makePerasVotePoolWriterFromVoteDB
  ) where

import Control.Monad (join)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )
import Ouroboros.Consensus.Util.IOLike

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

makePerasVotePoolReaderFromVoteDB ::
  IOLike m =>
  PerasVoteDB m blk ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromVoteDB PerasVoteDB{..} =
  ObjectPoolReader
    { oprObjectId = getPerasVoteId
    , oprZeroTicketNo = zeroPerasVoteTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        votesAfterLastKnown <- getVotesAfter lastKnown
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

makePerasVotePoolWriterFromVoteDB ::
  (StandardHash blk, IOLike m) =>
  StrictTVar m PerasVoteStakeDistr ->
  SystemTime m ->
  PerasVoteDB m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromVoteDB distrVar systemTime PerasVoteDB{..} =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes -> do
        now <- systemTimeCurrent systemTime
        join $ atomically $ do
          distr <- readTVar distrVar
          case validatePerasVotes distr votes of
            -- All votes are valid => add them to the pool
            ([], validatedVotes) ->
              fmap sequence_ $
                sequence $
                  fmap (addVote . WithArrivalTime now) $
                    validatedVotes
            -- Some votes are invalid => reject the whole batch
            (errs, _) ->
              throw (PerasVoteValidationError errs)
    , opwHasObject = (flip Set.member) <$> getVoteIds
    }

data PerasVoteInboundException
  = forall blk. PerasVoteValidationError [PerasValidationErr blk]

deriving instance Show PerasVoteInboundException

instance Exception PerasVoteInboundException

-- | Validate a batch of Peras votes against the given stake distribution.
validatePerasVotes ::
  StandardHash blk =>
  PerasVoteStakeDistr ->
  [PerasVote blk] ->
  ([PerasValidationErr blk], [ValidatedPerasVote blk])
validatePerasVotes distr votes = do
  let perasParams = mkPerasParams
  -- TODO pass down 'BlockConfig' when all the plumbing is in place
  -- see https://github.com/tweag/cardano-peras/issues/73
  -- see https://github.com/tweag/cardano-peras/issues/120
  partitionEithers (validatePerasVote perasParams distr <$> votes)
