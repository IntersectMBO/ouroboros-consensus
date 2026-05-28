{-# LANGUAGE FlexibleContexts #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- votes from the 'PerasVoteDB' (or the 'ChainDB' which is wrapping the
-- 'PerasVoteDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
  ( makePerasVotePoolReaderFromVoteDB
  , makePerasVotePoolWriterFromVoteDB
  , makePerasVotePoolReaderFromChainDB
  , makePerasVotePoolWriterFromChainDB
  ) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (PerasVote)
  , IsPerasVote
  , PerasVoteId
  , ValidatedPerasVote (vpvVote)
  , getPerasVoteId
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.Peras.Context (PerasEpochContextResolverHandle, verifyPerasVoteInContext)
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB.API as PerasVoteDB
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM (STM, atomically)
  )

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

-------------------------------------------------------------------------------
-- Readers
-------------------------------------------------------------------------------

-- | Internal helper: create a pool reader from a @getVotesAfter@ function.
makePerasVotePoolReader ::
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
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
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasVoteDB m blk ->
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReaderFromVoteDB perasVoteDB =
  makePerasVotePoolReader
    (PerasVoteDB.getVotesAfter perasVoteDB)

makePerasVotePoolReaderFromChainDB ::
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
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
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  SystemTime m ->
  PerasVoteDB m blk ->
  PerasEpochContextResolverHandle m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromVoteDB systemTime perasVoteDB resolverHandle =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes -> do
        now <- systemTimeCurrent systemTime
        atomically $ do
          alreadyInDb <- PerasVoteDB.getVoteIds perasVoteDB
          let votesNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasVoteId) votes
          validatedVotes <- traverse (verifyPerasVoteInContext resolverHandle) votesNotAlreadyInDb
          -- Some votes are invalid => reject the whole batch
          -- We could combine the two 'traverse' operations into one in which case
          -- any validated vote would be immediately added no matter what is the
          -- validity of the other votes in the batch.
          traverse_ (PerasVoteDB.addVote perasVoteDB . WithArrivalTime now) validatedVotes
    , opwHasObject = do
        voteIds <- PerasVoteDB.getVoteIds perasVoteDB
        pure $ \voteId -> Set.member voteId voteIds
    }

-- | Create a pool writer from the 'ChainDB'.
-- This properly handles the produced certs by letting the ChainDB take care
-- of them (see 'ChainDB.addPerasVoteWithAsyncCertHandling').
makePerasVotePoolWriterFromChainDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  SystemTime m ->
  ChainDB m blk ->
  PerasEpochContextResolverHandle m blk ->
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m
makePerasVotePoolWriterFromChainDB systemTime chainDB resolverHandle =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes -> do
        now <- systemTimeCurrent systemTime
        validatedVotes <- atomically $ do
          alreadyInDb <- ChainDB.getPerasVoteIds chainDB
          let votesNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasVoteId) votes
          traverse (verifyPerasVoteInContext resolverHandle) votesNotAlreadyInDb
        -- Some votes are invalid => reject the whole batch
        -- We could combine the two 'traverse' operations into one in which case
        -- any validated vote would be immediately added no matter what is the
        -- validity of the other votes in the batch.
        traverse_ (ChainDB.addPerasVoteWithAsyncCertHandling chainDB . WithArrivalTime now) validatedVotes
    , opwHasObject = do
        voteIds <- ChainDB.getPerasVoteIds chainDB
        pure $ \voteId -> Set.member voteId voteIds
    }
