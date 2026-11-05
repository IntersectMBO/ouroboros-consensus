{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- votes from the 'PerasVoteDB' (or the 'ChainDB' which is wrapping the
-- 'PerasVoteDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
  ( makePerasVotePoolReaderFromChainDB
  , makePerasVotePoolWriterFromChainDB

    -- * For testing purposes
  , makeTestPerasVotePoolReaderFromVoteDB
  , makeTestPerasVotePoolWriterFromVoteDB
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasVote
  , PerasVoteId
  , ValidatedPerasVote (..)
  , getPerasVoteId
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectIdRequestability (ObjectIdRequestable)
  , ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolverHandle
  , verifyPerasVoteWithHandle
  )
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddPerasVoteResult
      ( AddedPerasVoteAndGeneratedNewCert
      , AddedPerasVoteButDidntGenerateNewCert
      , PerasVoteAlreadyInDB
      )
  , ChainDB
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB
  , PerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB.API as PerasVoteDB
import Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (..))

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
  ObjectPoolReader PerasVoteId (PerasVote blk) PerasVoteTicketNo m
makePerasVotePoolReader getVotesAfterSTM =
  ObjectPoolReader
    { oprObjectId = getPerasVoteId
    , oprZeroTicketNo = PerasVoteDB.zeroPerasVoteTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        votesAfterLastKnownNoLimit <- getVotesAfterSTM lastKnown
        if Map.null votesAfterLastKnownNoLimit
          then pure Nothing
          else pure . Just $ do
            let votesAfterLastKnown = takeAscMap (fromIntegral limit) votesAfterLastKnownNoLimit
            pure $ Map.map (vpvVote . forgetArrivalTime) votesAfterLastKnown
    }

makeTestPerasVotePoolReaderFromVoteDB ::
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasVoteDB m blk ->
  ObjectPoolReader PerasVoteId (PerasVote blk) PerasVoteTicketNo m
makeTestPerasVotePoolReaderFromVoteDB perasVoteDB =
  makePerasVotePoolReader
    (PerasVoteDB.getVotesAfter perasVoteDB)

makePerasVotePoolReaderFromChainDB ::
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
  ChainDB m blk ->
  ObjectPoolReader PerasVoteId (PerasVote blk) PerasVoteTicketNo m
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
makeTestPerasVotePoolWriterFromVoteDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  SystemTime m ->
  PerasVoteDB m blk ->
  PerasEpochContextResolverHandle m blk ->
  ObjectPoolWriter PerasVoteId (PerasVote blk) m
makeTestPerasVotePoolWriterFromVoteDB systemTime perasVoteDB resolverHandle =
  ObjectPoolWriter
    { opwObjectId = getPerasVoteId
    , opwAddObjects = \votes -> do
        now <- systemTimeCurrent systemTime
        addVoteResults <- atomically $ do
          alreadyInDb <- PerasVoteDB.getVoteIds perasVoteDB
          let votesNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasVoteId) votes
          validatedVotes <- traverse (verifyPerasVoteWithHandle resolverHandle) votesNotAlreadyInDb
          -- Some votes are invalid => reject the whole batch
          --
          -- NOTE: we could combine the two 'traverse' operations into one in
          -- which case any validated vote would be immediately added no matter
          -- what is the validity of the other votes in the batch.
          traverse
            (\c -> (,) (getPerasVoteId c) <$> PerasVoteDB.addVote perasVoteDB (WithArrivalTime now c))
            validatedVotes
        fmap (,Set.empty)
          -- Set PerasVoteId
          . fmap Set.fromList
          -- [PerasVoteId]
          . (fmap . fmap) fst
          -- [(PerasVoteId, AddPerasVoteResult blk)]
          . fmap
            ( filter $ \case
                (_, AddedPerasVoteButDidntGenerateNewCert) -> True
                (_, AddedPerasVoteAndGeneratedNewCert{}) -> True
                (_, PerasVoteAlreadyInDB) -> False
            )
          -- [(PerasVoteId, AddPerasVoteResult blk)]
          . (traverse . traverse) id
          -- [(PerasVoteId, m (AddPerasVoteResult blk))]
          $ addVoteResults
    , opwHasObject = do
        voteIds <- PerasVoteDB.getVoteIds perasVoteDB
        pure $ \voteId -> Set.member voteId voteIds
    , opwObjectIdRequestability = pure $ const ObjectIdRequestable
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
  ObjectPoolWriter PerasVoteId (PerasVote blk) m
makePerasVotePoolWriterFromChainDB systemTime chainDB =
  let resolverHandle = ChainDB.getPerasEpochContextResolverHandle chainDB
   in ObjectPoolWriter
        { opwObjectId = getPerasVoteId
        , opwAddObjects = \votes -> do
            now <- systemTimeCurrent systemTime
            validatedVotes <- atomically $ do
              alreadyInDb <- ChainDB.getPerasVoteIds chainDB
              let votesNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasVoteId) votes
              traverse (verifyPerasVoteWithHandle resolverHandle) votesNotAlreadyInDb
            -- Some votes are invalid => reject the whole batch
            --
            -- NOTE: we could combine the two 'traverse' operations into one in
            -- which case any validated vote would be immediately added no
            -- matter what is the validity of the other votes in the batch.
            fmap (,Set.empty)
              -- Set PerasVoteId
              . fmap Set.fromList
              -- [PerasVoteId]
              . (fmap . fmap) fst
              -- [(PerasVoteId, AddPerasVoteResult blk)]
              . fmap
                ( filter $ \case
                    (_, AddedPerasVoteButDidntGenerateNewCert) -> True
                    (_, AddedPerasVoteAndGeneratedNewCert{}) -> True
                    (_, PerasVoteAlreadyInDB) -> False
                )
              -- [(PerasVoteId, AddPerasVoteResult blk)]
              . (fmap . fmap . fmap) fst
              -- [(PerasVoteId, (AddPerasVoteResult blk, Maybe (AddPerasCertPromise m)))]
              . traverse
                ( \c ->
                    (,) (getPerasVoteId c) <$> ChainDB.addPerasVoteWithAsyncCertHandling chainDB (WithArrivalTime now c)
                )
              -- [ValidatedPerasVote blk]
              $ validatedVotes
        , opwHasObject = do
            voteIds <- ChainDB.getPerasVoteIds chainDB
            pure $ \voteId -> Set.member voteId voteIds
        , opwObjectIdRequestability = pure $ const ObjectIdRequestable
        }
