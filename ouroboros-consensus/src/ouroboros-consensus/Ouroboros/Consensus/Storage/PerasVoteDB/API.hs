{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , AddPerasVoteResult (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo

    -- * Invariants
  , prop_addVoteThenGetVoteIds
  , prop_getVotesAfterZero
  , prop_getVotesAfterMonotonic
  , prop_garbageCollectRemovesOldVotes
  , prop_addVoteThenGetForgedCertForRound
  ) where

import Control.Monad (join)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM (..))

data PerasVoteDB m blk = PerasVoteDB
  { addVote ::
      WithArrivalTime (ValidatedPerasVote blk) ->
      STM m (m (AddPerasVoteResult blk))
  -- ^ Add a Peras vote to the database. The result indicates whether the vote
  -- was actually added, or if it was already present.
  --
  -- NOTE: the resulting computation over 'm' is there solely for tracing
  -- purposes. Use the `join . atomically` pattern to consume its output.
  , getVoteIds ::
      STM m (Set (PerasVoteId blk))
  -- ^ Get the set of all vote IDs currently in the database.
  , getVotesAfter ::
      PerasVoteTicketNo ->
      STM m (Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ Get all votes with a ticket number strictly greater than the given one,
  -- in ascending order.
  , getForgedCertForRound ::
      PerasRoundNo ->
      STM m (Maybe (ValidatedPerasCert blk))
  -- ^ Get the certificate if quorum was reached for the given round.
  , garbageCollect ::
      PerasRoundNo ->
      STM m (m ())
  -- ^ Garbage-collect state strictly older than the given slot number.
  --
  -- NOTE: the resulting computation over 'm' is there solely for tracing
  -- purposes. Use the `join . atomically` pattern to consume its output.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDB" (PerasVoteDB m blk)

-- | A sequence number, incremented every time we receive a new vote.
newtype PerasVoteTicketNo = PerasVoteTicketNo Word64
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasVoteTicketNo :: PerasVoteTicketNo
zeroPerasVoteTicketNo = PerasVoteTicketNo 0

data AddPerasVoteResult blk
  = PerasVoteAlreadyInDB
  | AddedPerasVoteButDidntGenerateNewCert
  | AddedPerasVoteAndGeneratedNewCert (ValidatedPerasCert blk)
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- * Invariants

-- | After adding a vote, its ID should be present in 'getVoteIds'.
prop_addVoteThenGetVoteIds ::
  MonadSTM m =>
  PerasVoteDB m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  m Bool
prop_addVoteThenGetVoteIds db vote =
  atomically $ do
    let voteId = getPerasVoteId vote
    _ <- addVote db vote
    voteIds <- getVoteIds db
    pure $
      voteId `elem` voteIds

-- | 'getVotesAfter' with ticket 0 should return all votes in the database.
prop_getVotesAfterZero ::
  MonadSTM m =>
  PerasVoteDB m blk ->
  m Bool
prop_getVotesAfterZero db =
  atomically $ do
    allVotes <- getVotesAfter db zeroPerasVoteTicketNo
    voteIds <- getVoteIds db
    let allVoteIds =
          Set.fromList $
            fmap (getPerasVoteId . forgetArrivalTime) $
              Map.elems allVotes
    pure $
      length allVotes == Set.size voteIds
        && allVoteIds == voteIds

-- | 'getVotesAfter' returns strictly increasing ticket numbers.
prop_getVotesAfterMonotonic ::
  MonadSTM m =>
  PerasVoteDB m blk ->
  PerasVoteTicketNo ->
  m Bool
prop_getVotesAfterMonotonic db ticketNo =
  atomically $ do
    votes <- getVotesAfter db ticketNo
    let tickets = Map.keys votes
    pure $
      tickets == List.sort tickets
        && all (> ticketNo) tickets

-- | After garbage collection for round N, no votes for rounds <N should remain.
prop_garbageCollectRemovesOldVotes ::
  MonadSTM m =>
  PerasVoteDB m blk ->
  PerasRoundNo ->
  m Bool
prop_garbageCollectRemovesOldVotes db roundNo =
  atomically $ do
    _ <- garbageCollect db roundNo
    allVotes <- getVotesAfter db zeroPerasVoteTicketNo
    let roundsNos = getPerasVoteRound . forgetArrivalTime <$> Map.elems allVotes
    pure $
      all (>= roundNo) roundsNos

-- | When adding a vote results in a certificate just being forged for a round,
-- this certificate should also be retrievable via 'getForgedCertForRound'.
prop_addVoteThenGetForgedCertForRound ::
  (MonadSTM m, StandardHash blk) =>
  PerasVoteDB m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  m Bool
prop_addVoteThenGetForgedCertForRound db vote = do
  join $ atomically $ do
    getAddVoteResult <- addVote db vote
    let voteRoundNo = getPerasVoteRound (forgetArrivalTime vote)
    retrievedCert <- getForgedCertForRound db voteRoundNo
    pure $ do
      getAddVoteResult >>= \case
        -- Just forged a new certificate, so it should be retrievable.
        AddedPerasVoteAndGeneratedNewCert forgedCert ->
          pure $ Just forgedCert == retrievedCert
        -- None of the other two cases imply there existence of a certificate
        -- for this round, so we can't make any assumptions about it here.
        _ ->
          pure True
