{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LeiosVoteState (module LeiosVoteState) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , STM
  , atomically
  , dupTChan
  , newBroadcastTChan
  , newTVar
  , readTChan
  , readTVar
  , writeTChan
  , writeTVar
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LeiosDemoTypes
  ( LeiosCert
  , LeiosCommittee
  , LeiosSeatId
  , LeiosSignature
  , LeiosVote (..)
  , RbHash
  , VoteInvalid (..)
  , Weight
  , aggregateLeiosCert
  , minCertificationThreshold
  , validateLeiosVote
  )

-- FIXME: Garbage collection of vote state
data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m AddVoteResult
  -- ^ Add a new vote to the LeiosVoteState. Adding the same vote multiple
  -- times will not result in multiple notifications to subscribers.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  , queryCert :: RbHash -> m (Maybe LeiosCert)
  -- ^ Look up the assembled certificate for a 'RbHash', or
  -- 'Nothing' if its collected votes haven't crossed
  -- 'minCertificationThreshold'.
  }

data AddVoteResult
  = NoCommittee
  | VoteInvalid VoteInvalid
  | AlreadyKnown
  | -- | The vote was added to the state. The first 'Weight' is this voter's own
    -- weight; the second is the running per-point tally after this addition.
    -- The LeiosCert is 'Just' whenever that tally is at or above
    -- 'minCertificationThreshold'.
    --
    -- Both fields are 'Weight', so transposing them at a construction or
    -- destructuring site type checks and yields plausible but wrong telemetry.
    -- Keep the order above when touching either this constructor or its three
    -- use sites.
    --
    -- The tally is surfaced rather than traced where it is computed because
    -- the update runs in STM, which cannot trace. Callers emit it, as they
    -- already do for certification.
    Added !Weight !Weight (Maybe LeiosCert)
  deriving (Eq, Show)

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Per-'RbHash' tally we maintain inside 'newLeiosVoteState'.
-- Holds the contributing voters plus a memoised certificate once the
-- threshold is crossed.
data PointState = PointState
  { psVoters :: !(Map LeiosSeatId (Weight, LeiosSignature))
  , psTotal :: !Weight
  -- ^ Running sum of 'psVoters' weights, maintained incrementally. Kept in the
  -- state rather than recomputed per vote: summing the map is linear in the
  -- committee, so recomputing made the per-point cost quadratic, and every
  -- post-threshold vote paid it too once the tally started being reported.
  , psCert :: !(Maybe LeiosCert)
  -- ^ Assembled once when this point's total weight first reaches
  -- 'minCertificationThreshold'; reused for subsequent post-threshold
  -- votes so we don't keep rerunning BLS aggregation.
  }

emptyPointState :: PointState
emptyPointState = PointState Map.empty 0 Nothing

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState ::
  MonadSTM m =>
  -- | Get the current 'LeiosCommittee'.
  STM m (Maybe LeiosCommittee) ->
  m (LeiosVoteState m)
newLeiosVoteState getCommittee = do
  votesChan <- atomically newBroadcastTChan
  seenVotes <- atomically $ newTVar Set.empty
  pointStates <- atomically $ newTVar (Map.empty :: Map RbHash PointState)
  pure
    LeiosVoteState
      { addVote = \vote -> do
          -- Validate outside the transaction: the BLS pairing is ms-scale, and
          -- inside 'atomically' every conflicting commit re-ran it. Worst case
          -- now is one redundant verification per concurrently-received duplicate.
          alreadySeen <- atomically $ Set.member vote <$> readTVar seenVotes
          if alreadySeen
            then pure AlreadyKnown
            else do
              -- TODO: disallow votes from different epoch (than the committee is).
              -- Could use slot numbers or put epoch into votes to distinguish?
              atomically getCommittee >>= \case
                Nothing -> pure NoCommittee
                Just committee ->
                  case validateLeiosVote committee vote of
                    Left reason -> pure $ VoteInvalid reason
                    Right weight -> atomically $ do
                      seen <- readTVar seenVotes
                      if Set.member vote seen
                        then pure AlreadyKnown
                        else do
                          writeTVar seenVotes $! Set.insert vote seen
                          writeTChan votesChan vote

                          -- FIXME: This code is not only ugly, but we need to also
                          -- keep track of which committee the cert is for. We shall
                          -- only use the cert (return on queryCert) if we are in
                          -- the same epoch as when it was aggregated / the
                          -- committee still the same.

                          -- Update the per-point tally, assembling (and
                          -- caching) the certificate the first time the
                          -- threshold is crossed.
                          states <- readTVar pointStates
                          let pst = Map.findWithDefault emptyPointState vote.announcingRbHash states
                              -- 'Map.insert' replaces any entry this seat already
                              -- had, so the running total must drop the old weight
                              -- rather than simply adding the new one.
                              (mOld, voters') =
                                Map.insertLookupWithKey
                                  (\_ new _old -> new)
                                  vote.voterId
                                  (weight, vote.voteSignature)
                                  pst.psVoters
                              totalW = pst.psTotal + weight - maybe 0 fst mOld
                              pst' = pst{psVoters = voters', psTotal = totalW}
                              pst'' = case pst.psCert of
                                Just _ -> pst'
                                Nothing
                                  | totalW >= minCertificationThreshold ->
                                      -- Voters were validated against this committee before
                                      -- being added and the per-voter signatures already
                                      -- passed individual verification, so aggregation must
                                      -- succeed. TODO: replace 'error' with a tracer.
                                      case aggregateLeiosCert committee (fmap snd pst'.psVoters) of
                                        Left e ->
                                          error $
                                            "LeiosVoteState.addVote: aggregateLeiosCert "
                                              <> "failed on validated votes; should not happen: "
                                              <> show e
                                        Right cert -> pst'{psCert = Just cert}
                                  | otherwise -> pst'
                          writeTVar pointStates $! Map.insert vote.announcingRbHash pst'' states
                          pure $ Added weight totalW pst''.psCert
      , subscribeVotes = do
          chan <- atomically $ dupTChan votesChan
          pure $
            LeiosVoteSubscription
              { getNextVote = readTChan chan
              }
      , queryCert = \pt -> atomically $ do
          states <- readTVar pointStates
          pure $ Map.lookup pt states >>= psCert
      }
