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
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
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
  , validateLeiosVote
  )
import LeiosTxCache.API (maxAnnouncementCount)

-- | FIXME: UNSAFE bound on growth: only the most recently first-seen
-- 'maxAnnouncementCount' points are kept, and a vote for anything older is
-- treated as new. Enough to take load readings without the state growing
-- without end; not enough to stand up to anyone trying. See 'boundPoints'.
data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m AddVoteResult
  -- ^ Add a new vote to the LeiosVoteState. Adding the same vote multiple
  -- times will not result in multiple notifications to subscribers.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  , queryCert :: RbHash -> m (Maybe LeiosCert)
  -- ^ Look up the assembled certificate for a 'RbHash', or 'Nothing' if its
  -- collected votes haven't crossed 'ppLeiosQuorumStakeThresholdL'.
  }

data AddVoteResult
  = NoCommittee
  | VoteInvalid VoteInvalid
  | AlreadyKnown
  | -- | The vote was added to the state. The 'LeiosCert' is 'Just' whenever the
    -- tally is at or above the quorum threshold in force.
    --
    -- The tally is surfaced rather than traced where it is computed because
    -- the update runs in STM, which cannot trace. Callers emit it, as they
    -- already do for certification.
    Added !VoteTally (Maybe LeiosCert)
  deriving (Eq, Show)

-- | What one accepted vote did to its point's tally.
--
-- A record rather than three positional 'Weight's: transposing any two of them
-- at a construction or destructuring site would type check and yield plausible
-- but wrong telemetry.
data VoteTally = VoteTally
  { vtWeight :: !Weight
  -- ^ The accepted vote's own weight.
  , vtTally :: !Weight
  -- ^ Running per-point tally after this vote was counted.
  , vtThreshold :: !Weight
  -- ^ The quorum in force when the tally was taken. Carried alongside rather
  -- than looked up by consumers because it comes from the committee that
  -- validated this vote, and that committee turns over at epoch boundaries.
  }
  deriving (Eq, Show)

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Per-'RbHash' tally we maintain inside 'newLeiosVoteState'.
-- Holds the contributing voters plus a memoised certificate once the
-- threshold is crossed.
data PointState = PointState
  { psSeen :: !(Set LeiosVote)
  -- ^ Every vote counted for this point, so that a duplicate is recognised.
  -- Held per point rather than in one set across all points so that dropping
  -- a point drops its votes with it; a global set would have to be filtered.
  , psVoters :: !(Map LeiosSeatId (Weight, LeiosSignature))
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
emptyPointState = PointState Set.empty Map.empty 0 Nothing

-- | Whether this exact vote has already been counted for its point.
seenIn :: LeiosVote -> Map RbHash PointState -> Bool
seenIn vote =
  maybe False (Set.member vote . psSeen) . Map.lookup vote.announcingRbHash

-- | Retain only the most recently first-seen 'maxAnnouncementCount' points,
-- evicting the oldest to make room.
--
-- FIXME: UNSAFE, and only here to bound memory for load testing. Recency is
-- /first-seen order/, not chain order, because a vote carries no slot: its
-- 'RbHash' cannot be placed in time without the announcing header, which the
-- vote state does not have. An adversary can therefore mint votes on fabricated
-- 'RbHash'es and walk every honest point out of the window, costing it nothing
-- and costing us every tally in progress.
--
-- The real fix is to stop the flood rather than to survive it, which wants a
-- slot in the vote: with one, a vote too far from the current tip can be
-- rejected before it occupies anything, and eviction can follow the chain
-- instead of arrival.
boundPoints ::
  RbHash ->
  (Map RbHash PointState, StrictSeq RbHash) ->
  (Map RbHash PointState, StrictSeq RbHash)
boundPoints rbHash (states, order)
  | Map.member rbHash states = (states, order)
  | otherwise = case Seq.lookup 0 order' of
      Just oldest
        | Seq.length order' > maxAnnouncementCount ->
            (Map.delete oldest states, Seq.drop 1 order')
      _ -> (states, order')
 where
  order' = order |> rbHash

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState ::
  MonadSTM m =>
  -- | Get the current 'LeiosCommittee' and threshold 'Weight'.
  STM m (Maybe (LeiosCommittee, Weight)) ->
  m (LeiosVoteState m)
newLeiosVoteState getCommittee = do
  votesChan <- atomically newBroadcastTChan
  pointStates <- atomically $ newTVar (Map.empty :: Map RbHash PointState)
  pointOrder <- atomically $ newTVar (Seq.empty :: StrictSeq RbHash)
  pure
    LeiosVoteState
      { addVote = \vote -> do
          -- Validate outside the transaction: the BLS pairing is ms-scale, and
          -- inside 'atomically' every conflicting commit re-ran it. Worst case
          -- now is one redundant verification per concurrently-received duplicate.
          alreadySeen <- atomically $ seenIn vote <$> readTVar pointStates
          if alreadySeen
            then pure AlreadyKnown
            else do
              -- TODO: disallow votes from different epoch (than the committee is).
              -- Could use slot numbers or put epoch into votes to distinguish?
              atomically getCommittee >>= \case
                Nothing -> pure NoCommittee
                Just (committee, threshold) ->
                  case validateLeiosVote committee vote of
                    Left reason -> pure $ VoteInvalid reason
                    Right weight -> atomically $ do
                      states0 <- readTVar pointStates
                      if seenIn vote states0
                        then pure AlreadyKnown
                        else do
                          writeTChan votesChan vote

                          -- FIXME: This code is not only ugly, but we need to also
                          -- keep track of which committee the cert is for. We shall
                          -- only use the cert (return on queryCert) if we are in
                          -- the same epoch as when it was aggregated / the
                          -- committee still the same.

                          -- Update the per-point tally, assembling (and
                          -- caching) the certificate the first time the
                          -- threshold is crossed.
                          -- Make room before inserting, so the window holds
                          -- this point rather than evicting it immediately.
                          order0 <- readTVar pointOrder
                          let (states, order) = boundPoints vote.announcingRbHash (states0, order0)
                              pst = Map.findWithDefault emptyPointState vote.announcingRbHash states
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
                              pst' =
                                pst
                                  { psSeen = Set.insert vote pst.psSeen
                                  , psVoters = voters'
                                  , psTotal = totalW
                                  }
                              pst'' = case pst.psCert of
                                Just _ -> pst'
                                Nothing
                                  | totalW >= threshold ->
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
                          writeTVar pointOrder $! order
                          pure $
                            Added
                              VoteTally
                                { vtWeight = weight
                                , vtTally = totalW
                                , vtThreshold = threshold
                                }
                              pst''.psCert
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
