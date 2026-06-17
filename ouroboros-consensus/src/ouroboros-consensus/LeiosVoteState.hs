{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LeiosVoteState (module LeiosVoteState) where

import qualified Cardano.Crypto.Leios as Leios
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
  ( Committee
  , LeiosPoint (..)
  , LeiosSignature
  , LeiosVote (..)
  , VoteInvalid (..)
  , VoterId
  , Weight
  , minCertificationThreshold
  , validateLeiosVote
  )

data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m AddVoteResult
  -- ^ Add a new vote to the LeiosVoteState. Adding the same vote multiple
  -- times will not result in multiple notifications to subscribers.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  , queryCert :: LeiosPoint -> m (Maybe Leios.LeiosCert)
  -- ^ Look up the assembled certificate for a 'LeiosPoint', or
  -- 'Nothing' if its collected votes haven't crossed
  -- 'minCertificationThreshold'.
  }

data AddVoteResult
  = NoCommittee
  | VoteInvalid VoteInvalid
  | AlreadyKnown
  | -- | The vote was added to the state, with the running per-point weight
    -- after this addition. The LeiosCert is 'Just' whenever the tally for the
    -- vote's point is at or above 'minCertificationThreshold'.
    Added Weight (Maybe Leios.LeiosCert)
  deriving (Eq, Show)

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Per-'LeiosPoint' tally we maintain inside 'newLeiosVoteState'.
-- Holds the contributing voters plus a memoised certificate once the
-- threshold is crossed.
data PointState = PointState
  { psVoters :: !(Map VoterId (Weight, LeiosSignature))
  , psCert :: !(Maybe Leios.LeiosCert)
  -- ^ Assembled once when this point's total weight first reaches
  -- 'minCertificationThreshold'; reused for subsequent post-threshold
  -- votes so we don't keep rerunning BLS aggregation.
  }

emptyPointState :: PointState
emptyPointState = PointState Map.empty Nothing

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState ::
  MonadSTM m =>
  -- | Get the current 'Committee'.
  STM m (Maybe Committee) ->
  m (LeiosVoteState m)
newLeiosVoteState getCommittee = do
  votesChan <- atomically newBroadcastTChan
  seenVotes <- atomically $ newTVar Set.empty
  pointStates <- atomically $ newTVar (Map.empty :: Map LeiosPoint PointState)
  pure
    LeiosVoteState
      { addVote = \vote -> atomically $ do
          seen <- readTVar seenVotes
          if Set.member vote seen
            then pure AlreadyKnown
            else do
              -- TODO: disallow votes from different epoch (than the committee is).
              -- Could use slot numbers or put epoch into votes to distinguish?
              getCommittee >>= \case
                Nothing -> pure NoCommittee
                Just committee ->
                  case validateLeiosVote committee vote of
                    Left reason -> pure $ VoteInvalid reason
                    Right weight -> do
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
                      let pt = vote.point
                      states <- readTVar pointStates
                      let pst = Map.findWithDefault emptyPointState pt states
                          pst' =
                            pst
                              { psVoters =
                                  Map.insert vote.voterId (weight, vote.voteSignature) pst.psVoters
                              }
                          totalW = sum [w | (w, _) <- Map.elems pst'.psVoters]
                          pst'' = case pst.psCert of
                            Just _ -> pst'
                            Nothing
                              | totalW >= minCertificationThreshold ->
                                  -- Voters were validated against this committee before
                                  -- being added and the per-voter signatures already
                                  -- passed individual verification, so aggregation must
                                  -- succeed. TODO: replace 'error' with a tracer.
                                  case Leios.aggregateLeiosCert committee (fmap snd pst'.psVoters) of
                                    Left e ->
                                      error $
                                        "LeiosVoteState.addVote: aggregateLeiosCert "
                                          <> "failed on validated votes; should not happen: "
                                          <> show e
                                    Right cert -> pst'{psCert = Just cert}
                              | otherwise -> pst'
                      writeTVar pointStates $! Map.insert pt pst'' states
                      pure $ Added weight pst''.psCert
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
