{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LeiosVoteState (module LeiosVoteState) where

import Cardano.Crypto.DSIGN (DSIGNAggregatable (aggregateSigsDSIGN))
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
  ( Committee (..)
  , EbHash (..)
  , LeiosPoint (..)
  , LeiosSignature
  , LeiosVote (..)
  , VoteInvalid (..)
  , VoterId (..)
  , Weight
  , encodeSignersBitfield
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
                                  pst'{psCert = assembleCert committee pt pst'}
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

-- | Build a 'Leios.LeiosCert' from a point's collected votes against
-- the given committee. Returns 'Nothing' if BLS aggregation of the
-- individual signatures fails (shouldn't happen with well-formed
-- BLS sigs).
assembleCert :: Committee -> LeiosPoint -> PointState -> Maybe Leios.LeiosCert
assembleCert committee pt pst =
  case aggregateSigsDSIGN sigs of
    Left _ -> Nothing
    Right aggSig ->
      Just
        Leios.LeiosCert
          { Leios.slotNo = pt.pointSlotNo
          , Leios.endorserBlockHash = toLeiosEbHash pt.pointEbHash
          , Leios.signers = encodeSignersBitfield committeeSize voterIdxs
          , Leios.aggregatedSignature = aggSig
          }
 where
  committeeSize = length committee.voters

  votersList = Map.toAscList pst.psVoters
  voterIdxs = [fromIntegral (voterIndex vid) | (vid, _) <- votersList]
  sigs = [s | (_, (_, s)) <- votersList]

  -- Convert consensus's local 'EbHash' to 'cardano-crypto-leios'-flavoured 'Leios.EbHash'.
  toLeiosEbHash (MkEbHash bs) = Leios.MkEbHash bs
