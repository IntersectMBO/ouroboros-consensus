{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Set as Set
import LeiosDemoTypes (Committee, LeiosVote, VoteInvalid (..), validateLeiosVote)

data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m AddVoteResult
  -- ^ Add a new vote to the LeiosVoteState. Adding the same vote multiple
  -- times will not result in multiple notifications to subscribers.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  }

data AddVoteResult
  = NoCommittee
  | VoteInvalid VoteInvalid
  | AlreadyKnown
  | Added
  deriving (Eq, Show)

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState ::
  MonadSTM m =>
  -- | Get the current 'Committee'.
  STM m (Maybe Committee) ->
  m (LeiosVoteState m)
newLeiosVoteState getCommittee = do
  votesChan <- atomically newBroadcastTChan
  seenVotes <- atomically $ newTVar Set.empty
  pure
    LeiosVoteState
      { addVote = \vote -> atomically $ do
          -- TODO: disallow votes from different epoch (than the committee is).
          -- Could use slot numbers or put epoch into votes to distinguish?
          getCommittee >>= \case
            Nothing -> pure NoCommittee
            Just committee ->
              case validateLeiosVote committee vote of
                Left reason -> pure $ VoteInvalid reason
                Right _weight -> do
                  seen <- readTVar seenVotes
                  if Set.member vote seen
                    then pure AlreadyKnown
                    else do
                      writeTVar seenVotes $! Set.insert vote seen
                      writeTChan votesChan vote
                      pure Added
      , subscribeVotes = do
          chan <- atomically $ dupTChan votesChan
          pure $
            LeiosVoteSubscription
              { getNextVote = readTChan chan
              }
      }
