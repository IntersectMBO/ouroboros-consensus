module LeiosVoteState (module LeiosVoteState) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , STM
  , atomically
  , dupTChan
  , newBroadcastTChan
  , readTChan
  , writeTChan
  )
import LeiosDemoTypes (LeiosVote)

data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m ()
  -- ^ Add a new vote to the LeiosVoteState.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  }

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState :: MonadSTM m => m (LeiosVoteState m)
newLeiosVoteState = do
  votesChan <- atomically newBroadcastTChan
  pure
    LeiosVoteState
      { addVote = atomically . writeTChan votesChan
      , subscribeVotes = do
          chan <- atomically $ dupTChan votesChan
          pure $
            LeiosVoteSubscription
              { getNextVote = readTChan chan
              }
      }
