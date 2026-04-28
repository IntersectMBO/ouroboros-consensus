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
import LeiosDemoTypes (LeiosVote)

data LeiosVoteState m = LeiosVoteState
  { addVote :: LeiosVote -> m ()
  -- ^ Add a new vote to the LeiosVoteState. Adding the same vote multiple
  -- times will not result in multiple notifications to subscribers.
  , subscribeVotes :: m (LeiosVoteSubscription m)
  -- ^ Subscribe to new votes arriving in the LeiosVoteState. This will only
  -- serve new additions, starting from when this function was called.
  }

data LeiosVoteSubscription m = LeiosVoteSubscription {getNextVote :: STM m LeiosVote}

-- | Create a new empty 'LeiosVoteState'.
newLeiosVoteState :: MonadSTM m => m (LeiosVoteState m)
newLeiosVoteState = do
  votesChan <- atomically newBroadcastTChan
  seenVotes <- atomically $ newTVar Set.empty
  pure
    LeiosVoteState
      { addVote = \vote -> atomically $ do
          seen <- readTVar seenVotes
          if Set.member vote seen
            then pure ()
            else do
              writeTVar seenVotes $! Set.insert vote seen
              writeTChan votesChan vote
      , subscribeVotes = do
          chan <- atomically $ dupTChan votesChan
          pure $
            LeiosVoteSubscription
              { getNextVote = readTChan chan
              }
      }
