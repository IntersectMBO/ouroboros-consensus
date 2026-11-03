-- | Where the node announces each endorser block and closure it acquires.
--
-- LeiosFetch announces them, once each write is durable; see
-- 'LeiosEbNotification' for at which points. The ChainDB subscribes, so
-- that ChainSel can select a block certifying an endorser block once the node
-- holds that endorser block's closure.
module Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosEbNotification (..)
  , LeiosAcquisitions (..)
  , newLeiosAcquisitions
  , noLeiosAcquisitions
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , STM
  , atomically
  , dupTChan
  , newBroadcastTChan
  , readTChan
  , retry
  , writeTChan
  )
import Ouroboros.Consensus.Leios.Types (BytesSize, LeiosPoint)

-- | Something LeiosFetch acquired at a 'LeiosPoint'.
--
-- LeiosFetch sends each at most once per point: 'AcquiredEb' once the point's
-- body is durable, and 'AcquiredEbTxs' once its closure is. It sends neither
-- for a point whose closure the LeiosDB already held when the node started.
data LeiosEbNotification
  = -- | The node holds the EB's body.
    AcquiredEb
      -- | the EB's point
      LeiosPoint
      -- | the size of its body
      BytesSize
  | -- | The node holds the EB's body and every tx it lists.
    AcquiredEbTxs
      -- | the EB's point
      LeiosPoint

-- | A broadcast of the node's acquisitions.
data LeiosAcquisitions m = MkLeiosAcquisitions
  { announceLeiosAcquisition :: LeiosEbNotification -> STM m ()
  , subscribeLeiosAcquisitions :: m (STM m LeiosEbNotification)
  -- ^ Each acquisition announced from now on, once and in order, blocking
  -- until there is one.
  }

newLeiosAcquisitions :: MonadSTM m => m (LeiosAcquisitions m)
newLeiosAcquisitions = do
  chan <- atomically newBroadcastTChan
  pure
    MkLeiosAcquisitions
      { announceLeiosAcquisition = writeTChan chan
      , subscribeLeiosAcquisitions = readTChan <$> atomically (dupTChan chan)
      }

-- | For a node that fetches nothing, such as a tool replaying its own
-- databases: nothing is ever announced.
noLeiosAcquisitions :: MonadSTM m => LeiosAcquisitions m
noLeiosAcquisitions =
  MkLeiosAcquisitions
    { announceLeiosAcquisition = \_ -> pure ()
    , subscribeLeiosAcquisitions = pure retry
    }
