-- | Where the node emits each endorser block and closure it acquires.
--
-- The node emits them once each write is durable; see
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

-- | Something the node acquired at a 'LeiosPoint'.
--
-- The node emits each at most once per point: 'AcquiredEb' once the point's
-- body is durable, and 'AcquiredEbTxs' once its closure is. It emits neither
-- for a point whose closure the LeiosDB already held when the node started. So
-- a subscriber that subscribes before it scans the LeiosDB at start-up, as the
-- ChainDB does, hears of each point once at most, and only of one its scan did
-- not find.
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
  { emitLeiosAcquisition :: LeiosEbNotification -> STM m ()
  , subscribeLeiosAcquisitions :: m (STM m LeiosEbNotification)
  -- ^ Each acquisition emitted from now on, once and in order, blocking
  -- until there is one.
  }

-- | A new broadcast, with no subscriber yet.
--
-- Its channel is unbounded: each subscriber holds every acquisition emitted
-- since it subscribed that it has yet to read. That stays small, as long as
-- each subscriber reads its next acquisition as soon as it has handled the
-- previous one, and handles one without waiting long; the ChainDB's waits at
-- most on ChainSel's queue, which ChainSel drains. The node emits at most two
-- acquisitions per point, and acquires only the points it focuses its
-- elections on: the first announcement it accepts in each, or a certified one.
-- So acquisitions arrive at the rate of elections, whatever its peers do.
newLeiosAcquisitions :: MonadSTM m => m (LeiosAcquisitions m)
newLeiosAcquisitions = do
  chan <- atomically newBroadcastTChan
  pure
    MkLeiosAcquisitions
      { emitLeiosAcquisition = writeTChan chan
      , subscribeLeiosAcquisitions = readTChan <$> atomically (dupTChan chan)
      }

-- | For what opens a ChainDB but fetches nothing, such as a tool replaying its
-- own databases: nothing is ever emitted.
noLeiosAcquisitions :: MonadSTM m => LeiosAcquisitions m
noLeiosAcquisitions =
  MkLeiosAcquisitions
    { emitLeiosAcquisition = \_ -> pure ()
    , subscribeLeiosAcquisitions = pure retry
    }
