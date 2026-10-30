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

-- | An acquisition the node announces.
--
-- LeiosFetch sends each notification when it acquires the EB, at the youngest
-- 'LeiosPoint' at which an election focused the EB. It sends it again whenever
-- the EB takes an election's focus afterwards, unless that election's slot is
-- older than the ImmutableDB tip's: when a later election's focus is an
-- announcement of the EB, or when a verified certificate moves an election's
-- focus to the EB. An election's focus is the first announcement in it that the
-- node accepts, until a verified certificate names another, so every
-- 'LeiosPoint' that a block certifies is a focus. So a subscriber hears of the
-- EB's youngest focus.
--
-- 'AcquiredEb' carries the body's size, which LeiosFetch does not know for an
-- EB the LeiosDB already held when the node started, so for such an EB it sends
-- only 'AcquiredEbTxs'.
data LeiosEbNotification
  = -- | The node holds the EB's body.
    AcquiredEb
      -- | the EB's point
      LeiosPoint
      -- | its size in bytes
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
