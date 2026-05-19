{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( -- * System time
    SystemStart (..)

    -- * Relative time
  , RelativeTime (..)
  , addRelTime
  , diffRelTime
  , fromRelativeTime
  , toRelativeTime

    -- * Get current time (as 'RelativeTime')
  , SystemTime (..)

    -- * Attach an arrival time (as 'RelativeTime') to an object
  , WithArrivalTime (..)
  , addArrivalTime

    -- * Slot length
  , getSlotLength
  , mkSlotLength

    -- ** Conversions
  , slotLengthFromMillisec
  , slotLengthFromSec
  , slotLengthToMillisec
  , slotLengthToSec

    -- ** opaque
  , SlotLength
  ) where

import Cardano.Slotting.Time
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

addRelTime :: NominalDiffTime -> RelativeTime -> RelativeTime
addRelTime = addRelativeTime

diffRelTime :: RelativeTime -> RelativeTime -> NominalDiffTime
diffRelTime = diffRelativeTime

{-------------------------------------------------------------------------------
  Get current time (as RelativeTime)
-------------------------------------------------------------------------------}

-- | System time
--
-- Slots are counted from the system start.
data SystemTime m = SystemTime
  { systemTimeCurrent :: m RelativeTime
  -- ^ Get current time (as a 'RelativeTime')
  --
  -- For real deployment, this will take the current 'UTCTime' and then
  -- subtract the 'SystemStart' (see 'defaultSystemTime'). Tests don't
  -- bother with a 'UTCTime' and just work entirely in 'RelativeTime'.
  , systemTimeWait :: m ()
  -- ^ Wait for 'SystemStart'
  --
  -- For the real deployment, this waits for the current 'UTCTime'
  -- to reach 'SystemStart'. In tests this does nothing.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "SystemTime" (SystemTime m)

{-------------------------------------------------------------------------------
  Attach an arrival time (as RelativeTime) to an object
-------------------------------------------------------------------------------}

-- | WithArrivalTime
data WithArrivalTime a = WithArrivalTime
  { getArrivalTime :: !RelativeTime
  -- ^ The time at which the object arrived
  , forgetArrivalTime :: !a
  -- ^ The object without its arrival time
  }
  deriving (Show, Eq, Ord, Generic, NoThunks)

-- | Add an arrival time to an object
addArrivalTime :: Monad m => SystemTime m -> a -> m (WithArrivalTime a)
addArrivalTime systemTime a = do
  t <- systemTimeCurrent systemTime
  return (WithArrivalTime t a)
