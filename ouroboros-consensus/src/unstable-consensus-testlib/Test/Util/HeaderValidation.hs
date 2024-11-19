{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Util.HeaderValidation (
    -- * Bogus time
    AddBogusTime
  , addBogusTime
  , addBogusTimeToFragment
  ) where

import           Cardano.Slotting.Time (RelativeTime (..))
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Bogus time
-------------------------------------------------------------------------------}

-- REVIEW: I'm not sure about the name of this type class, and how safe it is to use it in testing code. What if suddenly a test decides to rely on the header's time?
class AddBogusTime blk where
  addBogusTime ::
       Header blk
    -> HeaderWithTime blk

-- REVIEW: I even wonder if this instance should be defined here and
-- at all: there might be use cases in which the 'TestBlock's are used
-- in tests that depend on the header having a meaningful time
-- associated with them.
--
instance AddBogusTime blk where
  addBogusTime testHeader = HeaderWithTime {
      hwtHeader = testHeader
    , hwtSlotRelativeTime = RelativeTime (error "Header time should not be used!")
    }

addBogusTimeToFragment :: (AF.HasHeader (Header blk), Typeable blk)
  => AnchoredFragment (Header blk)
  -> AnchoredFragment (HeaderWithTime blk)
addBogusTimeToFragment = AF.mapAnchoredFragment addBogusTime
