{-# LANGUAGE FlexibleContexts #-}

module Test.Util.Header (
    -- * Enriching headers with a relative slot time
    attachSlotTime
  , attachSlotTimeToFragment
  , dropTimeFromFragment
  ) where

import           Cardano.Slotting.EpochInfo.API (epochInfoSlotToRelativeTime)
import           Data.Functor.Identity (runIdentity)
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block (Header, blockSlot)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
                     (ImmutableEraParams, immutableEpochInfo)
import           Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Enriching headers with a relative slot time
-------------------------------------------------------------------------------}

dropTimeFromFragment :: (AF.HasHeader (Header blk))
  => AnchoredFragment (HeaderWithTime blk)
  -> AnchoredFragment (Header blk)
dropTimeFromFragment  = AF.mapAnchoredFragment hwtHeader

attachSlotTimeToFragment ::
     ( AF.HasHeader (Header blk)
     , Typeable blk
     , ImmutableEraParams blk)
  => TopLevelConfig blk
  -> AnchoredFragment (Header blk)
  -> AnchoredFragment (HeaderWithTime blk)
attachSlotTimeToFragment cfg = AF.mapAnchoredFragment (attachSlotTime cfg)

attachSlotTime ::
     (AF.HasHeader (Header blk), ImmutableEraParams blk)
  => TopLevelConfig blk
  -> Header blk
  -> HeaderWithTime blk
attachSlotTime cfg hdr = HeaderWithTime {
      hwtHeader           = hdr
    , hwtSlotRelativeTime =
        runIdentity $ epochInfoSlotToRelativeTime ei (blockSlot hdr)
    }
  where
    ei = immutableEpochInfo cfg
