{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.HeaderValidation (
    ByronOtherHeaderEnvelopeError (..)
  , TipInfoIsEBB (..)
  ) where

import qualified Cardano.Chain.Slotting as CC
import           Control.Monad (when)
import           Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Byron.Ledger.PBFT ()
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Util (whenJust)

{-------------------------------------------------------------------------------
  Envelope
-------------------------------------------------------------------------------}

instance HasAnnTip ByronBlock where
  type TipInfo ByronBlock = TipInfoIsEBB ByronBlock
  tipInfoHash _ (TipInfoIsEBB h _) = h
  getTipInfo b = TipInfoIsEBB (blockHash b) (byronHeaderIsEBB b)

data ByronOtherHeaderEnvelopeError =
    UnexpectedEBBInSlot !SlotNo
  | InvalidCheckpoint -- TODO args
  deriving (Eq, Show, Generic, NoThunks)

instance BasicEnvelopeValidation ByronBlock where
  expectedFirstBlockNo  _ = BlockNo 0
  minimumPossibleSlotNo _ = SlotNo 0

  -- EBB shares its block number with its predecessor
  expectedNextBlockNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) b =
     case (prevIsEBB, curIsEBB) of
       (IsNotEBB, IsEBB) -> b
       _otherwise        -> succ b

  -- EBB shares its slot number with its successor
  minimumNextSlotNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) s =
      case (prevIsEBB, curIsEBB) of
        (IsEBB, IsNotEBB) -> s
        _otherwise        -> succ s

instance ValidateEnvelope ByronBlock where
  type OtherHeaderEnvelopeError ByronBlock = ByronOtherHeaderEnvelopeError

  additionalEnvelopeChecks cfg _ledgerView hdr = do
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ UnexpectedEBBInSlot actualSlotNo
      when (not (fromIsEBB newIsEBB)) $ -- TODO fine to ignore EBBs?
        whenJust (Map.lookup (blockNo hdr) checkpoints) $ \checkpoint ->
          when (checkpoint /= blockHash hdr) $
            throwError InvalidCheckpoint
    where
      actualSlotNo :: SlotNo
      actualSlotNo = blockSlot hdr

      newIsEBB :: IsEBB
      newIsEBB = byronHeaderIsEBB hdr

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots =
          CC.unEpochSlots
        . byronEpochSlots
        . configBlock
        $ cfg

      checkpoints =
        byronCheckpoints . configBlock $ cfg
