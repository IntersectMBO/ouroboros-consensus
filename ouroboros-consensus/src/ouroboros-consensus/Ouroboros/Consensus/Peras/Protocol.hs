{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Consensus.Peras.Protocol
  ( PerasLedgerView (..)
  , PerasProtocol (..)
  , defaultGetPerasLedgerView
  ) where

import Cardano.Ledger.BaseTypes (PositiveInterval, UnitInterval)
import Cardano.Slotting.Slot (SlotInterval)
import Data.Maybe.Strict (StrictMaybe)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (..))

-- | The Peras parameters a ledger view carries.
data PerasLedgerView = MkPerasLedgerView
  { plvMinCandidateBlockAge :: !SlotInterval
  , plvHealingFactor :: !PositiveInterval
  , plvCertBoost :: !Word16
  , plvTargetCommitteeSize :: !Word16
  , plvBootstrapRound :: !(StrictMaybe Word32)
  , plvQuorumThresholdSafetyMargin :: !UnitInterval
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

class ConsensusProtocol p => PerasProtocol p where
  -- | The Peras parameters this ledger view carries, or 'Nothing' if the
  -- protocol has no Peras.
  getPerasLedgerView :: proxy p -> LedgerView p -> Maybe PerasLedgerView

-- | 'getPerasLedgerView' for a protocol that has no Peras.
defaultGetPerasLedgerView :: proxy p -> LedgerView p -> Maybe PerasLedgerView
defaultGetPerasLedgerView _proxy _view = Nothing
