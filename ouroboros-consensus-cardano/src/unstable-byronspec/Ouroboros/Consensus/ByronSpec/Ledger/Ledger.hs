{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Ledger
  ( ByronSpecLedgerError (..)
  , initByronSpecLedgerState

    -- * Type family instances
  , LedgerState (..)
  , Ticked (..)
  ) where

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import Codec.Serialise
import Control.Monad.Except
import qualified Control.State.Transition as Spec
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunk (..), NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.ByronSpec.Ledger.Accessors
import Ouroboros.Consensus.ByronSpec.Ledger.Block
import Ouroboros.Consensus.ByronSpec.Ledger.Conversions
import Ouroboros.Consensus.ByronSpec.Ledger.Genesis (ByronSpecGenesis)
import Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data instance LedgerState ByronSpecBlock = ByronSpecLedgerState
  { byronSpecLedgerTip :: Maybe SlotNo
  -- ^ Tip of the ledger (most recently applied block, if any)
  --
  -- The spec state stores the last applied /hash/, but not the /slot/.
  , byronSpecLedgerState :: Spec.State Spec.CHAIN
  -- ^ The spec state proper
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Serialise
  deriving NoThunks via AllowThunk (LedgerState ByronSpecBlock)

newtype ByronSpecLedgerError = ByronSpecLedgerError
  { unByronSpecLedgerError :: NonEmpty (Spec.PredicateFailure Spec.CHAIN)
  }
  deriving (Show, Eq)
  deriving NoThunks via AllowThunk ByronSpecLedgerError

type instance LedgerCfg LedgerState ByronSpecBlock = ByronSpecGenesis

instance UpdateLedger ByronSpecBlock

initByronSpecLedgerState :: ByronSpecGenesis -> LedgerState ByronSpecBlock
initByronSpecLedgerState cfg =
  ByronSpecLedgerState
    { byronSpecLedgerTip = Nothing
    , byronSpecLedgerState = Rules.initStateCHAIN cfg
    }

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ByronSpecBlock) where
  getTip (ByronSpecLedgerState tip state) =
    castPoint $
      getByronSpecTip tip state

instance GetTip (Ticked LedgerState ByronSpecBlock) where
  getTip (TickedByronSpecLedgerState tip state) =
    castPoint $
      getByronSpecTip tip state

getByronSpecTip :: Maybe SlotNo -> Spec.State Spec.CHAIN -> Point ByronSpecBlock
getByronSpecTip Nothing _ = GenesisPoint
getByronSpecTip (Just slot) state =
  BlockPoint
    slot
    (getChainStateHash state)

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

data instance Ticked LedgerState ByronSpecBlock = TickedByronSpecLedgerState
  { untickedByronSpecLedgerTip :: Maybe SlotNo
  , tickedByronSpecLedgerState :: Spec.State Spec.CHAIN
  }
  deriving stock (Show, Eq)
  deriving NoThunks via AllowThunk (Ticked LedgerState ByronSpecBlock)

type instance AuxLedgerEvent ByronSpecBlock = VoidLedgerEvent

instance IsLedger LedgerState ByronSpecBlock where
  type LedgerErr LedgerState ByronSpecBlock = ByronSpecLedgerError

  applyChainTickLedgerResult _evs cfg slot (ByronSpecLedgerState tip state) =
    pureLedgerResult
      ( TickedByronSpecLedgerState
          { untickedByronSpecLedgerTip = tip
          , tickedByronSpecLedgerState =
              Rules.applyChainTick
                cfg
                (toByronSpecSlotNo slot)
                state
          }
      , TickDiff UnitTables -- ByronSpec has no on-disk tables, so ticking produces no diff
      )

deriving newtype instance Semigroup (TxsDiff ByronSpecBlock)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

instance BlockSupportsLedgerHD ByronSpecBlock where
  type Keys ByronSpecBlock = UnitTables
  type Values ByronSpecBlock = UnitTables
  type Diff ByronSpecBlock = UnitTables
  blockKeys _ = UnitTables
  combineTickAndBlockDiff (TickDiff UnitTables) (BlockDiff UnitTables) = TickAndBlockDiff UnitTables
  forwardTickDiff (TickDiff UnitTables) UnitTables = UnitTables
  forwardBlockDiff (BlockDiff UnitTables) UnitTables = UnitTables
  forwardTickAndBlockDiff (TickAndBlockDiff UnitTables) UnitTables = UnitTables
  forwardTxsDiff (TxsDiff UnitTables) UnitTables = UnitTables
  restrictValues UnitTables UnitTables = UnitTables
  valuesSize UnitTables = 0
  encodeValuesForInMemory UnitTables = mempty
  decodeValuesForInMemory _ = pure UnitTables

instance SingleEraBlockSupportsLedgerHD ByronSpecBlock where
  type TxIn ByronSpecBlock = Void
  type TxOut ByronSpecBlock = Void
  rangeReadValues _ _ = (UnitTables, Nothing)
  keysToList _ = []
  valuesToList _ = []
  valuesFromList _ = UnitTables
  diffToList _ = []
  emptyValues = UnitTables
  emptyTickDiff = TickDiff UnitTables
  combineTransAndTickDiff (TickDiff UnitTables) (TickDiff UnitTables) = TickDiff UnitTables
  packTxInBytes = absurd
  unpackTxInBytes _ = Left "Absurd"

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ApplyBlock LedgerState ByronSpecBlock where
  applyBlockLedgerResultWithValidation _ _ cfg block (TickedByronSpecLedgerState _tip state) _values =
    withExcept ByronSpecLedgerError $
      fmap (pureLedgerResult . (,BlockDiff UnitTables) . ByronSpecLedgerState (Just (blockSlot block))) $ -- Note that the CHAIN rule also applies the chain tick. So even
      -- though the ledger we received has already been ticked with
      -- 'applyChainTick', we do it again as part of CHAIN. This is safe, as
      -- it is idempotent. If we wanted to avoid the repeated tick, we would
      -- have to call the subtransitions of CHAIN (except for ticking).
        Rules.liftCHAIN
          cfg
          (byronSpecBlock block)
          state

  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult (error . ("reapplyBlockLedgerResult: unexpected error " ++) . show)

{-------------------------------------------------------------------------------
  CommonProtocolParams
-------------------------------------------------------------------------------}

instance CommonProtocolParams ByronSpecBlock where
  maxHeaderSize = fromIntegral . Spec._maxHdrSz . getPParams
  maxTxSize = fromIntegral . Spec._maxTxSz . getPParams

getPParams :: LedgerState ByronSpecBlock -> Spec.PParams
getPParams =
  Spec.protocolParameters
    . getChainStateUPIState
    . byronSpecLedgerState
