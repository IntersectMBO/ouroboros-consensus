{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Ledger
  ( ByronSpecLedgerError (..)
  , initByronSpecLedgerState

    -- * Type family instances
  , LedgerState (..)
  , LedgerTables (..)
  , Ticked (..)
  ) where

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import Codec.Serialise
import Control.Monad.Except
import qualified Control.State.Transition as Spec
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
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
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data instance LedgerState ByronSpecBlock mk = ByronSpecLedgerState
  { byronSpecLedgerTip :: Maybe SlotNo
  -- ^ Tip of the ledger (most recently applied block, if any)
  --
  -- The spec state stores the last applied /hash/, but not the /slot/.
  , byronSpecLedgerState :: Spec.State Spec.CHAIN
  -- ^ The spec state proper
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Serialise
  deriving NoThunks via AllowThunk (LedgerState ByronSpecBlock mk)

newtype ByronSpecLedgerError = ByronSpecLedgerError
  { unByronSpecLedgerError :: NonEmpty (Spec.PredicateFailure Spec.CHAIN)
  }
  deriving (Show, Eq)
  deriving NoThunks via AllowThunk ByronSpecLedgerError

type instance LedgerCfg (LedgerState ByronSpecBlock) = ByronSpecGenesis

instance UpdateLedger ByronSpecBlock

initByronSpecLedgerState :: ByronSpecGenesis -> LedgerState ByronSpecBlock mk
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

instance GetTip (Ticked (LedgerState ByronSpecBlock)) where
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

data instance Ticked (LedgerState ByronSpecBlock) mk = TickedByronSpecLedgerState
  { untickedByronSpecLedgerTip :: Maybe SlotNo
  , tickedByronSpecLedgerState :: Spec.State Spec.CHAIN
  }
  deriving stock (Show, Eq)
  deriving NoThunks via AllowThunk (Ticked (LedgerState ByronSpecBlock) mk)

instance IsLedger (LedgerState ByronSpecBlock) where
  type LedgerErr (LedgerState ByronSpecBlock) = ByronSpecLedgerError

  type
    AuxLedgerEvent (LedgerState ByronSpecBlock) =
      VoidLedgerEvent (LedgerState ByronSpecBlock)

  applyChainTickLedgerResult _evs cfg slot (ByronSpecLedgerState tip state) =
    pureLedgerResult $
      TickedByronSpecLedgerState
        { untickedByronSpecLedgerTip = tip
        , tickedByronSpecLedgerState =
            Rules.applyChainTick
              cfg
              (toByronSpecSlotNo slot)
              state
        }

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

type instance TxIn (LedgerState ByronSpecBlock) = Void
type instance TxOut (LedgerState ByronSpecBlock) = Void
instance LedgerTablesAreTrivial (LedgerState ByronSpecBlock) where
  convertMapKind (ByronSpecLedgerState x y) =
    ByronSpecLedgerState x y
instance LedgerTablesAreTrivial (Ticked (LedgerState ByronSpecBlock)) where
  convertMapKind (TickedByronSpecLedgerState x y) =
    TickedByronSpecLedgerState x y
deriving via
  Void
  instance
    IndexedMemPack (LedgerState ByronSpecBlock EmptyMK) Void
deriving via
  TrivialLedgerTables (LedgerState ByronSpecBlock)
  instance
    HasLedgerTables (LedgerState ByronSpecBlock)
deriving via
  TrivialLedgerTables (Ticked (LedgerState ByronSpecBlock))
  instance
    HasLedgerTables (Ticked (LedgerState ByronSpecBlock))
deriving via
  TrivialLedgerTables (LedgerState ByronSpecBlock)
  instance
    CanStowLedgerTables (LedgerState ByronSpecBlock)

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ByronSpecBlock) ByronSpecBlock where
  applyBlockLedgerResultWithValidation _ _ cfg block (TickedByronSpecLedgerState _tip state) =
    withExcept ByronSpecLedgerError $
      fmap (pureLedgerResult . ByronSpecLedgerState (Just (blockSlot block))) $ -- Note that the CHAIN rule also applies the chain tick. So even
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

  getBlockKeySets _ = emptyLedgerTables

{-------------------------------------------------------------------------------
  CommonProtocolParams
-------------------------------------------------------------------------------}

instance CommonProtocolParams ByronSpecBlock where
  maxHeaderSize = fromIntegral . Spec._maxHdrSz . getPParams
  maxTxSize = fromIntegral . Spec._maxTxSz . getPParams

getPParams :: LedgerState ByronSpecBlock mk -> Spec.PParams
getPParams =
  Spec.protocolParameters
    . getChainStateUPIState
    . byronSpecLedgerState
