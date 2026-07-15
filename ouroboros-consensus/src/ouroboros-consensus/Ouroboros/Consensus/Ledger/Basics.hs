{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract".
module Ouroboros.Consensus.Ledger.Basics
  ( -- * The 'LedgerState' definition
    LedgerCfg
  , LedgerState
  , TickedLedgerState

    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult

    -- * GetTip
  , GetTip (..)
  , GetTipSTM (..)
  , getTipHash
  , getTipM
  , getTipSlot

    -- * Associated types by block type
  , AuxLedgerEvent
  , LedgerConfig
  ) where

import Data.Kind (Constraint, Type)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

type GetTip :: Type -> Constraint
class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'GenesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

type GetTipSTM :: (Type -> Type) -> Type -> Constraint
class GetTipSTM m l where
  getTipSTM :: l -> STM m (Point l)

getTipM :: (GetTipSTM m l, MonadSTM m) => l -> m (Point l)
getTipM = atomically . getTipSTM

{-------------------------------------------------------------------------------
  Events directly from the ledger
-------------------------------------------------------------------------------}

-- | A 'Data.Void.Void' isomorph for explicitly declaring that some ledger has
-- no events
type VoidLedgerEvent :: Type
data VoidLedgerEvent

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
type LedgerResult :: Type -> Type -> Type
data LedgerResult blk a = LedgerResult
  { lrEvents :: [AuxLedgerEvent blk]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

castLedgerResult ::
  AuxLedgerEvent l ~ AuxLedgerEvent l' =>
  LedgerResult l a ->
  LedgerResult l' a
castLedgerResult (LedgerResult x0 x1) = LedgerResult x0 x1

embedLedgerResult ::
  (AuxLedgerEvent l -> AuxLedgerEvent l') ->
  LedgerResult l a ->
  LedgerResult l' a
embedLedgerResult inj lr = lr{lrEvents = inj `map` lrEvents lr}

pureLedgerResult :: a -> LedgerResult l a
pureLedgerResult a =
  LedgerResult
    { lrEvents = mempty
    , lrResult = a
    }

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
--
-- Types that inhabit this family will come from the Ledger code.
type LedgerCfg :: (Type -> Type) -> Type -> Type
type family LedgerCfg l blk :: Type

-- | Event emitted by the ledger
--
-- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
-- 'InspectLedger'. When that module is rewritten to make use of ledger
-- derived events, we may rename this type.
type AuxLedgerEvent :: Type -> Type
type family AuxLedgerEvent blk :: Type

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
--
-- This is the Consensus notion of a Ledger /ledger state/. Each block type is
-- associated with one of the Ledger types for the /ledger state/. Virtually
-- every concept in this codebase revolves around this type, or the referenced
-- @blk@. Whenever we use the type variable @l@ we intend to signal that the
-- expected instantiation is either a 'LedgerState' or some wrapper over it
-- (like the 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState').
--
-- The main operations we can do with a 'LedgerState' are /ticking/ (defined in
-- 'IsLedger'), and /applying a block/ (defined in
-- 'Ouroboros.Consensus.Ledger.Abstract.ApplyBlock').
type LedgerState :: Type -> Type
data family LedgerState blk

type TickedLedgerState blk = Ticked LedgerState blk

type instance HeaderHash (LedgerState blk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)

type LedgerConfig blk = LedgerCfg LedgerState blk
