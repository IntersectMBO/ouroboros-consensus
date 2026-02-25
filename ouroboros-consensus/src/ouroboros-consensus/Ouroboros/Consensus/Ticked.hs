{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Ticked (Ticked (..)) where

import Data.Kind (Type)
import Data.SOP.BasicFunctors
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract

{-------------------------------------------------------------------------------
  Ticked state
-------------------------------------------------------------------------------}

-- | " Ticked " piece of state, either 'LedgerState' or 'ChainDepState'
--
-- Ticking refers to the passage of time (the ticking of the clock). When a
-- piece of state is marked as ticked, it means that time-related changes have
-- been applied to the state. There are exactly two methods in the interface
-- that do that: 'Ouroboros.Consensus.Protocol.Abstract.tickChainDepState' and
-- 'Ouroboros.Consensus.Ledger.Basics.applyChainTickLedgerResult'.
--
-- Also note that a successful forecast
-- @'Ouroboros.Consensus.Forecast.forecastFor'
-- ('Ouroboros.Consensus.Ledger.SupportsProtocol.ledgerViewForecastAt' cfg st)
-- slot@ must equal
-- @'Ouroboros.Consensus.Ledger.SupportsProtocol.protocolLedgerView' cfg
-- ('Ouroboros.Consensus.Ledger.Basics.applyChainTick' cfg slot st)@. Thus a
-- 'Ouroboros.Consensus.Protocol.Abstract.LedgerView' can only be projected
-- from a 'Ticked' state, but cannot itself be ticked.
--
-- Some examples of time related changes:
--
-- * Scheduled delegations might have been applied in Byron
-- * New leader schedule computed for Shelley
-- * Transition from Byron to Shelley activated in the hard fork combinator.
-- * Nonces switched out at the start of a new epoch.
type Ticked :: k -> k
data family Ticked st

-- Standard instance for use with trivial state

data instance Ticked () = TickedTrivial
  deriving Show

type instance HeaderHash (Ticked l) = HeaderHash l

{-------------------------------------------------------------------------------
  Forwarding type class instances
-------------------------------------------------------------------------------}

deriving newtype instance
  {-# OVERLAPPING #-}
  Show (Ticked (f a)) =>
  Show ((Ticked :.: f) (a :: Type))

deriving newtype instance
  NoThunks (Ticked (f a)) =>
  NoThunks ((Ticked :.: f) a)
