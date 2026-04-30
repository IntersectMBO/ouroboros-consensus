{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Tables.Basics
  ( -- * Kinds

    -- | For convenience' sake, we define these kinds which convey the intended
    -- instantiation for the type variables.
    LedgerStateKind
  , MapKind
  , StateKind

    -- * Ledger tables
  , LedgerTables (..)
  , TxIn
  , TxOut
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.Tables.Kinds

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | The Ledger Tables represent the portion of the data on disk that has been
-- pulled from disk and attached to the in-memory Ledger State or that will
-- eventually be written to disk.
--
-- With UTxO-HD and the split of the Ledger /ledger state/ into the in-memory
-- part and the on-disk part, this splitting was reflected in the new type
-- parameter added to the (Consensus)
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState', to which we refer as "the
-- MapKind" or @mk@.
--
-- Every 'Ouroboros.Consensus.Ledger.Basics.LedgerState' (or @LedgerState@-like
-- type, such as the 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState') is
-- associated with a 'LedgerTables' and they both share the @mk@. They both are
-- of kind 'LedgerStateKind'. 'LedgerTables' is just a way to refer /only/ to a
-- partial view of the on-disk data without having the rest of the in-memory
-- 'LedgerState' in scope.
--
-- The @mk@ can be instantiated to anything that is map-like, i.e. that expects
-- two type parameters, the key and the value.
type LedgerTables :: Type -> MapKind -> Type
newtype LedgerTables blk mk = LedgerTables
  { getLedgerTables :: mk (TxIn blk) (TxOut blk)
  }
  deriving stock Generic

deriving stock instance
  Show (mk (TxIn blk) (TxOut blk)) =>
  Show (LedgerTables blk mk)
deriving stock instance
  Eq (mk (TxIn blk) (TxOut blk)) =>
  Eq (LedgerTables blk mk)
deriving newtype instance
  NoThunks (mk (TxIn blk) (TxOut blk)) =>
  NoThunks (LedgerTables blk mk)

-- | Each @LedgerState@ instance will have the notion of a @TxIn@ for the tables.
--
-- This will change once there is more than one table.
type TxIn :: Type -> Type
type family TxIn blk

-- | Each @LedgerState@ instance will have the notion of a @TxOut@ for the
-- tables.
--
-- This will change once there is more than one table.
type TxOut :: Type -> Type
type family TxOut blk
