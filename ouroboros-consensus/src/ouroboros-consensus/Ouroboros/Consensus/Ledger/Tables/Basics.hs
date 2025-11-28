{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Ledger.Tables.Basics
  ( -- * Kinds
    LedgerStateKind
  , StateKind
  , F2

    -- * Ledger tables
  , LedgerTables (..)
  , Table (..)

    -- * Known tables
  , TABLE (..)
  , TablesForBlock

    -- ** UTxO table
  , TxIn
  , TxOut

    -- ** Instant stake table
  , Coin
  , Credential

    -- * Helpers
  , MemPackIdx
  , setterForSing
  , KVConstraintsMK
  , LedgerTableConstraintsMK

    -- * Casting

  -- , SameTablesTypes
  -- , SameTypesAsMyself
  -- , CanCastLedgerTables (..)
  -- , CanCastTables (..)
  , Key
  , Value
  -- , WrapKey (..)
  -- , WrapValue (..)
  -- , AllKeys
  -- , AllValues
  , AllTables
  , getTableByTag
  -- , defaultCastLedgerTables
  -- , defaultCastTable
  ) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.TxIn
import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.SOP.Constraint
import Data.SOP.Index (Index (..), projectNP)
import Data.SOP.Strict
import Data.Singletons
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Ledger.LedgerStateType
import Ouroboros.Consensus.Util.IndexedMemPack

--------------------------------------------------------------------------------
-- Keys and values
--------------------------------------------------------------------------------

-- | The possible tables that Consensus is aware of.
type data TABLE = UTxOTable | InstantStakeTable

type Key :: TABLE -> Type
type family Key table where
  Key UTxOTable = TxIn
  Key InstantStakeTable = Credential 'Staking

------------------------------------------------------------
-- START Singletons
------------------------------------------------------------

data STABLE a where
  SUTxOTable :: STABLE UTxOTable
  SInstantStakeTable :: STABLE InstantStakeTable

type instance Sing = STABLE

instance SingI UTxOTable where
  sing = SUTxOTable
instance SingI InstantStakeTable where
  sing = SInstantStakeTable

instance TestEquality STABLE where
  testEquality SUTxOTable SUTxOTable = Just Refl
  testEquality SUTxOTable _ = Nothing
  testEquality SInstantStakeTable SInstantStakeTable = Just Refl
  testEquality SInstantStakeTable _ = Nothing

------------------------------------------------------------
-- END Singletons
------------------------------------------------------------

-- | The table keys and values for each table.
type Value :: TABLE -> Type -> Type
type family Value table blk where
  Value UTxOTable blk = TxOut blk
  Value InstantStakeTable blk = CompactForm Coin

-- class All (Compose c (WrapKey blk)) (TablesForBlock blk) => AllKeys c blk
-- instance All (Compose c (WrapKey blk)) (TablesForBlock blk) => AllKeys c blk
-- class All (Compose c (WrapValue blk)) (TablesForBlock blk) => AllValues c blk
-- instance All (Compose c (WrapValue blk)) (TablesForBlock blk) => AllValues c blk

-- newtype WrapKey blk table = WrapKey (Key blk table)

-- deriving instance NoThunks (Fst (TableKV table blk)) => NoThunks (WrapKey blk table)
-- deriving instance Show (Fst (TableKV table blk)) => Show (WrapKey blk table)
-- deriving instance Eq (Fst (TableKV table blk)) => Eq (WrapKey blk table)

-- newtype WrapValue blk table = WrapValue (Value blk table)

-- deriving instance NoThunks (Snd (TableKV table blk)) => NoThunks (WrapValue blk table)
-- deriving instance Show (Snd (TableKV table blk)) => Show (WrapValue blk table)
-- deriving instance Eq (Snd (TableKV table blk)) => Eq (WrapValue blk table)

type TxOut :: Type -> Type
type family TxOut blk

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | A table value
type Table :: F2 -> Type -> TABLE -> Type
newtype Table mk blk table = Table {getTable :: mk (Key table) (Value table blk)}
  deriving Generic

deriving instance NoThunks (mk (Key table) (Value table blk)) => NoThunks (Table mk blk table)
deriving instance Show (mk (Key table) (Value table blk)) => Show (Table mk blk table)
deriving instance Eq (mk (Key table) (Value table blk)) => Eq (Table mk blk table)

class All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l
instance All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l

-- | Each block will declare its tables.
--
-- TablesForBlock (LedgerState Byron) = '[]
--
-- TablesForBlock (LedgerState Shelley) = '[UTxOTable]
-- TablesForBlock (LedgerState Allegra) = '[UTxOTable]
-- TablesForBlock (LedgerState Mary)    = '[UTxOTable]
--
-- TablesForBlock (LedgerState Conway) = '[UTxOTable, InstantStakeTable]
type TablesForBlock :: Type -> [TABLE]
type family TablesForBlock blk

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
type LedgerTables :: StateKind
newtype LedgerTables blk mk = LedgerTables
  { getLedgerTables :: NP (Table mk blk) (TablesForBlock blk)
  }
  deriving Generic

{-
Problem with design 1 is that we could do:

type instance TablesForBlock ByronBlock = '[UTxOTable]
type instance TxIn ByronBlock = Void
-}

-- data LedgerTables' l mk = LedgerTables'
--   { getUTxOTable :: mk (TblKey UTxO blk) (TxOut l)  -- If TxIn l ~ Void => this can only be EmptyMK
--   , getInstantStakeTable :: mk (TblKey InstantStake l) (Coin l)
--   }
--   deriving Generic

deriving newtype instance
  AllTables NoThunks mk blk =>
  NoThunks (LedgerTables blk mk)
deriving newtype instance
  AllTables Show mk blk =>
  Show (LedgerTables blk mk)
deriving newtype instance
  AllTables Eq mk blk =>
  Eq (LedgerTables blk mk)

------------------------------------------------------------
-- Type proof of inclusion
------------------------------------------------------------

findIndex ::
  forall (xs :: [TABLE]) (x :: TABLE).
  SList xs ->
  Sing x ->
  Maybe (Index xs x)
findIndex SNil _ = Nothing
findIndex (SCons sy sxs) sx =
  case testEquality sx sy of
    Just Refl -> Just IZ
    Nothing -> IS <$> findIndex sxs sx

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

-- | Given a proof of inclusion, create a lens for the particular item in the NP
ixNP :: Index xs x -> Lens' (NP f xs) (f x)
ixNP IZ f (x :* xs) = (:* xs) <$> f x
ixNP (IS m) f (x :* xs) = (x :*) <$> ixNP m f xs

setterForSing ::
  forall l mk table.
  SList (TablesForBlock l) ->
  Sing table ->
  Maybe (ASetter' (LedgerTables l mk) (Table mk l table))
setterForSing sxs sx =
  (\mbm -> \f -> fmap LedgerTables . ixNP mbm f . getLedgerTables) <$> findIndex sxs sx

getTableByTag ::
  forall tag mk l.
  ( SListI (TablesForBlock l)
  , SingI (TablesForBlock l)
  ) =>
  Sing tag ->
  LedgerTables l mk ->
  Maybe (Table mk l tag)
getTableByTag stag (LedgerTables np) =
  let mem = findIndex (sing @(TablesForBlock l)) stag
   in fmap (flip projectNP np) mem

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Auxiliary information for @IndexedMemPack@. @mk@ is there only because the
-- last instance needs to be a full type.
type MemPackIdx :: StateKind
type family MemPackIdx l mk where
  MemPackIdx blk mk = LedgerState blk mk

--------------------------------------------------------------------------------
-- Auxiliary classes
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Single l
------------------------------------------------------------

class
  ( Ord (Key table)
  , MemPack (Key table)
  , Eq (Value table blk)
  , IndexedMemPack (MemPackIdx blk mk) (Value table blk)
  ) =>
  KVConstraintsMK blk mk table
instance
  ( Ord (Key table)
  , MemPack (Key table)
  , Eq (Value table blk)
  , IndexedMemPack (MemPackIdx blk mk) (Value table blk)
  ) =>
  KVConstraintsMK blk mk table

class
  ( SingI (TablesForBlock blk)
  , All (KVConstraintsMK blk mk) (TablesForBlock blk)
  ) =>
  LedgerTableConstraintsMK blk mk
instance
  ( SingI (TablesForBlock blk)
  , All (KVConstraintsMK blk mk) (TablesForBlock blk)
  ) =>
  LedgerTableConstraintsMK blk mk
