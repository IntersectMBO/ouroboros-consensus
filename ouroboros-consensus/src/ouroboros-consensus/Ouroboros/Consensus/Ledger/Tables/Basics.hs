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
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Ledger.Tables.Basics
  ( -- * Kinds
    LedgerStateKind
  , MapKind

    -- * Ledger tables
  , LedgerTables (..)
  , Table (..)
  , TableKV

    -- * Known tables
  , TAG (..)
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
  , WrapKey (..)
  , WrapValue (..)
  , AllKeys
  , AllValues
  , AllTables
  -- , defaultCastLedgerTables
  -- , defaultCastTable
  ) where

import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.Singletons
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.TypeLevel
import Ouroboros.Consensus.Ledger.LedgerStateType

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

-- | Something that holds two types, which intend to represent /keys/ and
-- /values/.
type MapKind = KV

-- | A LedgerState-like type that depends on a map-like type
type LedgerStateKind = MapKind -> Type

--------------------------------------------------------------------------------
-- Keys and values
--------------------------------------------------------------------------------

-- | The possible tables that Consensus is aware of.
type data TAG = UTxOTable | InstantStakeTable

------------------------------------------------------------
-- START Singletons
------------------------------------------------------------

data STAG a where
  SUTxOTable :: STAG UTxOTable
  SInstantStakeTable :: STAG InstantStakeTable

type instance Sing = STAG

instance SingI UTxOTable where
  sing = SUTxOTable
instance SingI InstantStakeTable where
  sing = SInstantStakeTable

instance TestEquality STAG where
  testEquality SUTxOTable SUTxOTable = Just Refl
  testEquality SUTxOTable _ = Nothing
  testEquality SInstantStakeTable SInstantStakeTable = Just Refl
  testEquality SInstantStakeTable _ = Nothing

------------------------------------------------------------
-- END Singletons
------------------------------------------------------------

-- | The table keys and values for each table.
type TableKV :: TAG -> Type -> (Type, Type)
type family TableKV tag blk where
  TableKV UTxOTable blk = '(TxIn blk, TxOut blk)
  TableKV InstantStakeTable blk = '(Credential blk, Coin blk)

class All (Compose c (WrapKey blk)) (TablesForBlock blk) => AllKeys c blk
instance All (Compose c (WrapKey blk)) (TablesForBlock blk) => AllKeys c blk
class All (Compose c (WrapValue blk)) (TablesForBlock blk) => AllValues c blk
instance All (Compose c (WrapValue blk)) (TablesForBlock blk) => AllValues c blk

type family Key blk tag where
  Key blk tag = Fst (TableKV tag blk)

newtype WrapKey blk tag = WrapKey (Key blk tag)

deriving instance NoThunks (Fst (TableKV tag blk)) => NoThunks (WrapKey blk tag)
deriving instance Show (Fst (TableKV tag blk)) => Show (WrapKey blk tag)
deriving instance Eq (Fst (TableKV tag blk)) => Eq (WrapKey blk tag)

type family Value blk tag where
  Value blk tag = Snd (TableKV tag blk)

newtype WrapValue blk tag = WrapValue (Value blk tag)

deriving instance NoThunks (Snd (TableKV tag blk)) => NoThunks (WrapValue blk tag)
deriving instance Show (Snd (TableKV tag blk)) => Show (WrapValue blk tag)
deriving instance Eq (Snd (TableKV tag blk)) => Eq (WrapValue blk tag)

type Credential :: Type -> Type
type family Credential blk

type Coin :: Type -> Type
type family Coin blk

type TxIn :: Type -> Type
type family TxIn blk

type TxOut :: Type -> Type
type family TxOut blk

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | A table value
type Table :: KV -> Type -> TAG -> Type
newtype Table mk blk tag = Table {getTable :: mk (Fst (TableKV tag blk)) (Snd (TableKV tag blk))}
  deriving Generic

deriving instance NoThunks (mk (Key blk tag) (Value blk tag)) => NoThunks (Table mk blk tag)
deriving instance Show (mk (Key blk tag) (Value blk tag)) => Show (Table mk blk tag)
deriving instance Eq (mk (Key blk tag) (Value blk tag)) => Eq (Table mk blk tag)

class All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l
instance All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l

-- | Each block will declare its tables.
--
-- > TablesForBlock (LedgerState Byron) = '[]
--
-- > TablesForBlock (LedgerState Shelley) = '[UTxOTable]
--
-- > TablesForBlock (LedgerState Conway) = '[UTxOTable, InstantStakeTable]
type TablesForBlock :: Type -> [TAG]
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
type LedgerTables :: Type -> MapKind -> Type
newtype LedgerTables blk mk = LedgerTables
  { getLedgerTables :: NP (Table mk blk) (TablesForBlock blk)
  }
  deriving Generic

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

data Membership (xs :: [TAG]) (x :: TAG) where
  MZ :: Membership (x ': xs) x
  MS :: Membership xs x -> Membership (y ': xs) x

deriving instance Show (Membership xs x)

findMembership ::
  forall (xs :: [TAG]) (x :: TAG).
  SList xs ->
  Sing x ->
  Maybe (Membership xs x)
findMembership SNil _ = Nothing
findMembership (SCons sy sxs) sx =
  case testEquality sx sy of
    Just Refl -> Just MZ
    Nothing -> MS <$> findMembership sxs sx

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

-- | Given a proof of inclusion, create a lens for the particular item in the NP
ixNP :: Membership xs x -> Lens' (NP f xs) (f x)
ixNP MZ f (x :* xs) = (:* xs) <$> f x
ixNP (MS m) f (x :* xs) = (x :*) <$> ixNP m f xs

setterForSing ::
  forall l mk tag.
  SList (TablesForBlock l) ->
  Sing tag ->
  Maybe (ASetter' (LedgerTables l mk) (Table mk l tag))
setterForSing sxs sx =
  (\mbm -> \f -> fmap LedgerTables . ixNP mbm f . getLedgerTables) <$> findMembership sxs sx

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Auxiliary information for @IndexedMemPack@. @mk@ is there only because the
-- last instance needs to be a full type.
type MemPackIdx :: Type -> MapKind -> Type
type family MemPackIdx l mk where
  MemPackIdx blk mk = LedgerState blk mk

--------------------------------------------------------------------------------
-- Auxiliary classes
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Single l
------------------------------------------------------------

class
  ( Ord (Fst (TableKV tag blk))
  , MemPack (Fst (TableKV tag blk))
  , Eq (Snd (TableKV tag blk))
  , IndexedMemPack (MemPackIdx blk mk) (Snd (TableKV tag blk))
  ) =>
  KVConstraintsMK blk mk tag
instance
  ( Ord (Fst (TableKV tag blk))
  , MemPack (Fst (TableKV tag blk))
  , Eq (Snd (TableKV tag blk))
  , IndexedMemPack (MemPackIdx blk mk) (Snd (TableKV tag blk))
  ) =>
  KVConstraintsMK blk mk tag

class
  (SingI (TablesForBlock blk), All (KVConstraintsMK blk mk) (TablesForBlock blk)) =>
  LedgerTableConstraintsMK blk mk
instance
  (SingI (TablesForBlock blk), All (KVConstraintsMK blk mk) (TablesForBlock blk)) =>
  LedgerTableConstraintsMK blk mk
