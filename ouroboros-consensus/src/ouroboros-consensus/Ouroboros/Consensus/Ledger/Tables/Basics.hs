{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
  , SameTablesTypes
  , SameTypesAsMyself
  , CanCastLedgerTables (..)
  , CanCastTables (..)
  , Key
  , Value
  , WrapKey (..)
  , WrapValue (..)
  , AllKeys
  , AllValues
  , AllTables
  ) where

import Data.Coerce
import Data.Kind (Type)
import Data.List.Singletons hiding (All)
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.Singletons
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.TypeLevel

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
type TableKV :: TAG -> LedgerStateKind -> (Type, Type)
type family TableKV tag l where
  TableKV UTxOTable l = '(TxIn l, TxOut l)
  TableKV InstantStakeTable l = '(Credential l, Coin l)

class All (Compose c (WrapKey l)) (TablesForBlock l) => AllKeys c l
instance All (Compose c (WrapKey l)) (TablesForBlock l) => AllKeys c l
class All (Compose c (WrapValue l)) (TablesForBlock l) => AllValues c l
instance All (Compose c (WrapValue l)) (TablesForBlock l) => AllValues c l

type family Key l tag where
  Key l tag = Fst (TableKV tag l)

newtype WrapKey l tag = WrapKey (Key l tag)

deriving instance NoThunks (Fst (TableKV tag l)) => NoThunks (WrapKey l tag)
deriving instance Show (Fst (TableKV tag l)) => Show (WrapKey l tag)
deriving instance Eq (Fst (TableKV tag l)) => Eq (WrapKey l tag)

newtype WrapValue l tag = WrapValue (Value l tag)

deriving instance NoThunks (Snd (TableKV tag l)) => NoThunks (WrapValue l tag)
deriving instance Show (Snd (TableKV tag l)) => Show (WrapValue l tag)
deriving instance Eq (Snd (TableKV tag l)) => Eq (WrapValue l tag)

type family Value l tag where
  Value l tag = Snd (TableKV tag l)

type Credential :: LedgerStateKind -> Type
type family Credential l

type Coin :: LedgerStateKind -> Type
type family Coin l

type TxIn :: LedgerStateKind -> Type
type family TxIn l

type TxOut :: LedgerStateKind -> Type
type family TxOut l

type instance TxIn (LedgerTables l) = TxIn l
type instance TxOut (LedgerTables l) = TxOut l
type instance Credential (LedgerTables l) = Credential l
type instance Coin (LedgerTables l) = Coin l
type instance TablesForBlock (LedgerTables l) = TablesForBlock l

type instance TxIn (Ticked l) = TxIn l
type instance TxOut (Ticked l) = TxOut l
type instance Credential (Ticked l) = Credential l
type instance Coin (Ticked l) = Coin l
type instance TablesForBlock (Ticked l) = TablesForBlock l

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | A table value
type Table :: KV -> LedgerStateKind -> TAG -> Type
newtype Table mk l tag = Table {getTable :: mk (Fst (TableKV tag l)) (Snd (TableKV tag l))} deriving Generic

deriving instance NoThunks (mk (Key l tag) (Value l tag)) => NoThunks (Table mk l tag)
deriving instance Show (mk (Key l tag) (Value l tag)) => Show (Table mk l tag)
deriving instance Eq (mk (Key l tag) (Value l tag)) => Eq (Table mk l tag)

class All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l
instance All (Compose c (Table mk l)) (TablesForBlock l) => AllTables c mk l

-- | Each block will declare its tables.
--
-- > TablesForBlock (LedgerState Byron) = '[]
--
-- > TablesForBlock (LedgerState Shelley) = '[UTxOTable]
--
-- > TablesForBlock (LedgerState Conway) = '[UTxOTable, InstantStakeTable]
type TablesForBlock :: LedgerStateKind -> [TAG]
type family TablesForBlock l

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
type LedgerTables :: LedgerStateKind -> MapKind -> Type
newtype LedgerTables l mk = LedgerTables
  { getLedgerTables :: NP (Table mk l) (TablesForBlock l)
  }
  deriving Generic

{-

Byron -> Nil

Shelley -> Table mk UTxOTable Shelley :* Nil

Conway -> Table mk UTxOTable Conway :* Table mk InstantStakeTable Conway :* Nil

-}

deriving newtype instance
  AllTables NoThunks mk l =>
  NoThunks (LedgerTables l mk)
deriving newtype instance
  AllTables Show mk l =>
  Show (LedgerTables l mk)
deriving newtype instance
  AllTables Eq mk l =>
  Eq (LedgerTables l mk)

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
type MemPackIdx :: LedgerStateKind -> MapKind -> Type
type family MemPackIdx l mk where
  MemPackIdx (LedgerTables l) mk = MemPackIdx l mk
  MemPackIdx (Ticked l) mk = MemPackIdx l mk
  MemPackIdx l mk = l mk

--------------------------------------------------------------------------------
-- Auxiliary classes
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Single l
------------------------------------------------------------

class
  ( Ord (Fst (TableKV tag l))
  , MemPack (Fst (TableKV tag l))
  , Eq (Snd (TableKV tag l))
  , IndexedMemPack (MemPackIdx l mk) (Snd (TableKV tag l))
  ) =>
  KVConstraintsMK l mk tag
instance
  ( Ord (Fst (TableKV tag l))
  , MemPack (Fst (TableKV tag l))
  , Eq (Snd (TableKV tag l))
  , IndexedMemPack (MemPackIdx l mk) (Snd (TableKV tag l))
  ) =>
  KVConstraintsMK l mk tag

class
  ( SingI (TablesForBlock l)
  , All (KVConstraintsMK l mk) (TablesForBlock l)
  ) =>
  LedgerTableConstraintsMK l mk
instance
  ( SingI (TablesForBlock l)
  , All (KVConstraintsMK l mk) (TablesForBlock l)
  ) =>
  LedgerTableConstraintsMK l mk

class SameTablesTypes l l mk => SameTypesAsMyself l mk
instance SameTablesTypes l l mk => SameTypesAsMyself l mk

------------------------------------------------------------
-- Two l
------------------------------------------------------------

class
  ( SameShapeAs (TablesForBlock l) (TablesForBlock l')
  , AllZipF
      (LiftedCoercible (Table mk l) (Table mk l'))
      (TablesForBlock l)
      (TablesForBlock l')
  , SListI (TablesForBlock l)
  , SListI (TablesForBlock l')
  , TablesForBlock l ~ TablesForBlock l'
  ) =>
  SameTablesTypes l l' mk
instance
  ( SameShapeAs (TablesForBlock l) (TablesForBlock l')
  , AllZipF
      (LiftedCoercible (Table mk l) (Table mk l'))
      (TablesForBlock l)
      (TablesForBlock l')
  , SListI (TablesForBlock l)
  , SListI (TablesForBlock l')
  , TablesForBlock l ~ TablesForBlock l'
  ) =>
  SameTablesTypes l l' mk

class
  (SameTablesTypes l l' mk, All (CanCastTables l l') (TablesForBlock l)) =>
  CanCastLedgerTables l l' mk
  where
  castLedgerTables ::
    LedgerTables l mk ->
    LedgerTables l' mk
  castLedgerTables (LedgerTables l1) = LedgerTables (hcoerce l1)

class (forall mk. SameTablesTypes l l' mk) => CanCastTables l l' tag where
  castTable :: Table mk l tag -> Table mk l' tag
  default castTable :: TableKV tag l ~ TableKV tag l' => Table mk l tag -> Table mk l' tag
  castTable = coerce

instance (SameTypesAsMyself l mk, All (CanCastTables l l) (TablesForBlock l)) => CanCastLedgerTables l l mk
