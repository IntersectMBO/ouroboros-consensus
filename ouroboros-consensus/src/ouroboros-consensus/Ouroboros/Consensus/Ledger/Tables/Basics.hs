{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
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

  --
  --  For convenience' sake, we define these kinds which convey the intended
  -- instantiation for the type variables.
    LedgerStateKind
  , MapKind

    -- * Ledger tables
  , LedgerTables (..)
  , Table (..)
  , TAG (..)
  , STAG (..)
  , TablesForBlock
  , TxIn
  , TxOut
  , AccountState
  , Credential
  , onUTxOTable
  , onAccountsTable
  , onTable

    -- * Helpers
  , MemPackIdx

    -- * Casting
  , SameUtxoTypes
  , castLedgerTables
  , Sing
  , sing
  , SingI
  ) where

import Data.Eq.Singletons
import Data.Kind (Type)
import Data.Proxy
import Data.SOP.Constraint
import Data.SOP.Sing
import Data.SOP.Strict
import Data.Singletons (Sing, SingI (..), singByProxy)
import Data.Singletons.Decide
import Data.Type.Equality (TestEquality (testEquality), (:~:) (..))
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.TypeLevel
import qualified Prelude.Singletons as S

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

-- | Something that holds two types, which intend to represent /keys/ and
-- /values/.
type MapKind = KV

type LedgerStateKind = MapKind -> Type -- LedgerState ByronBlock

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | The possible tables that Consensus is aware of.
type data TAG = UTxOTable | AccountsTable

------------------------------------------------------------
-- START Singletons
------------------------------------------------------------
data STAG a where
  SUTxOTable :: STAG UTxOTable
  SAccountsTable :: STAG AccountsTable

type instance Sing = STAG

instance SingI UTxOTable where
  sing = SUTxOTable
instance SingI AccountsTable where
  sing = SAccountsTable

instance TestEquality STAG where
  testEquality SUTxOTable SUTxOTable = Just Refl
  testEquality SUTxOTable _ = Nothing
  testEquality SAccountsTable SAccountsTable = Just Refl
  testEquality SAccountsTable _ = Nothing

------------------------------------------------------------
-- END Singletons
------------------------------------------------------------

-- | The table keys and values for each table.
type TableKV :: TAG -> LedgerStateKind -> (Type, Type)
type family TableKV tag l where
  TableKV UTxOTable l = '(TxIn l, TxOut l)
  TableKV AccountsTable l = '(Credential l, AccountState l)

-- | A table value
type Table :: KV -> LedgerStateKind -> TAG -> Type
newtype Table mk l tag = Table {getTable :: mk (Fst (TableKV tag l)) (Snd (TableKV tag l))}

-- | Each block will declare its tables.
--
-- > TablesForBlock (LedgerState Byron) = '[]
--
-- > TablesForBlock (LedgerState Shelley) = '[UTxOTable]
--
-- > TablesForBlock (LedgerState Conway) = '[UTxOTable, AccountsTable]
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

deriving newtype instance
  All
    (Compose NoThunks (Table mk l))
    (TablesForBlock l) =>
  NoThunks (LedgerTables l mk)

------------------------------------------------------------
-- Type proof of inclusion
------------------------------------------------------------

data Membership (xs :: [TAG]) (x :: TAG) where
  MZ :: Membership (x ': xs) x
  MS :: Membership xs x -> Membership (y ': xs) x

deriving instance Show (Membership xs x)

findMembership ::
  forall (xs :: [TAG]) (x :: TAG).
  S.SList xs ->
  S.Sing x ->
  Maybe (Membership xs x)
findMembership S.SNil _ = Nothing
findMembership (S.SCons sy sxs) sx =
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
ixNP _ _ Nil = error "impossible: membership longer than NP"

setterForSing ::
  forall l mk tag.
  S.SList (TablesForBlock l) -> -- '[UTxOtable, AccountsTable]
  S.Sing tag -> -- SUTxoTable
  Maybe (ASetter' (LedgerTables l mk) (Table mk l tag))
setterForSing sxs sx =
  (\mbm -> \f -> fmap LedgerTables . ixNP mbm f . getLedgerTables) <$> findMembership sxs sx

onUTxOTable ::
  SLIST (TablesForBlock l) =>
  Proxy l -> ASetter' (LedgerTables l mk) (Table mk l UTxOTable)
onUTxOTable p = onTable p (Proxy @UTxOTable)

onAccountsTable ::
  SLIST (TablesForBlock l) =>
  Proxy l -> ASetter' (LedgerTables l mk) (Table mk l AccountsTable)
onAccountsTable p = onTable p (Proxy @AccountsTable)

onTable ::
  forall l (tag :: TAG) mk.
  (SingI tag, SLIST (TablesForBlock l)) =>
  Proxy l ->
  Proxy tag ->
  ASetter' (LedgerTables l mk) (Table mk l tag)
onTable _ _ =
  case setterForSing sListt (sing :: Sing tag) of
    Nothing -> \_ s -> pure s
    Just setter -> setter

class SLIST xs where
  sListt :: S.SList xs

type Credential :: LedgerStateKind -> Type
type family Credential l

type AccountState :: LedgerStateKind -> Type
type family AccountState l

type TxIn :: LedgerStateKind -> Type
type family TxIn l

type TxOut :: LedgerStateKind -> Type
type family TxOut l

type instance TxIn (LedgerTables l) = TxIn l
type instance TxOut (LedgerTables l) = TxOut l
type instance Credential (LedgerTables l) = Credential l
type instance AccountState (LedgerTables l) = AccountState l
type instance TablesForBlock (LedgerTables l) = TablesForBlock l

type instance TxIn (Ticked l) = TxIn l
type instance TxOut (Ticked l) = TxOut l
type instance Credential (Ticked l) = Credential l
type instance AccountState (Ticked l) = AccountState l
type instance TablesForBlock (Ticked l) = TablesForBlock l

-- | Auxiliary information for @IndexedMemPack@.
type MemPackIdx :: LedgerStateKind -> MapKind -> Type
type family MemPackIdx l mk where
  MemPackIdx (LedgerTables l) mk = MemPackIdx l mk
  MemPackIdx (Ticked l) mk = MemPackIdx l mk
  MemPackIdx l mk = l mk

type SameUtxoTypes l l' =
  (SListI (TablesForBlock l), SListI (TablesForBlock l'), TablesForBlock l ~ TablesForBlock l')

castLedgerTables ::
  ( SameShapeAs (TablesForBlock l) (TablesForBlock l')
  , AllZipF
      (LiftedCoercible (Table mk l) (Table mk l'))
      (TablesForBlock l)
      (TablesForBlock l')
  , SameUtxoTypes l l'
  ) =>
  LedgerTables l mk ->
  LedgerTables l' mk
castLedgerTables (LedgerTables l1) = LedgerTables (hcoerce l1)
