{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
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

  --
  --  For convenience' sake, we define these kinds which convey the intended
  -- instantiation for the type variables.
    LedgerStateKind
  , MapKind

    -- * Ledger tables
  , LedgerTables (..)
  , Table (..)
  , TAG (..)
  , TableIndices
  , TxIn
  , TxOut
  , AccountState
  , Credential

    -- * Helpers
  , MemPackIdx

    -- * Casting
  , SameUtxoTypes
  , castLedgerTables
  ) where

import Data.Kind (Type)
import Data.SOP.Constraint
import Data.SOP.Strict
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util.TypeLevel

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

-- | Something that holds two types, which intend to represent /keys/ and
-- /values/.
type MapKind = KV

type LedgerStateKind = MapKind -> Type

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
type LedgerTables :: LedgerStateKind -> MapKind -> Type
newtype LedgerTables l mk = LedgerTables
  { getLedgerTables :: NP (Table mk l) (TableIndices l)
  }
  deriving Generic

deriving newtype instance
  All
    (Compose NoThunks (Table mk l))
    (TableIndices l) =>
  NoThunks (LedgerTables l mk)

type Table :: KV -> LedgerStateKind -> TAG -> Type
newtype Table mk l tag = Table (mk (Fst (KVs tag l)) (Snd (KVs tag l)))

type data TAG = UTxOTable | AccountsTable

type KVs :: TAG -> LedgerStateKind -> (Type, Type)
type family KVs (tag :: TAG) l where
  KVs UTxOTable l = '(TxIn l, TxOut l)
  KVs AccountsTable l = '(Credential l, AccountState l)

type TableIndices :: LedgerStateKind -> [TAG]
type family TableIndices l

-- type LS :: Type -> LedgerStateKind
-- data family LS blk mk

-- data Byron
-- type instance TableIndices (LS Byron) = '[]

-- data BlkA
-- type instance TableIndices (LS BlkA) = '[UTxOTable]

-- data BlkB
-- type instance TableIndices (LS BlkB) = '[UTxOTable (LS BlkB), AccountsTable (LS BlkB)]

-- type HFBlock = '[Byron, BlkA, BlkB]

-- type instance TableIndices (LS HFBlk) = '[UTxOTable (LS HFBlk), AccountsTable (LS BlkB)]

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
type instance TableIndices (LedgerTables l) = TableIndices l

type instance TxIn (Ticked l) = TxIn l
type instance TxOut (Ticked l) = TxOut l
type instance Credential (Ticked l) = Credential l
type instance AccountState (Ticked l) = AccountState l
type instance TableIndices (Ticked l) = TableIndices l

-- | Auxiliary information for @IndexedMemPack@.
type MemPackIdx :: LedgerStateKind -> MapKind -> Type
type family MemPackIdx l mk where
  MemPackIdx (LedgerTables l) mk = MemPackIdx l mk
  MemPackIdx (Ticked l) mk = MemPackIdx l mk
  MemPackIdx l mk = l mk

type SameUtxoTypes l l' =
  (SListI (TableIndices l), SListI (TableIndices l'), TableIndices l ~ TableIndices l')

castLedgerTables ::
  ( SameShapeAs (TableIndices l) (TableIndices l')
  , AllZipF
      (LiftedCoercible (Table mk l) (Table mk l'))
      (TableIndices l)
      (TableIndices l')
  , SameUtxoTypes l l'
  ) =>
  LedgerTables l mk ->
  LedgerTables l' mk
castLedgerTables (LedgerTables l1) = LedgerTables (hcoerce l1)
