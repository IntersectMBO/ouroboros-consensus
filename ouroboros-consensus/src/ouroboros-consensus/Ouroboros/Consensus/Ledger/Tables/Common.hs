{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Tables.Common (
    -- * Kinds
    LedgerStateKind
  , MapKind
    -- * Ledger tables
  , Castable
  , Key
  , LedgerTables (..)
  , Value
  , castLedgerTables
  ) where

import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Ticked (Ticked1)

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

type MapKind         = {- key -} Type -> {- value -} Type -> Type
type LedgerStateKind = MapKind -> Type

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

-- | The Ledger Tables represent the portion of the data on disk that has been
-- pulled from disk and attached to the in-memory Ledger State or that will
-- eventually be written to disk.
--
-- With UTxO-HD and the split of the Ledger State into the in-memory part and
-- the on-disk part, this splitting was reflected in the new type parameter
-- added to the Ledger State, to which we refer as "the MapKind" or @mk@.
--
-- Every @'LedgerState'@ is associated with a @'LedgerTables'@ and they both
-- share the @mk@. They both are of kind @'LedgerStateKind'@. @'LedgerTables'@
-- is just a way to refer /only/ to a partial view of the on-disk data without
-- having the rest of the in-memory Ledger State in scope.
--
-- The @mk@ can be instantiated to anything that is map-like, i.e. that expects
-- two type parameters, the key and the value.
type LedgerTables :: LedgerStateKind -> MapKind -> Type
newtype LedgerTables l mk = LedgerTables {
    getLedgerTables :: mk (Key l) (Value l)
  }
  deriving stock Generic

deriving stock instance Show (mk (Key l) (Value l))
                     => Show (LedgerTables l mk)
deriving stock instance Eq (mk (Key l) (Value l))
                     => Eq (LedgerTables l mk)
deriving newtype instance NoThunks (mk (Key l) (Value l))
                       => NoThunks (LedgerTables l mk)

type Key :: LedgerStateKind -> Type
type family Key l

type Value :: LedgerStateKind -> Type
type family Value l

type instance Key   (LedgerTables l) = Key l
type instance Value (LedgerTables l) = Value l
type instance Key   (Ticked1 l)      = Key l
type instance Value (Ticked1 l)      = Value l

type Castable l l' = (Key l ~ Key l', Value l ~ Value l')

castLedgerTables ::
     forall l' l mk. Castable l l'
  => LedgerTables l mk
  -> LedgerTables l' mk
castLedgerTables = coerce

