{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Classes for 'MapKind's and concrete 'MapKind's
module Ouroboros.Consensus.Ledger.Tables.MapKind
  ( -- * Classes
    CanMapMK (..)
  , ZeroableMK (..)

    -- * Concrete MapKinds
  , DiffMK (..)
  , EmptyMK (..)
  , KeysMK (..)
  , TrackingMK (..)
  , ValuesMK (..)
  ) where

import Data.Kind (Constraint)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Ledger.LedgerStateType
import Ouroboros.Consensus.Ledger.Tables.Diff (Diff (..))

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

type ZeroableMK :: F2 -> Constraint
class ZeroableMK mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v

type CanMapMK :: F2 -> Constraint
class CanMapMK mk where
  mapMK :: (v -> v') -> mk k v -> mk k v'

{-------------------------------------------------------------------------------
  EmptyMK
-------------------------------------------------------------------------------}

data EmptyMK k v = EmptyMK
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

instance ZeroableMK EmptyMK where
  emptyMK = EmptyMK

instance CanMapMK EmptyMK where
  mapMK _ EmptyMK = EmptyMK

{-------------------------------------------------------------------------------
  KeysMK
-------------------------------------------------------------------------------}

newtype KeysMK k v = KeysMK (Set k)
  deriving stock (Generic, Eq, Show)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NoThunks

instance ZeroableMK KeysMK where
  emptyMK = KeysMK mempty

instance CanMapMK KeysMK where
  mapMK _ (KeysMK ks) = KeysMK ks

{-------------------------------------------------------------------------------
  ValuesMK
-------------------------------------------------------------------------------}

newtype ValuesMK k v = ValuesMK {getValuesMK :: Map k v}
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

instance ZeroableMK ValuesMK where
  emptyMK = ValuesMK mempty

instance CanMapMK ValuesMK where
  mapMK f (ValuesMK vs) = ValuesMK $ Map.map f vs

{-------------------------------------------------------------------------------
  DiffMK
-------------------------------------------------------------------------------}

newtype DiffMK k v = DiffMK {getDiffMK :: Diff k v}
  deriving stock (Generic, Eq, Show)
  deriving newtype Functor
  deriving anyclass NoThunks

instance ZeroableMK DiffMK where
  emptyMK = DiffMK mempty

instance CanMapMK DiffMK where
  mapMK f (DiffMK d) = DiffMK $ fmap f d

{-------------------------------------------------------------------------------
  TrackingMK
-------------------------------------------------------------------------------}

data TrackingMK k v = TrackingMK !(Map k v) !(Diff k v)
  deriving (Generic, Eq, Show, NoThunks)

instance ZeroableMK TrackingMK where
  emptyMK = TrackingMK mempty mempty

instance CanMapMK TrackingMK where
  mapMK f (TrackingMK vs d) = TrackingMK (Map.map f vs) (fmap f d)
