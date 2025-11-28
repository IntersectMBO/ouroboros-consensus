{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Classes for 'MapKind's and concrete 'MapKind's
module Ouroboros.Consensus.Ledger.Tables.MapKind
  ( -- * Classes
    CanMapMK (..)
  , EqMK
  , NoThunksMK
  , ShowMK
  , ZeroableMK (..)

    -- * Concrete MapKinds
  , CodecMK (..)
  , DiffMK (..)
  , EmptyMK (..)
  , KeysMK (..)
  --  , SeqDiffMK (..)
  , TrackingMK (..)
  , ValuesMK (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Data.Kind (Constraint)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Ledger.Tables.Basics
import Ouroboros.Consensus.Ledger.Tables.Diff (Diff (..))

-- import Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

type ZeroableMK :: F2 -> Constraint
class ZeroableMK mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v

type CanMapMK :: F2 -> Constraint
class CanMapMK mk where
  mapMK :: (v -> v') -> mk k v -> mk k v'

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type ShowMK :: F2 -> Constraint
class (forall k v. (Show k, Show v) => Show (mk k v)) => ShowMK mk

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type EqMK :: F2 -> Constraint
class (forall k v. (Eq k, Eq v) => Eq (mk k v)) => EqMK mk

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type NoThunksMK :: F2 -> Constraint
class
  (forall k v. (NoThunks k, NoThunks v) => NoThunks (mk k v)) =>
  NoThunksMK mk

{-------------------------------------------------------------------------------
  EmptyMK
-------------------------------------------------------------------------------}

data EmptyMK k v = EmptyMK
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

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
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

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
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

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
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance ZeroableMK DiffMK where
  emptyMK = DiffMK mempty

instance CanMapMK DiffMK where
  mapMK f (DiffMK d) = DiffMK $ fmap f d

{-------------------------------------------------------------------------------
  TrackingMK
-------------------------------------------------------------------------------}

data TrackingMK k v = TrackingMK !(Map k v) !(Diff k v)
  deriving (Generic, Eq, Show, NoThunks)
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance ZeroableMK TrackingMK where
  emptyMK = TrackingMK mempty mempty

instance CanMapMK TrackingMK where
  mapMK f (TrackingMK vs d) = TrackingMK (Map.map f vs) (fmap f d)

{-------------------------------------------------------------------------------
  CodecMK
-------------------------------------------------------------------------------}

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
--
-- It is important to note that in the context of the HardForkCombinator, the
-- key @k@ has to be accessible from any era we are currently in, regardless of
-- which era it was created in. Because of that, we need that the serialization
-- of the key remains stable accross eras.
--
-- Ledger will provide more efficient encoders than CBOR, which will produce a
-- @'ShortByteString'@ directly.
--
-- See also 'HasCanonicalTxIn' in
-- "Ouroboros.Consensus.HardFork.Combinator.Ledger".
--
-- We will serialize UTxO maps as unstowed ledger tables when storing snapshots
-- while using an in-memory backend for the LedgerDB.
data CodecMK k v = CodecMK
  { encodeKey :: !(k -> CBOR.Encoding)
  , encodeValue :: !(v -> CBOR.Encoding)
  , decodeKey :: !(forall s. CBOR.Decoder s k)
  , decodeValue :: !(forall s. CBOR.Decoder s v)
  }
