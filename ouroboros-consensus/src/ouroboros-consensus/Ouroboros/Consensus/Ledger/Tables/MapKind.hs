{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Ledger.Tables.MapKind (
    -- * Classes
    CanEmptyMK (..)
  , CanMapKeysMK (..)
  , CanMapMK (..)
  , EqMK
  , NoThunksMK
  , ShowMK
    -- * Concrete MapKinds
  , Canonical (..)
  , CodecMK (..)
  , DiffMK (..)
  , EmptyMK (..)
  , KeysMK (..)
  , SeqDiffMK (..)
  , TrackingMK (..)
  , ValuesMK (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind (Constraint)
import           Data.Map.Diff.Strict (Diff)
import qualified Data.Map.Diff.Strict.Internal as Diff.Internal
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Ledger.Tables.Common
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

type CanEmptyMK :: MapKind -> Constraint
class CanEmptyMK mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v

type CanMapMK :: MapKind -> Constraint
class CanMapMK mk where
  mapMK :: (v -> v') -> mk k v -> mk k v'

type CanMapKeysMK :: MapKind -> Constraint
class CanMapKeysMK mk where
  mapKeysMK :: Ord k' => (k -> k') -> mk k v -> mk k' v

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type ShowMK :: MapKind -> Constraint
class (forall k v. (Show k, Show v) => Show (mk k v)) => ShowMK mk

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type EqMK :: MapKind -> Constraint
class (forall k v. (Eq k, Eq v) => Eq (mk k v)) => EqMK mk

-- | For convenience, such that we don't have to include @QuantifiedConstraints@
-- everywhere.
type NoThunksMK :: MapKind -> Constraint
class (forall k v. (NoThunks k, NoThunks v) => NoThunks (mk k v))
   => NoThunksMK mk

{-------------------------------------------------------------------------------
  EmptyMK
-------------------------------------------------------------------------------}

data EmptyMK k v = EmptyMK
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK EmptyMK where
  emptyMK = EmptyMK

instance CanMapMK EmptyMK where
  mapMK _ EmptyMK = EmptyMK

instance CanMapKeysMK EmptyMK where
  mapKeysMK _ EmptyMK = EmptyMK

{-------------------------------------------------------------------------------
  KeysMK
-------------------------------------------------------------------------------}

newtype KeysMK k v = KeysMK (Set k)
  deriving stock (Generic, Eq, Show)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK KeysMK where
  emptyMK = KeysMK mempty

instance CanMapMK KeysMK where
  mapMK _ (KeysMK ks) = KeysMK ks

instance CanMapKeysMK KeysMK where
  mapKeysMK f (KeysMK ks) = KeysMK $ Set.map f ks

{-------------------------------------------------------------------------------
  ValuesMK
-------------------------------------------------------------------------------}

newtype ValuesMK k v = ValuesMK { getValuesMK :: Map k v }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK ValuesMK where
  emptyMK = ValuesMK mempty

instance CanMapMK ValuesMK where
  mapMK f (ValuesMK vs) = ValuesMK $ fmap f vs

instance CanMapKeysMK ValuesMK where
  mapKeysMK f (ValuesMK vs) = ValuesMK $ Map.mapKeys f vs

{-------------------------------------------------------------------------------
  EmptyMK
-------------------------------------------------------------------------------}

newtype DiffMK k v = DiffMK { getDiffMK :: Diff k v }
  deriving stock (Generic, Eq, Show)
  deriving newtype Functor
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK DiffMK where
  emptyMK = DiffMK mempty

instance CanMapKeysMK DiffMK where
  mapKeysMK f (DiffMK (Diff.Internal.Diff m)) = DiffMK . Diff.Internal.Diff $
    Map.mapKeys f m

instance CanMapMK DiffMK where
  mapMK f (DiffMK d) = DiffMK $ fmap f d

{-------------------------------------------------------------------------------
  TrackingMK
-------------------------------------------------------------------------------}

data TrackingMK k v = TrackingMK !(Map k v) !(Diff k v)
  deriving (Generic, Eq, Show, NoThunks)
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK TrackingMK where
  emptyMK = TrackingMK mempty mempty

instance CanMapMK TrackingMK where
  mapMK f (TrackingMK vs d) = TrackingMK (fmap f vs) (fmap f d)

instance CanMapKeysMK TrackingMK where
  mapKeysMK f (TrackingMK vs d) =
      TrackingMK
        (getValuesMK . mapKeysMK f . ValuesMK $ vs)
        (getDiffMK . mapKeysMK f . DiffMK $ d)

{-------------------------------------------------------------------------------
  Canonical
-------------------------------------------------------------------------------}

-- | The Canonical MapKind, which has no constructor, and therefore a
-- @LedgerState blk Canonical@ will have all the contents in memory. This is
-- just a phantom type used to sketch later development and shall not land on
-- the release version.
data Canonical k v = Canonical
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

{-------------------------------------------------------------------------------
  SeqDiffMK
-------------------------------------------------------------------------------}

newtype SeqDiffMK  k v = SeqDiffMK { getSeqDiffMK :: DiffSeq k v }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance CanEmptyMK SeqDiffMK where
  emptyMK = SeqDiffMK empty

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
data CodecMK k v = CodecMK {
    encodeKey   :: !(k -> CBOR.Encoding)
  , encodeValue :: !(v -> CBOR.Encoding)
  , decodeKey   :: !(forall s . CBOR.Decoder s k)
  , decodeValue :: !(forall s . CBOR.Decoder s v)
  }
