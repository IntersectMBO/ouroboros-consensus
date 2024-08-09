{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Classes for 'MapKind's and concrete 'MapKind's
module Ouroboros.Consensus.Ledger.Tables.MapKind (
    -- * Classes
    CanMapKeysMK (..)
  , CanMapMK (..)
  , EqMK
  , NoThunksMK
  , ShowMK
  , ZeroableMK (..)
    -- * Concrete MapKinds
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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Ledger.Tables.Basics
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq (DiffSeq)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DiffSeq
import           Ouroboros.Consensus.Ledger.Tables.UtxoDiff (UtxoDiff)
import qualified Ouroboros.Consensus.Ledger.Tables.UtxoDiff as UtxoDiff

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

type ZeroableMK :: MapKind -> Constraint
class ZeroableMK mk where
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

instance ZeroableMK EmptyMK where
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

instance ZeroableMK KeysMK where
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

instance ZeroableMK ValuesMK where
  emptyMK = ValuesMK mempty

instance CanMapMK ValuesMK where
  mapMK f (ValuesMK vs) = ValuesMK $ Map.map f vs

instance CanMapKeysMK ValuesMK where
  mapKeysMK f (ValuesMK vs) = ValuesMK $ Map.mapKeys f vs

{-------------------------------------------------------------------------------
  DiffMK
-------------------------------------------------------------------------------}

newtype DiffMK k v = DiffMK { getDiffMK :: UtxoDiff k v }
  deriving stock (Generic, Eq, Show)
  deriving newtype Functor
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance ZeroableMK DiffMK where
  emptyMK = DiffMK UtxoDiff.empty

instance CanMapKeysMK DiffMK where
  mapKeysMK f (DiffMK (UtxoDiff.UtxoDiff m1 m2)) =
     DiffMK $ UtxoDiff.UtxoDiff (Map.mapKeys f m1) (Map.mapKeys f m2)

instance CanMapMK DiffMK where
  mapMK f (DiffMK d) = DiffMK $ fmap f d

{-------------------------------------------------------------------------------
  TrackingMK
-------------------------------------------------------------------------------}

data TrackingMK k v = TrackingMK !(Map k v) !(UtxoDiff k v)
  deriving (Generic, Eq, Show, NoThunks)
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance ZeroableMK TrackingMK where
  emptyMK = TrackingMK mempty UtxoDiff.empty

instance CanMapMK TrackingMK where
  mapMK f (TrackingMK vs d) = TrackingMK (fmap f vs) (fmap f d)

instance CanMapKeysMK TrackingMK where
  mapKeysMK f (TrackingMK vs d) =
      TrackingMK
        (getValuesMK . mapKeysMK f . ValuesMK $ vs)
        (getDiffMK . mapKeysMK f . DiffMK $ d)

{-------------------------------------------------------------------------------
  SeqDiffMK
-------------------------------------------------------------------------------}

newtype SeqDiffMK  k v = SeqDiffMK { getSeqDiffMK :: DiffSeq k v }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks
  deriving anyclass (ShowMK, EqMK, NoThunksMK)

instance ZeroableMK SeqDiffMK where
  emptyMK = SeqDiffMK DiffSeq.empty


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
data CodecMK k v = CodecMK {
    encodeKey   :: !(k -> CBOR.Encoding)
  , encodeValue :: !(v -> CBOR.Encoding)
  , decodeKey   :: !(forall s . CBOR.Decoder s k)
  , decodeValue :: !(forall s . CBOR.Decoder s v)
  }
