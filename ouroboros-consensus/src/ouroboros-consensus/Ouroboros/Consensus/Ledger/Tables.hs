{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | This module defines the 'LedgerTables', a portion of the Ledger notion of a
-- /ledger state/ (not to confuse with our
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState') that together with it,
-- conforms a complete Ledger /ledger state/.
--
-- In particular, 'LedgerTables' contain maps from keys to values. For now,
-- their only current instantiation is to hold the UTxO set, but future features
-- will extend this to hold some other data structures.
--
-- The overall goal this type achieves is to extract and inject these parts of
-- the /ledger state/, and once extracted, be able to dump them in some disk
-- backend, such that the memory usage of the process is smaller. See
-- 'Ouroboros.Consensus.Storage.LedgerDB.BackingStore' and
-- 'Ouroboros.Consensus.Storage.LedgerDB.DbChangelog' for usages of this notion.
module Ouroboros.Consensus.Ledger.Tables (
    -- * Core
    module Ouroboros.Consensus.Ledger.Tables.Basics
  , module Ouroboros.Consensus.Ledger.Tables.MapKind
    -- * Utilities
  , module Ouroboros.Consensus.Ledger.Tables.Combinators
    -- * Basic LedgerState classes
  , CanStowLedgerTables (..)
  , HasLedgerTables (..)
  , HasTickedLedgerTables
    -- * Serialization
  , CanSerializeLedgerTables
  , codecLedgerTables
  , valuesMKDecoder
  , valuesMKEncoder
    -- * Special classes
  , LedgerTablesAreTrivial
  , convertMapKind
  , trivialLedgerTables
  ) where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Control.Monad (replicateM)
import           Data.Kind (Constraint)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Ledger.Tables.Basics
import           Ouroboros.Consensus.Ledger.Tables.Combinators
import           Ouroboros.Consensus.Ledger.Tables.MapKind
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

-- | Extracting @'LedgerTables'@ from @l mk@ (which will share the same @mk@),
-- or replacing the @'LedgerTables'@ associated to a particular @l@.
type HasLedgerTables :: LedgerStateKind -> Constraint
class ( Ord (Key l)
      , Eq (Value l)
      , Show (Key l)
      , Show (Value l)
      , NoThunks (Key l)
      , NoThunks (Value l)
      ) => HasLedgerTables l where

  -- | Extract the ledger tables from a ledger state
  --
  -- The constraints on @mk@ are necessary because the 'CardanoBlock' instance
  -- uses them.
  projectLedgerTables ::
       (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => l mk
    -> LedgerTables l mk
  default projectLedgerTables ::
         (ZeroableMK mk, LedgerTablesAreTrivial l)
      => l mk
      -> LedgerTables l mk
  projectLedgerTables _ = trivialLedgerTables

  -- | Overwrite the tables in the given ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- The constraints on @mk@ are necessary because the 'CardanoBlock' instance
  -- uses them.
  withLedgerTables ::
       (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => l any
    -> LedgerTables l mk
    -> l mk
  default withLedgerTables ::
       LedgerTablesAreTrivial l
    => l any
    -> LedgerTables l mk
    -> l mk
  withLedgerTables st _ = convertMapKind st

instance ( Ord (Key l)
         , Eq (Value l)
         , Show (Key l)
         , Show (Value l)
         , NoThunks (Key l)
         , NoThunks (Value l)
         ) => HasLedgerTables (LedgerTables l) where
  projectLedgerTables = castLedgerTables
  withLedgerTables _ = castLedgerTables

-- | Convenience class, useful for partially applying the composition of
-- 'HasLedgerTables' and 'Ticked1'.
type HasTickedLedgerTables :: LedgerStateKind -> Constraint
class HasLedgerTables (Ticked1 l) => HasTickedLedgerTables l where
instance HasLedgerTables (Ticked1 l) => HasTickedLedgerTables l

-- | LedgerTables are projections of data from a LedgerState and as such they
-- can be injected back into a LedgerState. This is necessary because the Ledger
-- rules are unaware of UTxO-HD changes. Thus, by stowing the ledger tables, we are
-- able to provide a Ledger State with a restricted UTxO set that is enough to
-- execute the Ledger rules.
--
-- In particular, HardForkBlock LedgerStates are never given diretly to the
-- ledger but rather unwrapped and then it is the inner ledger state the one we
-- give to the ledger. This means that all the single era blocks must be an
-- instance of this class, but HardForkBlocks might avoid doing so.
type CanStowLedgerTables :: LedgerStateKind -> Constraint
class CanStowLedgerTables l where

  stowLedgerTables     :: l ValuesMK -> l EmptyMK
  default stowLedgerTables ::
       (LedgerTablesAreTrivial l)
    => l ValuesMK
    -> l EmptyMK
  stowLedgerTables = convertMapKind

  unstowLedgerTables   :: l EmptyMK  -> l ValuesMK
  default unstowLedgerTables ::
       (LedgerTablesAreTrivial l)
    => l EmptyMK
    -> l ValuesMK
  unstowLedgerTables = convertMapKind

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | This class provides a 'CodecMK' that can be used to encode/decode keys and
-- values on @'LedgerTables' l mk@
type CanSerializeLedgerTables :: LedgerStateKind -> Constraint
class CanSerializeLedgerTables l where
  codecLedgerTables :: LedgerTables l CodecMK
  default codecLedgerTables ::
       ( FromCBOR (Key l), FromCBOR (Value l)
       , ToCBOR   (Key l), ToCBOR   (Value l)
       )
    => LedgerTables l CodecMK
  codecLedgerTables = LedgerTables $ CodecMK toCBOR toCBOR fromCBOR fromCBOR

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
     ( HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => LedgerTables l ValuesMK
  -> CBOR.Encoding
valuesMKEncoder tables =
       CBOR.encodeListLen (ltcollapse $ ltmap (K2 . const 1) tables)
    <> ltcollapse (ltliftA2 (K2 .: go) codecLedgerTables tables)
  where
    go :: CodecMK k v -> ValuesMK k v -> CBOR.Encoding
    go (CodecMK encK encV _decK _decV) (ValuesMK m) =
         CBOR.encodeMapLen (fromIntegral $ Map.size m)
      <> Map.foldMapWithKey (\k v -> encK k <> encV v) m

-- | Default decoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKDecoder ::
     ( HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => CBOR.Decoder s (LedgerTables l ValuesMK)
valuesMKDecoder = do
    numTables <- CBOR.decodeListLen
    if numTables == 0
      then
        return $ ltpure emptyMK
      else do
        mapLen <- CBOR.decodeMapLen
        ret    <- lttraverse (go mapLen) codecLedgerTables
        Exn.assert (ltcollapse (ltmap (K2 . const 1) ret) == numTables)
          $ return ret
 where
  go :: Ord k
     => Int
     -> CodecMK k v
     -> CBOR.Decoder s (ValuesMK k v)
  go len (CodecMK _encK _encV decK decV) =
        ValuesMK . Map.fromList
    <$> replicateM len ((,) <$> decK <*> decV)

{-------------------------------------------------------------------------------
  Special classes of ledger states
-------------------------------------------------------------------------------}

type LedgerTablesAreTrivial :: LedgerStateKind -> Constraint
-- | For some ledger states we won't be defining 'LedgerTables' and instead the
-- ledger state will be fully stored in memory, as before UTxO-HD. The ledger
-- states that are defined this way can be made instances of this class which
-- allows for easy manipulation of the types of @mk@ required at any step of the
-- program.
class (Key l ~ Void, Value l ~ Void) => LedgerTablesAreTrivial l where
  -- | If the ledger state is always in memory, then @l mk@ will be isomorphic
  -- to @l mk'@ for all @mk@, @mk'@. As a result, we can convert between ledgers
  -- states indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg @applyChainTickLedgerResult@).
  convertMapKind :: l mk -> l mk'

trivialLedgerTables ::
     (ZeroableMK mk, LedgerTablesAreTrivial l)
  => LedgerTables l mk
trivialLedgerTables = LedgerTables emptyMK
