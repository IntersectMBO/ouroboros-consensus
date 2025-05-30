{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'LedgerTables', a portion of the Ledger notion of a
-- /ledger state/ (not to confuse with our
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState') that together with it,
-- conforms a complete Ledger /ledger state/.
--
-- 'LedgerTables' are parametrized by two types: keys and values. For now, their
-- only current instantiation is to hold the UTxO set, but future features will
-- extend this to hold other parts of the ledger state that now live in memory.
-- However, 'LedgerTables' don't necessarily have to contain maps from keys to
-- values, and the particular instantiation might choose to ignore some of those
-- types (as phantom types). See 'KeysMK' for an example.
--
-- This type is used for two main purposes. Firstly, we use ledger tables to
-- /extract/ data from the /ledger state/ and store it on secondary storage (eg
-- a solid-state hard-drive). Secondly, when we load data from disk onto memory,
-- we use ledger tables to /inject/ data into the /ledger state/. This mechanism
-- allows us to keep most of the data on disk, which is rarely used, reducing
-- the memory usage of the Consensus layer.
--
-- = __Example__
--
-- As an example, consider a LedgerState that contains a Ledger /ledger state/
-- (such as the @NewEpochState@) and a UTxO set:
--
-- @
-- data instance t'Ouroboros.Consensus.Ledger.Basics.LedgerState' (Block era) mk = LedgerState {
--     theLedgerLedgerState :: NewEpochState era
--   , theTables            :: 'LedgerTables' (Block era) mk
-- }
-- @
--
-- The Ledger /ledger state/ contains a UTxO set as well, and with
-- @stowLedgerTables@ and @unstowLedgerTables@ we move those between the Ledger
-- /ledger state/ and the 'LedgerTables', for example:
--
-- @
-- 'unstowLedgerTables' (LedgerState {
--                         theLedgerLedgerState = NewEpochState {
--                             ...
--                           , utxoSet = Map.fromList [(\'a\', 100), (\'b\', 100), ...]
--                         }
--                       , theTables = 'EmptyMK'
--                     })
--  ==
--  LedgerState {
--      theLedgerLedgerState = NewEpochState {
--          ...
--        , utxoSet = Map.empty
--        }
--    , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100), ...])
--    })
-- @
--
-- @
-- 'stowLedgerTables' (LedgerState {
--                       theLedgerLedgerState = NewEpochState {
--                           ...
--                         , utxoSet = Map.empty
--                       }
--                     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100), ...])
--                   })
--  ==
--  LedgerState {
--      theLedgerLedgerState = NewEpochState {
--          ...
--        , utxoSet = Map.fromList [(\'a\', 100), (\'b\', 100), ...]
--        }
--    , theTables = 'EmptyMK'
--    })
-- @
--
-- Using these functions we can extract the data from the Ledger /ledger state/
-- for us Consensus to manipulate, and we can then inject it back so that we
-- provide the expected data to the ledger. Note that the Ledger rules for
-- applying a block are defined in a way that it only needs the subset of the
-- UTxO set that the block being applied will consume.
--
-- Now using 'Ouroboros.Consensus.Ledger.Tables.Utils.calculateDifference', we
-- can compare two (successive) t'Ouroboros.Consensus.Ledger.Basics.LedgerState's
-- to produce differences:
--
-- @
-- 'Ouroboros.Consensus.Ledger.Tables.Utils.calculateDifference'
--   (LedgerState {
--       ...
--     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100)])
--     })
--   (LedgerState {
--       ...
--     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'c\', 200)])
--     })
-- ==
--  'TrackingMK'
--    (Map.fromList [(\'a\', 100),    (\'c\', 200)])
--    (Map.fromList [(\'b\', Delete), (\'c\', Insert 200)])
-- @
--
-- This operation provided a 'TrackingMK' which is in fact just a 'ValuesMK' and
-- 'DiffMK' put together.
--
-- We can then use those differences to /forward/ a collection of values, so for
-- example (taking the example above):
--
-- @
-- let tables1 = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100)])
--     tables2 = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'c\', 200)])
--     diffs = 'Ouroboros.Consensus.Ledger.Tables.Utils.rawForgetTrackingValues'
--           $ 'Ouroboros.Consensus.Ledger.Tables.Utils.rawCalculateDifference' tables1 tables2
-- in
--   'Ouroboros.Consensus.Ledger.Tables.Utils.rawApplyDiffs' tables1 diffs == tables2
-- @
--
-- Note: we usually don't call the @raw*@ methods directly but instead call the
-- corresponding function that operates on
-- t'Ouroboros.Consensus.Ledger.Basics.LedgerState's. See
-- "Ouroboros.Consensus.Ledger.Tables.Utils".
--
-- Also when applying a block that contains some transactions, we can produce
-- 'LedgerTable's of @KeysMK@, by gathering the txins required by the
-- transactions:
--
-- @
-- 'Ouroboros.Consensus.Ledger.Abstract.getBlockKeySets' (Block {..., txs = [Tx { input = [\'a\', \'b\'], outputs = [\'c\', \'d\'] }]})
--  == 'KeysMK' (Set.fromList [\'a\', \'b\'])
-- @
--
-- We shall use those later on to read the txouts from some storage.
--
-- We call those types ending in \"MK\" mapkinds. They model the different types
-- of collections and contained data in the tables. This example already covered
-- most of the standard mapkinds, in particular:
--
--   ['EmptyMK']: A nullary data constructor, an empty table.
--
--   ['ValuesMK']: Contains a @Data.Map@ from txin to txouts.
--
--   ['DiffMK']: Contains a @Data.Map@ from txin to a change on the value.
--
--   ['TrackingMK']: Contains both a 'ValuesMK' and 'DiffMK'.
--
--   ['KeysMK']: Contains a @Data.Set@ of txins.
--
--   ['SeqDiffMK']: A fingertree of 'DiffMK's.
module Ouroboros.Consensus.Ledger.Tables
  ( -- * Core
    module Ouroboros.Consensus.Ledger.Tables.Basics
  , module Ouroboros.Consensus.Ledger.Tables.MapKind

    -- * Utilities
  , module Ouroboros.Consensus.Ledger.Tables.Combinators

    -- * Basic LedgerState classes

    -- ** Stowing ledger tables
  , CanStowLedgerTables (..)

    -- ** Extracting and injecting ledger tables
  , HasLedgerTables (..)
  , HasTickedLedgerTables

    -- * Serialization
  , SerializeTablesHint
  , SerializeTablesWithHint (..)
  , defaultDecodeTablesWithHint
  , defaultEncodeTablesWithHint
  , valuesMKDecoder
  , valuesMKEncoder

    -- * Special classes
  , LedgerTablesAreTrivial
  , TrivialLedgerTables (..)
  , convertMapKind
  , trivialLedgerTables
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Void (Void)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Ledger.Tables.Basics
import Ouroboros.Consensus.Ledger.Tables.Combinators
import Ouroboros.Consensus.Ledger.Tables.MapKind
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

-- | Extracting @'LedgerTables'@ from @l mk@ (which will share the same @mk@),
-- or replacing the @'LedgerTables'@ associated to a particular @l@.
type HasLedgerTables :: LedgerStateKind -> Constraint
class
  ( Ord (TxIn l)
  , Eq (TxOut l)
  , Show (TxIn l)
  , Show (TxOut l)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  , MemPack (TxIn l)
  , IndexedMemPack (MemPackIdx l EmptyMK) (TxOut l)
  ) =>
  HasLedgerTables l
  where
  -- | Extract the ledger tables from a ledger state
  --
  -- The constraints on @mk@ are necessary because the 'CardanoBlock' instance
  -- uses them.
  projectLedgerTables ::
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    l mk ->
    LedgerTables l mk

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
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    l any ->
    LedgerTables l mk ->
    l mk

instance
  ( Ord (TxIn l)
  , Eq (TxOut l)
  , Show (TxIn l)
  , Show (TxOut l)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  , MemPack (TxIn l)
  , IndexedMemPack (MemPackIdx l EmptyMK) (TxOut l)
  ) =>
  HasLedgerTables (LedgerTables l)
  where
  projectLedgerTables = castLedgerTables
  withLedgerTables _ = castLedgerTables

-- | Convenience class, useful for partially applying the composition of
-- 'HasLedgerTables' and 'Ticked'.
type HasTickedLedgerTables :: LedgerStateKind -> Constraint
class HasLedgerTables (Ticked l) => HasTickedLedgerTables l

instance HasLedgerTables (Ticked l) => HasTickedLedgerTables l

-- | LedgerTables are projections of data from a LedgerState and as such they
-- can be injected back into a LedgerState. This is necessary because the Ledger
-- rules are currently unaware of UTxO-HD changes. Thus, by stowing the ledger
-- tables, we are able to provide a Ledger State with a restricted UTxO set that
-- is enough to execute the Ledger rules.
--
-- In particular, HardForkBlock LedgerStates are never given diretly to the
-- ledger but rather unwrapped and then it is the inner ledger state the one we
-- give to the ledger. This means that all the single era blocks must be an
-- instance of this class, but HardForkBlocks might avoid doing so.
type CanStowLedgerTables :: LedgerStateKind -> Constraint
class CanStowLedgerTables l where
  stowLedgerTables :: l ValuesMK -> l EmptyMK
  unstowLedgerTables :: l EmptyMK -> l ValuesMK

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
  forall l.
  SerializeTablesWithHint l =>
  l EmptyMK ->
  LedgerTables l ValuesMK ->
  CBOR.Encoding
valuesMKEncoder st tbs =
  CBOR.encodeListLen 1 <> encodeTablesWithHint st tbs

-- | Default decoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKDecoder ::
  forall l s.
  SerializeTablesWithHint l =>
  l EmptyMK ->
  CBOR.Decoder s (LedgerTables l ValuesMK)
valuesMKDecoder st =
  CBOR.decodeListLenOf 1 >> decodeTablesWithHint st

-- | When decoding the tables and in particular the UTxO set we want
-- to share data in the TxOuts in the same way the Ledger did (see the
-- @Share (TxOut era)@ instances). We need to provide the state in the
-- HFC case so that we can call 'eraDecoder' and also to extract the
-- interns from the state.
--
-- As we will decode with 'eraDecoder' we also need to use such era
-- for the encoding thus we need the hint also in the encoding.
--
-- See @SerializeTablesWithHint (LedgerState (HardForkBlock
-- (CardanoBlock c)))@ for a good example, the rest of the instances
-- are somewhat degenerate.
class SerializeTablesWithHint l where
  encodeTablesWithHint ::
    SerializeTablesHint (LedgerTables l ValuesMK) ->
    LedgerTables l ValuesMK ->
    CBOR.Encoding
  decodeTablesWithHint ::
    SerializeTablesHint (LedgerTables l ValuesMK) ->
    CBOR.Decoder s (LedgerTables l ValuesMK)

-- This is just for the BackingStore Lockstep tests. Once V1 is gone
-- we can inline it above.

-- | The hint for 'SerializeTablesWithHint'
type family SerializeTablesHint values :: Type

type instance SerializeTablesHint (LedgerTables l ValuesMK) = l EmptyMK

defaultEncodeTablesWithHint ::
  (MemPack (TxIn l), MemPack (TxOut l)) =>
  SerializeTablesHint (LedgerTables l ValuesMK) ->
  LedgerTables l ValuesMK ->
  CBOR.Encoding
defaultEncodeTablesWithHint _ (LedgerTables (ValuesMK tbs)) =
  mconcat
    [ CBOR.encodeMapLen (fromIntegral $ Map.size tbs)
    , Map.foldMapWithKey
        ( \k v ->
            mconcat
              [ CBOR.encodeBytes (packByteString k)
              , CBOR.encodeBytes (packByteString v)
              ]
        )
        tbs
    ]

defaultDecodeTablesWithHint ::
  (Ord (TxIn l), MemPack (TxIn l), MemPack (TxOut l)) =>
  SerializeTablesHint (LedgerTables l ValuesMK) ->
  CBOR.Decoder s (LedgerTables l ValuesMK)
defaultDecodeTablesWithHint _ = do
  n <- CBOR.decodeMapLen
  LedgerTables . ValuesMK <$> go n Map.empty
 where
  go 0 m = pure m
  go n !m = do
    (k, v) <- (,) <$> (unpackMonadFail =<< CBOR.decodeBytes) <*> (unpackMonadFail =<< CBOR.decodeBytes)
    go (n - 1) (Map.insert k v m)

{-------------------------------------------------------------------------------
  Special classes of ledger states
-------------------------------------------------------------------------------}

-- | For some ledger states we won't be defining 'LedgerTables' and instead the
-- ledger state will be fully stored in memory, as before UTxO-HD. The ledger
-- states that are defined this way can be made instances of this class which
-- allows for easy manipulation of the types of @mk@ required at any step of the
-- program.
type LedgerTablesAreTrivial :: LedgerStateKind -> Constraint
class (TxIn l ~ Void, TxOut l ~ Void) => LedgerTablesAreTrivial l where
  -- | If the ledger state is always in memory, then @l mk@ will be isomorphic
  -- to @l mk'@ for all @mk@, @mk'@. As a result, we can convert between ledgers
  -- states indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg @applyChainTickLedgerResult@).
  convertMapKind :: l mk -> l mk'

trivialLedgerTables ::
  (ZeroableMK mk, LedgerTablesAreTrivial l) =>
  LedgerTables l mk
trivialLedgerTables = LedgerTables emptyMK

-- | A newtype to @derive via@ the instances for blocks with trivial ledger
-- tables.
type TrivialLedgerTables :: LedgerStateKind -> MapKind -> Type
newtype TrivialLedgerTables l mk = TrivialLedgerTables {untrivialLedgerTables :: l mk}

type instance TxIn (TrivialLedgerTables l) = TxIn l
type instance TxOut (TrivialLedgerTables l) = TxOut l

instance LedgerTablesAreTrivial l => LedgerTablesAreTrivial (TrivialLedgerTables l) where
  convertMapKind = TrivialLedgerTables . convertMapKind . untrivialLedgerTables

instance LedgerTablesAreTrivial l => HasLedgerTables (TrivialLedgerTables l) where
  projectLedgerTables _ = trivialLedgerTables
  withLedgerTables st _ = convertMapKind st

instance LedgerTablesAreTrivial l => CanStowLedgerTables (TrivialLedgerTables l) where
  stowLedgerTables = convertMapKind
  unstowLedgerTables = convertMapKind

instance IndexedMemPack (TrivialLedgerTables l EmptyMK) Void where
  indexedTypeName _ = typeName @Void
  indexedPackedByteCount _ = packedByteCount
  indexedPackM _ = packM
  indexedUnpackM _ = unpackM

instance SerializeTablesWithHint (TrivialLedgerTables l) where
  decodeTablesWithHint _ = do
    _ <- CBOR.decodeMapLen
    pure (LedgerTables $ ValuesMK Map.empty)
  encodeTablesWithHint _ _ = CBOR.encodeMapLen 0
