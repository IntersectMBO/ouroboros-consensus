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
-- the memory usage of the Consensus layer. Ledger tables are used in the
-- 'Ouroboros.Consensus.Storage.LedgerDB.BackingStore' and
-- 'Ouroboros.Consensus.Storage.LedgerDB.DbChangelog' modules.
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
-- UTxO set that the block being applied will consume. See [the @DbChangelog@
-- documentation for block
-- application](Ouroboros-Consensus-Storage-LedgerDB-DbChangelog.html#g:applying).
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
--    (Map.fromList [(\'a\', 100),      (\'c\', 200)])
--    (Map.fromList [(\'b\', [Delete]), (\'c\', [Insert 200])])
-- @
--
-- This operation provided a 'TrackingMK' which is in fact just a 'ValuesMK' and
-- 'DiffMK' put together.
--
-- We can then use those differences to /forward/ a set of values, so for
-- example (taking the example above):
--
-- @
-- let state1 = LedgerState {
--                 ...
--               , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100)])
--              }
--     state2 = LedgerState {
--                ...
--              , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'c\', 200)])
--              }
--     state3 = LedgerState {
--                ...
--              , theTables = 'ValuesMK' (Map.fromList [])
--              }
-- in
--   'Ouroboros.Consensus.Ledger.Tables.Utils.applyDiffs' state3 ('Ouroboros.Consensus.Ledger.Tables.Utils.forgetTrackingValues' $ 'Ouroboros.Consensus.Ledger.Tables.Utils.calculateDifference' state1 state2)
-- ==
--  LedgerState {
--      ...
--    , theTables = 'ValuesMK' (Map.fromList [(\'c\', 200)])
--    }
-- @
--
-- Notice that we produced differences for @\'b\'@ and @\'c\'@, but as the input
-- state (@state3@) didn't contain @\'b\'@ the only difference that was applied
-- was the one of @\'c\'@.
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
-- We shall use those later on to read the txouts from some storage (which will
-- be the 'Ouroboros.Consensus.Storage.LedgerDB.BackingStore.BackingStore') and
-- forward the resulting txouts through a sequence of differences (which will be
-- 'Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.adcDiffs').
--
-- This example already covered most of the standard mapkinds, in particular:
--
--   ['EmptyMK']: A nullary data constructor, an empty table.
--
--   ['ValuesMK']: Contains a @Data.Map@ from txin to txouts.
--
--   ['DiffMK']: Contains a @Data.Map@ from txin to history of changes (see
--     "Data.Map.Diff.Strict").
--
--   ['TrackingMK']: Contains both a 'ValuesMK' and 'DiffMK'.
--
--   ['KeysMK']: Contains a @Data.Set@ of txins.
--
--   ['SeqDiffMK']: A fingertree of 'DiffMK's.
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
       (CanMapMK mk, CanMapKeysMK mk, CanMapMaybeMK mk, ZeroableMK mk)
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
--
-- TODO: can this be removed in favour of EncodeDisk and DecodeDisk?
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

-- | For some ledger states we won't be defining 'LedgerTables' and instead the
-- ledger state will be fully stored in memory, as before UTxO-HD. The ledger
-- states that are defined this way can be made instances of this class which
-- allows for easy manipulation of the types of @mk@ required at any step of the
-- program.
type LedgerTablesAreTrivial :: LedgerStateKind -> Constraint
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
