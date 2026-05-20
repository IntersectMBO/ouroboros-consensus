{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , ByteSize32 (..)
  , ConvertRawTxId (..)
  , GenTx
  , GenTxId
  , HasByteSize (..)
  , HasTxId (..)
  , HasTxs (..)
  , IgnoringOverflow (..)
  , Invalidated (..)
  , LedgerSupportsMempool (..)
  , TxId
  , TxLimits (..)
  , TxMeasureMetrics (..)
  , Validated
  , WhetherToIntervene (..)
  , nothingMkMempoolApplyTxError
  ) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
import Data.DerivingVia (InstantiatedAt (..))
import Data.Kind (Type)
import Data.Measure (Measure)
import qualified Data.Measure
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Stack (HasCallStack)
import NoThunks.Class
import Numeric.Natural
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Network.SizeInBytes as Network

-- | Generalized transaction
--
-- The mempool (and, accordingly, blocks) consist of "generalized
-- transactions"; this could be "proper" transactions (transferring funds) but
-- also other kinds of things such as update proposals, delegations, etc.
type GenTx :: Type -> Type
data family GenTx blk

-- | Updating the ledger with a single transaction may result in a different
-- error type as when updating it with a block
type ApplyTxErr :: Type -> Type
type family ApplyTxErr blk

-- | A flag indicating whether the mempool should reject a valid-but-problematic
-- transaction, in order to to protect its author from penalties etc
--
-- The primary example is that, as of the Alonzo ledger, a valid transaction can
-- carry an invalid script. If a remote peer sends us such a transaction (over a
-- Node-to-Node protocol), we include it in a block so that the ledger will
-- penalize them them for the invalid script: they wasted our resources by
-- forcing us to run the script to determine it's invalid. But if our local
-- wallet -- which we trust by assumption -- sends us such a transaction (over a
-- Node-to-Client protocol), we would be a good neighbor by rejecting that
-- transaction: they must have made some sort of mistake, and we don't want the
-- ledger to penalize them.
data WhetherToIntervene
  = -- | We do not trust remote peers, so if a problematic-yet-valid transaction
    -- arrives over NTN, we accept it; it will end up in a block and the ledger
    -- will penalize them for it.
    DoNotIntervene
  | -- | We trust local clients, so if a problematic-yet-valid transaction
    -- arrives over NTC, we reject it in order to avoid the ledger penalizing
    -- them for it.
    Intervene
  deriving Show

-- $note-hfc-mempool
--
-- == Note [Mempool no longer manages tables across HFC boundaries]
--
-- Previous versions of this module exposed @reapplyTxs@ (a batched
-- revalidation method with a default fold), @prependMempoolDiffs@ /
-- @applyMempoolDiffs@ (HFC-specific optimization hooks for re-projecting
-- diffs cheaply across eras), a 'WhatToDoWithTxDiffs' kind plus
-- @InputTxDiffs@ family (to type-level distinguish collect-vs-discard
-- revalidation), and @getTransactionKeySets@ (to pre-compute UTxO keys
-- needed by a tx). All of those have been removed.
--
-- Rationale: tx-induced UTxO diffs are now buffered inside the
-- per-block 'MempoolCache' rather than projected through HFC layers,
-- which makes the optimization hooks unnecessary; key-set computation is
-- internal to each block's 'applyTx' and lives in the Shelley instance
-- (the only block type that maintains tables). The batched
-- @reapplyTxs@ is now just a plain fold of 'reapplyTx' at the call
-- site.

class
  ( UpdateLedger blk
  , TxLimits blk
  , NoThunks (GenTx blk)
  , NoThunks (Validated (GenTx blk))
  , Show (GenTx blk)
  , Show (Validated (GenTx blk))
  , Show (ApplyTxErr blk)
  ) =>
  LedgerSupportsMempool blk
  where
  -- | Per-block mempool read cache.
  --
  -- Morally a snapshot of the UTxO entries needed by the mempool's
  -- current transactions, plus the diffs each transaction introduces.
  -- Keeping these values cached lets 'applyTx' / 'reapplyTx' validate
  -- each tx without round-tripping to disk.
  --
  -- This is a /pure/ value: the cache is not parameterised by @m@
  -- because the values it holds are already materialised. For the
  -- HFC instance the cache is era-aware (an @NS MempoolCache xs@-shaped
  -- thing) and gets re-initialised when the mempool crosses an era
  -- boundary.
  --
  -- TODO @js: pin down the cache lifecycle precisely (when is it constructed,
  -- when is it updated, when is it discarded).
  data MempoolCache blk

  -- | Build an initial 'MempoolCache' from a ticked state handle.
  --
  -- The state handle is required (rather than e.g. a unit-shaped
  -- factory) because the HFC instance needs the current era tag to
  -- position the @NS@-shaped cache.
  mkMempoolCache :: TickedStateHandle m blk -> MempoolCache blk

  -- | Read into the cache the UTxO values the given (unvalidated)
  -- transaction needs for validation/measurement.
  --
  -- This is the only monadic entry point that touches the on-disk
  -- tables for a given tx. Once it has run, all later operations on
  -- this tx ('txMeasure', 'applyTx', 'reapplyTx') can be /pure/: they
  -- look up values in the cache, never in the handle's
  -- 'LedgerTablesHandle'.
  --
  -- Idempotent: values already present in the cache are not re-read.
  addToCache ::
    Monad m =>
    TickedStateHandle m blk ->
    GenTx blk ->
    MempoolCache blk ->
    m (MempoolCache blk)

  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

  -- | Drop a transaction's contribution from the 'MempoolCache'.
  --
  -- Used when the mempool drops a tx without going through
  -- 'reapplyTx' (e.g. manual removal). The resulting cache has the
  -- tx's read values and diffs forgotten, so subsequent 'reapplyTx'
  -- calls on the remaining txs see the correct prefix-state.
  forgetTxFromCache :: Validated (GenTx blk) -> MempoolCache blk -> MempoolCache blk

  -- | Compute the measure of a transaction (e.g. size, ExUnits).
  --
  -- Pure: reads any required UTxO values from the cache (which the
  -- caller is expected to have populated for this tx via 'addToCache').
  --
  -- INVARIANT @Right x = txMeasure cfg cache tx@ implies
  -- @x 'Measure.<=' 'blockCapacityTxMeasure' cfg st@. Otherwise, the
  -- mempool could block forever.
  --
  -- Returns an exception if and only if the transaction violates the
  -- per-tx limits.
  txMeasure ::
    LedgerConfig blk ->
    MempoolCache blk ->
    GenTx blk ->
    Except (ApplyTxErr blk) (TxMeasure blk)

  -- | Apply an unvalidated transaction.
  --
  -- The mempool expects that the ledger checks the sanity of the
  -- transaction's size. The mempool implementation will add any valid
  -- transaction as long as there is at least one byte free in the
  -- mempool.
  --
  -- Pure: the values needed by this tx are expected to be in the
  -- 'MempoolCache' already (the caller is responsible for calling
  -- 'addToCache' first). The returned handle has the in-memory pure
  -- state updated via 'withTickedState' from 'MonadLedger' — it
  -- shares the underlying 'LedgerTablesHandle' with the input (no
  -- writes to tables; the UTxO diffs introduced by mempool txs are
  -- buffered in the 'MempoolCache' instead). The input handle is
  -- consumed; callers must use the returned one.
  --
  -- The returned cache contains the new tx's diff appended.
  --
  -- On error the cache is /not/ returned: the caller still holds the
  -- input cache and the failed tx is simply discarded.
  applyTx ::
    MonadLedger m blk =>
    LedgerConfig blk ->
    WhetherToIntervene ->
    -- | Slot number of the block containing the tx
    SlotNo ->
    GenTx blk ->
    MempoolCache blk ->
    TickedStateHandle m blk ->
    Except
      (ApplyTxErr blk)
      (TickedStateHandle m blk, MempoolCache blk, Validated (GenTx blk))

  -- | Apply a previously validated transaction to a potentially different
  -- ledger state.
  --
  -- When we re-apply a transaction to a potentially different ledger
  -- state, expensive checks such as cryptographic hashes can be skipped,
  -- but other checks (such as checking for double spending) must still
  -- be done.
  --
  -- Pure, with the same value-availability precondition as 'applyTx':
  -- callers must have populated the cache for the tx (via 'addToCache'
  -- on @'txForgetValidated' vtx@) before calling this.
  --
  -- The handle update semantics are the same as 'applyTx': the returned
  -- handle has its pure state updated via 'withTickedState', sharing
  -- the underlying 'LedgerTablesHandle' with the input.
  --
  -- The 'MempoolCache' is threaded through both the success and failure
  -- cases — but its content differs:
  --
  -- * On success: the cache is updated with the tx's re-derived diff.
  -- * On error: the failing tx is being evicted from the mempool, so the
  --   cache returned in the error has the tx's prior contributions
  --   /removed/. The caller can plug this cache straight back into the
  --   next 'reapplyTx' in the fold without recomputing it.
  reapplyTx ::
    (HasCallStack, MonadLedger m blk) =>
    LedgerConfig blk ->
    -- | Slot number of the block containing the tx
    SlotNo ->
    Validated (GenTx blk) ->
    MempoolCache blk ->
    TickedStateHandle m blk ->
    Except
      (ApplyTxErr blk, MempoolCache blk)
      (TickedStateHandle m blk, MempoolCache blk)

  -- | Discard the evidence that transaction has been previously validated
  txForgetValidated :: Validated (GenTx blk) -> GenTx blk

  -- | The ledger rules' error type for the mempool's current era might allow
  -- the mempool to reject a tx for its own reasons.
  --
  -- This function therefore constructs the type that the @LocalTxSubmission@
  -- node-to-client mini protocol sends when a tx is rejected.
  mkMempoolApplyTxError ::
    -- | for the HFC
    TickedLedgerState blk ->
    Text ->
    Maybe (ApplyTxErr blk)

-- | Value of 'mkMempoolApplyTxError' when the block type can never
-- construct the ledger error
nothingMkMempoolApplyTxError :: TickedLedgerState blk -> Text -> Maybe (ApplyTxErr blk)
nothingMkMempoolApplyTxError _ _ = Nothing

-- | A generalized transaction, 'GenTx', identifier.
type TxId :: Type -> Type
data family TxId blk

-- | Transactions with an identifier
--
-- The mempool will use these to locate transactions, so two different
-- transactions should have different identifiers.
class
  ( Show (TxId tx)
  , Ord (TxId tx)
  , NoThunks (TxId tx)
  ) =>
  HasTxId tx
  where
  -- | Return the 'TxId' of a 'GenTx'.
  --
  -- NOTE: a 'TxId' must be unique up to ledger rules, i.e., two 'GenTx's with
  -- the same 'TxId' must be the same transaction /according to the ledger/.
  -- However, we do not assume that a 'TxId' uniquely determines a 'GenTx':
  -- two 'GenTx's with the same 'TxId' can differ in, e.g., witnesses.
  --
  -- Should be cheap as this will be called often.
  txId :: tx -> TxId tx

-- | Extract the raw hash bytes from a 'TxId'.
class HasTxId tx => ConvertRawTxId tx where
  -- | NOTE: The composition @'toRawTxIdHash' . 'txId'@ must satisfy the same
  -- properties as defined in the docs of 'txId'.
  toRawTxIdHash :: TxId tx -> ShortByteString

-- | Shorthand: ID of a generalized transaction
type GenTxId blk = TxId (GenTx blk)

-- | Collect all transactions from a block
--
-- This is used for tooling only. We don't require it as part of RunNode
-- (and cannot, because we cannot give an instance for the dual ledger).
class HasTxs blk where
  -- | Return the transactions part of the given block in no particular order.
  extractTxs :: blk -> [GenTx blk]

{-------------------------------------------------------------------------------
  Tx sizes
-------------------------------------------------------------------------------}

-- | Each block has its limits of how many transactions it can hold. That limit
-- is compared against the sum of measurements taken of each of the
-- transactions in that block.
--
-- How we measure the transaction depends of the era that this transaction
-- belongs to (more specifically it depends on the block type to which this
-- transaction will be added). For initial eras (like Byron and initial
-- generations of Shelley based eras) this measure was simply a byte size
-- (block could not be bigger then given size - in bytes - specified by the
-- ledger state). In subsequent eras (starting with Alonzo) this measure was a
-- bit more complex as it had to take other factors into account (like
-- execution units). For details please see the individual instances for the
-- TxLimits.
class
  ( Measure (TxMeasure blk)
  , HasByteSize (TxMeasure blk)
  , NoThunks (TxMeasure blk)
  , TxMeasureMetrics (TxMeasure blk)
  , Show (TxMeasure blk)
  ) =>
  TxLimits blk
  where
  -- | The (possibly multi-dimensional) size of a transaction in a block.
  type TxMeasure blk

  -- | The size of the transaction from the perspective of diffusion layer
  txWireSize :: GenTx blk -> Network.SizeInBytes

  -- | The various sizes (bytes, Plutus script ExUnits, etc) of a tx /when it's
  -- in a block/
  --
  -- This size is used to compute how many transaction we can put in a block
  -- when forging one.
  --
  -- The byte size component in particular might differ from the size of the
  -- serialisation used to send and receive the transaction across the network.
  -- For example, CBOR-in-CBOR could be used when sending the transaction
  -- across the network, requiring a few extra bytes compared to the actual
  -- in-block serialisation. Another example is the transaction of the
  -- hard-fork combinator which will include an envelope indicating its era
  -- when sent across the network. However, when embedded in the respective
  -- era's block, there is no need for such envelope. An example from upstream
  -- is that the Cardano ledger's "Segregated Witness" encoding scheme
  -- contributes to the encoding overhead.
  --
  -- (The per-tx measure 'txMeasure' has been moved to
  -- 'LedgerSupportsMempool' so it can read UTxO values via the mempool
  -- cache and ticked state handle.)

  -- | What is the allowed capacity for the txs in an individual block?
  blockCapacityTxMeasure ::
    -- | at least for symmetry with 'txMeasure'
    LedgerConfig blk ->
    TickedLedgerState blk ->
    TxMeasure blk

-- | We intentionally do not declare a 'Num' instance! We prefer @ByteSize32@
-- to occur explicitly in the code where possible, for
-- legibility/perspicuousness. We also do not need nor want subtraction.
--
-- This data type measures the size of a transaction, the sum of the sizes of
-- txs in a block, the sum of the sizes of the txs in the mempool, etc. None of
-- those will ever need to represent gigabytes, so 32 bits suffice. But 16 bits
-- would not.
--
-- This is modular arithmetic, so uses need to be concerned with overflow. For
-- example, see the related guard in
-- 'Ouroboros.Consensus.Mempool.Update.pureTryAddTx'. One important element is
-- anticipating the possibility of very large summands injected by the
-- adversary.
--
-- There is a temptation to use 'Natural' here, since it can never overflow.
-- However, some points in the interface do not easily handle 'Natural's, such
-- as encoders. Thus 'Natural' would merely defer the overflow concern, and
-- even risks instilling a false sense that overflow need not be considered at
-- all.
newtype ByteSize32 = ByteSize32 {unByteSize32 :: Word32}
  deriving stock Show
  deriving newtype (Eq, Ord)
  deriving newtype NFData
  deriving newtype Serialise
  deriving
    (Monoid, Semigroup)
    via (InstantiatedAt Measure (IgnoringOverflow ByteSize32))
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "ByteSize" ByteSize32

-- | @'IgnoringOverflow' a@ has the same semantics as @a@, except it ignores
-- the fact that @a@ can overflow.
--
-- For example, @'Measure' 'Word32'@ is not lawful, because overflow violates
-- the /lattice-ordered monoid/ law. But @'Measure' (IgnoringOverflow
-- 'Word32')@ is lawful, since it explicitly ignores that case.
--
-- WARNING: anywhere this type occurs is a very strong indicator that overflow
-- will break assumptions, so overflow must therefore be guarded against.
--
-- TODO upstream this to the @measure@ package
newtype IgnoringOverflow a = IgnoringOverflow {unIgnoringOverflow :: a}
  deriving stock Show
  deriving newtype (Eq, Ord)
  deriving newtype NFData
  deriving newtype (Monoid, Semigroup)
  deriving newtype NoThunks
  deriving newtype HasByteSize
  deriving newtype TxMeasureMetrics

instance Measure (IgnoringOverflow ByteSize32) where
  zero = coerce (0 :: Word32)
  plus = coerce $ (+) @Word32
  min = coerce $ min @Word32
  max = coerce $ max @Word32

class HasByteSize a where
  -- | The byte size component (of 'TxMeasure')
  txMeasureByteSize :: a -> ByteSize32

instance HasByteSize ByteSize32 where
  txMeasureByteSize = id

class TxMeasureMetrics msr where
  txMeasureMetricTxSizeBytes :: msr -> ByteSize32
  txMeasureMetricExUnitsMemory :: msr -> Natural
  txMeasureMetricExUnitsSteps :: msr -> Natural
  txMeasureMetricRefScriptsSizeBytes :: msr -> ByteSize32

instance TxMeasureMetrics ByteSize32 where
  txMeasureMetricTxSizeBytes = id
  txMeasureMetricExUnitsMemory _ = 0
  txMeasureMetricExUnitsSteps _ = 0
  txMeasureMetricRefScriptsSizeBytes _ = mempty

-- | A transaction that was previously valid. Used to clarify the types on the
-- 'reapplyTxs' function.
data Invalidated blk = Invalidated
  { getInvalidated :: !(Validated (GenTx blk))
  , getReason :: !(ApplyTxErr blk)
  }
