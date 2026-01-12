{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , ByteSize32 (..)
  , ComputeDiffs (..)
  , ConvertRawTxId (..)
  , GenTx
  , GenTxId
  , HasByteSize (..)
  , HasTxId (..)
  , HasTxs (..)
  , IgnoringOverflow (..)
  , Invalidated (..)
  , LedgerSupportsMempool (..)
  , ReapplyTxsResult (..)
  , TxId
  , TxLimits (..)
  , TxMeasureMetrics (..)
  , Validated
  , WhetherToIntervene (..)
  , nothingMkMempoolPredicateFailure
  ) where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
import Data.DerivingVia (InstantiatedAt (..))
import qualified Data.Foldable as Foldable
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
import Ouroboros.Consensus.Ledger.Tables.Utils

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

-- | Whether to keep track of the diffs produced by applying the transactions.
--
-- When getting a mempool snapshot, we will revalidate all the
-- transactions but we won't do anything useful with the resulting
-- state. We can safely omit computing the differences in this case.
--
-- This optimization is worthwile as snapshotting is in the critical
-- path of block minting, and we don't make use of the resulting
-- state, only of the transactions that remain valid.
--
-- Eventually, the Ledger rules will construct the differences for us,
-- so this optimization will no longer be needed. That's why we chose
-- to go with a boolean isomorph instead of something fancier.
data ComputeDiffs
  = -- | This option should be used when syncing the mempool with the
    -- LedgerDB, to store a useful state in the mempool.
    ComputeDiffs
  | -- | This option should be used only when snapshotting the mempool,
    -- as we discard the resulting state anyways.
    IgnoreDiffs
  deriving Show

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
  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

  -- | Apply an unvalidated transaction
  --
  -- The mempool expects that the ledger checks the sanity of the transaction'
  -- size. The mempool implementation will add any valid transaction as long as
  -- there is at least one byte free in the mempool.
  --
  -- The resulting ledger state contains the diffs produced by applying this
  -- transaction alone.
  applyTx ::
    LedgerConfig blk ->
    WhetherToIntervene ->
    -- | Slot number of the block containing the tx
    SlotNo ->
    GenTx blk ->
    -- | Contain only the values for the tx to apply
    TickedLedgerState blk ValuesMK ->
    Except (ApplyTxErr blk) (TickedLedgerState blk DiffMK, Validated (GenTx blk))

  -- | Apply a previously validated transaction to a potentially different
  -- ledger state
  --
  -- When we re-apply a transaction to a potentially different ledger state
  -- expensive checks such as cryptographic hashes can be skipped, but other
  -- checks (such as checking for double spending) must still be done.
  --
  -- The returned ledger state contains the resulting values too so that this
  -- function can be used to reapply a list of transactions, providing as a
  -- first state one that contains the values for all the transactions.
  reapplyTx ::
    HasCallStack =>
    ComputeDiffs ->
    LedgerConfig blk ->
    -- | Slot number of the block containing the tx
    SlotNo ->
    Validated (GenTx blk) ->
    -- | Contains at least the values for the tx to reapply
    TickedLedgerState blk ValuesMK ->
    Except (ApplyTxErr blk) (TickedLedgerState blk TrackingMK)

  -- | Apply a list of previously validated transactions to a new ledger state.
  --
  -- It is never the case that we reapply one single transaction, we always
  -- reapply a list of transactions (and even one transaction can just be lifted
  -- into the unary list).
  --
  -- When reapplying a list of transactions, in the hard-fork instance we want
  -- to first project everything into the particular block instance and then we
  -- can inject/project the ledger tables only once. For single era blocks, this
  -- is by default implemented as a fold using 'reapplyTx'.
  --
  -- Notice: It is crucial that the list of validated transactions returned is
  -- in the same order as they were given, as we will use those later on to
  -- filter a list of 'TxTicket's.
  reapplyTxs ::
    ComputeDiffs ->
    LedgerConfig blk ->
    -- | Slot number of the block containing the tx
    SlotNo ->
    [(Validated (GenTx blk), extra)] ->
    TickedLedgerState blk ValuesMK ->
    ReapplyTxsResult extra blk
  reapplyTxs doDiffs cfg slot txs st =
    ( \(err, val, st') ->
        ReapplyTxsResult
          err
          (reverse val)
          st'
    )
      $ Foldable.foldl'
        ( \(accE, accV, st') (tx, extra) ->
            case runExcept (reapplyTx doDiffs cfg slot tx $ trackingToValues st') of
              Left err -> (Invalidated tx err : accE, accV, st')
              Right st'' ->
                ( accE
                , (tx, extra) : accV
                , case doDiffs of
                    ComputeDiffs -> prependTrackingDiffs st' st''
                    IgnoreDiffs -> st''
                )
        )
        ([], [], attachEmptyDiffs st)
        txs

  -- | Discard the evidence that transaction has been previously validated
  txForgetValidated :: Validated (GenTx blk) -> GenTx blk

  -- | Given a transaction, get the key-sets that we need to apply it to a
  -- ledger state. This is implemented in the Ledger. An example of non-obvious
  -- needed keys in Cardano are those of reference scripts for computing the
  -- transaction size.
  getTransactionKeySets :: GenTx blk -> LedgerTables (LedgerState blk) KeysMK

  -- Mempools live in a single slot so in the hard fork block case
  -- it is cheaper to perform these operations on LedgerStates, saving
  -- the time of projecting and injecting ledger tables.
  --
  -- The cost of this when adding transactions is very small compared
  -- to eg the networking costs of mempool synchronization, but still
  -- it is worthwile locking the mempool for as short as possible.
  --
  -- Eventually the Ledger will provide these diffs, so we might even
  -- be able to remove this optimization altogether.

  -- | Prepend diffs on ledger states
  prependMempoolDiffs ::
    TickedLedgerState blk DiffMK ->
    TickedLedgerState blk DiffMK ->
    TickedLedgerState blk DiffMK
  prependMempoolDiffs = prependDiffs

  -- | Apply diffs on ledger states
  applyMempoolDiffs ::
    LedgerTables (LedgerState blk) ValuesMK ->
    LedgerTables (LedgerState blk) KeysMK ->
    TickedLedgerState blk DiffMK ->
    TickedLedgerState blk ValuesMK
  applyMempoolDiffs = applyDiffForKeysOnTables

  -- | The ledger rules' error type for the mempool's current era might allow
  -- the mempool to reject a tx for its own reasons.
  --
  -- This function therefore constructs the type that the @LocalTxSubmission@
  -- node-to-client mini protocol sends when a tx is rejected.
  mkMempoolPredicateFailure ::
    TickedLedgerState blk mk ->   -- ^ for the HFC
    Text ->
    Maybe (ApplyTxErr blk)

-- | Value of 'mkMempoolPredicateFailure' when the block type can never
-- construct the ledger error
nothingMkMempoolPredicateFailure :: TickedLedgerState blk mk -> Text -> Maybe (ApplyTxErr blk)
nothingMkMempoolPredicateFailure _ _ = Nothing

data ReapplyTxsResult extra blk
  = ReapplyTxsResult
  { invalidatedTxs :: ![Invalidated blk]
  -- ^ txs that are now invalid. Order doesn't matter
  , validatedTxs :: ![(Validated (GenTx blk), extra)]
  -- ^ txs that are valid again, order must be the same as the order in
  -- which txs were received
  , resultingState :: !(TickedLedgerState blk TrackingMK)
  -- ^ Resulting ledger state
  }

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
  -- INVARIANT Assuming no hash collisions, the size should be the same in any
  -- state in which the transaction is valid. For example, it's acceptable to
  -- simply omit the size of ref scripts that could not be found, since their
  -- absence implies the tx is invalid. In fact, that invalidity could be
  -- reported by this function, but it need not be.
  --
  -- INVARIANT @Right x = txMeasure cfg st tx@ implies @x 'Measure.<='
  -- 'blockCapacityTxMeasure cfg st'. Otherwise, the mempool could block
  -- forever.
  --
  -- Returns an exception if and only if the transaction violates the per-tx
  -- limits.
  txMeasure ::
    -- | used at least by HFC's composition logic
    LedgerConfig blk ->
    -- | This state needs values as a transaction measure might depend on
    -- those. For example in Cardano they look at the reference scripts.
    TickedLedgerState blk ValuesMK ->
    GenTx blk ->
    Except (ApplyTxErr blk) (TxMeasure blk)

  -- | What is the allowed capacity for the txs in an individual block?
  blockCapacityTxMeasure ::
    -- | at least for symmetry with 'txMeasure'
    LedgerConfig blk ->
    TickedLedgerState blk mk ->
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
