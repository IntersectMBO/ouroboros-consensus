{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Ouroboros.Consensus.Ledger.SupportsMempool (
    ApplyTxErr
  , ConvertRawTxId (..)
  , GenTx
  , GenTxId
  , HasTxId (..)
  , HasTxs (..)
  , LedgerSupportsMempool (..)
  , TxId
  , TxLimits (..)
  , Validated
  , WhetherToIntervene (..)
  ) where

import           Control.Monad.Except
import           Data.ByteString.Short (ShortByteString)
import           Data.Kind (Type)
import           Data.Measure (BoundedMeasure)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.SizeInBytes (SizeInBytes)

-- | Generalized transaction
--
-- The mempool (and, accordingly, blocks) consist of "generalized
-- transactions"; this could be "proper" transactions (transferring funds) but
-- also other kinds of things such as update proposals, delegations, etc.
data family GenTx blk :: Type

-- | Updating the ledger with a single transaction may result in a different
-- error type as when updating it with a block
type family ApplyTxErr blk :: Type

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
  = DoNotIntervene
    -- ^ We do not trust remote peers, so if a problematic-yet-valid transaction
    -- arrives over NTN, we accept it; it will end up in a block and the ledger
    -- will penalize them for it.
  | Intervene
    -- ^ We trust local clients, so if a problematic-yet-valid transaction
    -- arrives over NTC, we reject it in order to avoid the ledger penalizing
    -- them for it.
    deriving (Show)

class ( UpdateLedger blk
      , TxLimits blk
      , NoThunks (GenTx blk)
      , NoThunks (Validated (GenTx blk))
      , NoThunks (Ticked (LedgerState blk))
      , NoThunks (TxMeasure blk)
      , Show (GenTx blk)
      , Show (Validated (GenTx blk))
      , Show (ApplyTxErr blk)
      ) => LedgerSupportsMempool blk where

  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

  -- | Apply an unvalidated transaction
  --
  -- The mempool expects that the ledger checks the sanity of the transaction'
  -- size. The mempool implementation will add any valid transaction as long as
  -- there is at least one byte free in the mempool.
  applyTx :: LedgerConfig blk
          -> WhetherToIntervene
          -> SlotNo -- ^ Slot number of the block containing the tx
          -> GenTx blk
          -> TickedLedgerState blk
          -> Except (ApplyTxErr blk) (TickedLedgerState blk, Validated (GenTx blk))

  -- | Apply a previously validated transaction to a potentially different
  -- ledger state
  --
  -- When we re-apply a transaction to a potentially different ledger state
  -- expensive checks such as cryptographic hashes can be skipped, but other
  -- checks (such as checking for double spending) must still be done.
  reapplyTx :: HasCallStack
            => LedgerConfig blk
            -> SlotNo -- ^ Slot number of the block containing the tx
            -> Validated (GenTx blk)
            -> TickedLedgerState blk
            -> Except (ApplyTxErr blk) (TickedLedgerState blk)

  -- | Discard the evidence that transaction has been previously validated
  txForgetValidated :: Validated (GenTx blk) -> GenTx blk

-- | A generalized transaction, 'GenTx', identifier.
data family TxId tx :: Type

-- | Transactions with an identifier
--
-- The mempool will use these to locate transactions, so two different
-- transactions should have different identifiers.
class ( Show     (TxId tx)
      , Ord      (TxId tx)
      , NoThunks (TxId tx)
      ) => HasTxId tx where

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

-- | Each block has its limits of how many transactions it can hold.
-- That limit is compared against the sum of measurements
-- taken of each of the transactions in that block.
--
-- How we measure the transaction depends of the era that this
-- transaction belongs to (more specifically it depends on the block
-- type to which this transaction will be added). For initial eras
-- (like Byron and initial generations of Shelley based eras) this
-- measure was simply a ByteSize (block could not be bigger then
-- given size - in bytes - specified by the ledger state). In future
-- eras (starting with Alonzo) this measure was a bit more complex
-- as it had to take other factors into account (like execution units).
-- For details please see the individual instances for the TxLimits.
class ( BoundedMeasure (TxMeasure blk)
      , NoThunks       (TxMeasure blk)
      ) => TxLimits blk where
  type TxMeasure blk

  -- | The transaction capacity of a block according to the currently adopted
  -- protocol parameters of the ledger state.
  --
  -- Notably, the byte size component of this measure must be (conservatively)
  -- computed by subtracting the header size and any other fixed overheads from
  -- the maximum block size.
  blockTxCapacity ::
       LedgerConfig blk   -- ^ used at least by HFC's composition logic
    -> TickedLedgerState blk
    -> TxMeasure blk

  -- | Return the size of a 'GenTx' /when it is embedded in a block/.
  --
  -- Notably, the byte size component of this measure should be the
  -- post-serialisation size in bytes of a 'GenTx'. Also note that the codec
  -- and so the byte size of a transaction might different when it's in a block
  -- versus when it's sent and received as part of @TxSubmission2@.
  --
  -- For example, CBOR-in-CBOR could be used when sending the transaction
  -- across the network, requiring a few extra bytes compared to the actual
  -- in-block serialisation. Another example is the transaction of the
  -- hard-fork combinator which will include an envelope indicating its era
  -- when sent across the network. However, when embedded in the respective
  -- era's block, there is no need for such envelope.
  --
  -- Can be implemented by serialising the 'GenTx', but, ideally, this is
  -- implement more efficiently. E.g., by returning the length of the
  -- annotation.
  --
  -- This may depend on the ledger state, /HOWEVER/: the tx should have the
  -- same size in any ledger state in which it is valid.
  txInBlockSize ::
       LedgerConfig blk   -- ^ used at least by HFC's composition logic
    -> TickedLedgerState blk
    -> GenTx blk
    -> TxMeasure blk

  -- | Project the byte size component of the transaction measure.
  txMeasureBytes :: proxy blk -> TxMeasure blk -> SizeInBytes
