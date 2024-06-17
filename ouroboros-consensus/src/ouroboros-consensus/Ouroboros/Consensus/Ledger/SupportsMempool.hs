{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
module Ouroboros.Consensus.Ledger.SupportsMempool (
    ApplyTxErr
  , GenTx
  , GenTxId
  , HasTxId (..)
  , HasTxs (..)
  , Invalidated (..)
  , LedgerSupportsMempool (..)
  , ReapplyTxsResult (..)
  , TxId
  , Validated
  , WhetherToIntervene (..)
  ) where

import           Control.Monad.Except
import           Data.Foldable
import           Data.Kind (Type)
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Util.IOLike

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
      , NoThunks (GenTx blk)
      , NoThunks (Validated (GenTx blk))
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
          -> TickedLedgerState blk ValuesMK
          -> Except (ApplyTxErr blk) (TickedLedgerState blk DiffMK, Validated (GenTx blk))

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
            -> TickedLedgerState blk ValuesMK
            -> Except (ApplyTxErr blk) (TickedLedgerState blk ValuesMK)

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
       HasCallStack
    => LedgerConfig blk
    -> SlotNo -- ^ Slot number of the block containing the tx
    -> [Validated (GenTx blk)]
    -> TickedLedgerState blk ValuesMK
    -> ReapplyTxsResult blk
  reapplyTxs cfg slot txs st =
      (\(err, val, st') ->
         ReapplyTxsResult
           err
           (reverse val)
           (forgetTrackingValues . calculateDifference st $ st')
      )
    $ foldl' (\(accE, accV, st') tx ->
                 case runExcept (reapplyTx cfg slot tx st') of
                   Left err   -> (Invalidated tx err : accE, accV, st')
                   Right st'' -> (accE, tx : accV, st'')
             ) ([], [], st) txs

  -- | The maximum number of bytes worth of transactions that can be put into
  -- a block according to the currently adopted protocol parameters of the
  -- ledger state.
  --
  -- This is (conservatively) computed by subtracting the header size and any
  -- other fixed overheads from the maximum block size.
  txsMaxBytes :: TickedLedgerState blk mk -> Word32

  -- | Return the post-serialisation size in bytes of a 'GenTx' /when it is
  -- embedded in a block/. This size might differ from the size of the
  -- serialisation used to send and receive the transaction across the
  -- network.
  --
  -- This size is used to compute how many transaction we can put in a block
  -- when forging one.
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
  txInBlockSize :: GenTx blk -> Word32

  -- | Discard the evidence that transaction has been previously validated
  txForgetValidated :: Validated (GenTx blk) -> GenTx blk

  txRefScriptSize :: LedgerConfig blk -> TickedLedgerState blk -> GenTx blk -> Int
  -- | Given a transaction, get the key-sets that we need to apply it to a
  -- ledger state.
  getTransactionKeySets :: GenTx blk -> LedgerTables (LedgerState blk) KeysMK

data ReapplyTxsResult blk =
  ReapplyTxsResult {
      -- | txs that are now invalid. Order doesn't matter
      invalidatedTxs :: ![Invalidated blk]
      -- | txs that are valid again, order must be the same as the order in
      -- which txs were received
    , validatedTxs   :: ![Validated (GenTx blk)]
      -- | Resulting ledger state
    , resultingState :: !(TickedLedgerState blk DiffMK)
    }

-- | A generalized transaction, 'GenTx', identifier.
type TxId :: Type -> Type
data family TxId blk

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

-- | Shorthand: ID of a generalized transaction
type GenTxId blk = TxId (GenTx blk)

-- | Collect all transactions from a block
--
-- This is used for tooling only. We don't require it as part of RunNode
-- (and cannot, because we cannot give an instance for the dual ledger).
class HasTxs blk where
  -- | Return the transactions part of the given block in no particular order.
  extractTxs :: blk -> [GenTx blk]

-- | A transaction that was previously valid. Used to clarify the types on the
-- 'reapplyTxs' function.
data Invalidated blk = Invalidated { getInvalidated :: Validated (GenTx blk)
                                   , getReason      ::  ApplyTxErr blk
                                   }
