{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Mempool capacity, size and transaction size datatypes.
--
-- This module also defines how to manually override the mempool capacity.
--
-- > import           Ouroboros.Consensus.Mempool.Capacity (Capacity)
-- > import qualified Ouroboros.Consensus.Mempool.Capacity as Capacity
module Ouroboros.Consensus.Mempool.Capacity (
    -- * Mempool capacity
    MempoolCapacityBytes (..)
  , MempoolCapacityBytesOverride (..)
  , computeMempoolCapacity
  , mkCapacityBytesOverride
    -- * Mempool Size
  , MempoolSize (..)
    -- * Transaction size
  , ByteSize (..)
  , TxLimits (..)
  ) where

import           Cardano.Prelude (NFData)
import           Data.Measure (Measure)
import           Data.Word (Word32)
import           NoThunks.Class
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ticked (Ticked (..))

{-------------------------------------------------------------------------------
  Mempool capacity in bytes
-------------------------------------------------------------------------------}

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacityBytes = MempoolCapacityBytes {
      getMempoolCapacityBytes :: Word32
    }
  deriving (Eq, Show, NoThunks)

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.
  deriving (Eq, Show)

-- | Create an override for the mempool capacity using the provided number of
-- bytes.
mkCapacityBytesOverride :: Word32 -> MempoolCapacityBytesOverride
mkCapacityBytesOverride = MempoolCapacityBytesOverride . MempoolCapacityBytes

-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
computeMempoolCapacity ::
     LedgerSupportsMempool blk
  => TickedLedgerState blk
  -> MempoolCapacityBytesOverride
  -> MempoolCapacityBytes
computeMempoolCapacity st mc = case mc of
    NoMempoolCapacityBytesOverride        -> noOverride
    MempoolCapacityBytesOverride override -> override
  where
    noOverride = MempoolCapacityBytes (txsMaxBytes st * 2)

{-------------------------------------------------------------------------------
  Mempool size
-------------------------------------------------------------------------------}

-- | The size of a mempool.
data MempoolSize = MempoolSize
  { msNumTxs   :: !Word32
    -- ^ The number of transactions in the mempool.
  , msNumBytes :: !Word32
    -- ^ The summed byte size of all the transactions in the mempool.
  } deriving (Eq, Show)

instance Semigroup MempoolSize where
  MempoolSize xt xb <> MempoolSize yt yb = MempoolSize (xt + yt) (xb + yb)

instance Monoid MempoolSize where
  mempty = MempoolSize { msNumTxs = 0, msNumBytes = 0 }
  mappend = (<>)

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
class Measure (TxMeasure blk) => TxLimits blk where
  type TxMeasure blk

  -- | What is the measure an individual tx?
  txMeasure ::
       TickedLedgerState blk
    -> Validated (GenTx blk)
    -> TxMeasure blk

  -- | What is the allowed capacity for txs in an individual block?
  txsBlockCapacity :: Ticked (LedgerState blk) -> TxMeasure blk

{-------------------------------------------------------------------------------
  ByteSize
-------------------------------------------------------------------------------}

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, NFData, Ord)
  deriving newtype (Measure)
