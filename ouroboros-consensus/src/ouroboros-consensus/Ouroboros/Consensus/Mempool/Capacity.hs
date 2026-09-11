{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Mempool capacity, size and transaction size datatypes.
--
-- This module also defines how to manually override the mempool capacity.
--
-- > import           Ouroboros.Consensus.Mempool.Capacity (Capacity)
-- > import qualified Ouroboros.Consensus.Mempool.Capacity as Capacity
module Ouroboros.Consensus.Mempool.Capacity
  ( -- * Mempool capacity
    MempoolCapacityBytesOverride (..)
  , computeMempoolCapacity
  , mkCapacityBytesOverride

    -- * Mempool Size
  , MempoolSize (..)
  ) where

import Data.DerivingVia (InstantiatedAt (..))
import Data.Measure (Measure)
import Data.Semigroup (stimes)
import Data.Word (Word32)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.SupportsMempool

{-------------------------------------------------------------------------------
  Mempool capacity in bytes
-------------------------------------------------------------------------------}

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity
data MempoolCapacityBytesOverride
  = -- | Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
    NoMempoolCapacityBytesOverride
  | -- | Use the least multiple of the block capacity that is no less than this
    -- size.
    MempoolCapacityBytesOverride !ByteSize32
  deriving (Eq, Show)

-- | Create an override for the mempool capacity using the provided number of
-- bytes.
mkCapacityBytesOverride :: ByteSize32 -> MempoolCapacityBytesOverride
mkCapacityBytesOverride = MempoolCapacityBytesOverride

-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
--
-- If an override is present, reinterpret it as a number of blocks (rounded
-- up), and then simply multiply the ledger's capacity by that number.
computeMempoolCapacity ::
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  TickedLedgerState blk mk ->
  MempoolCapacityBytesOverride ->
  TxMeasure blk
computeMempoolCapacity cfg st override =
  capacity
 where
  -- FIXME: this sizes the mempool from the ranking (Praos) block capacity only,
  -- and only from protocol parameters via 'blockCapacityTxMeasure'. Under Leios
  -- the mempool also feeds Endorser Blocks, whose capacity is
  -- 'ebCapacityTxMeasure', and a single EB can be many Praos blocks' worth, so
  -- the default (blockCount = 2) does not hold even one EB.
  --
  -- Today the only fix is 'MempoolCapacityBytesOverride', which is per-operator
  -- node config, not a protocol parameter. That is acceptable for the forge time
  -- cap (a genuine local latency/resource tradeoff), but mempool size is not
  -- local: an operator whose mempool is too small forges undersized EBs no
  -- matter what the ledger parameters allow, so it caps network throughput from
  -- one node's config. The default should instead be derived from the
  -- parameters, e.g. a small multiple of @blockCapacity + ebCapacity@ (both
  -- already parameter-derived here), so no override is needed for correctness
  -- and the effective size is network-consistent.
  oneBlock = blockCapacityTxMeasure cfg st
  ByteSize32 oneBlockBytes = txMeasureByteSize oneBlock

  blockCount = case override of
    NoMempoolCapacityBytesOverride -> 2
    MempoolCapacityBytesOverride (ByteSize32 x) ->
      -- This calculation is happening at Word32. If it was to overflow, it
      -- will round down instead.
      max 1 $
        if x + oneBlockBytes < x
          then x `div` oneBlockBytes
          else (x + oneBlockBytes - 1) `div` oneBlockBytes

  SemigroupViaMeasure capacity =
    stimes blockCount (SemigroupViaMeasure oneBlock)

newtype SemigroupViaMeasure a = SemigroupViaMeasure a
  deriving newtype (Eq, Measure)
  deriving Semigroup via (InstantiatedAt Measure (SemigroupViaMeasure a))

{-------------------------------------------------------------------------------
  Mempool size
-------------------------------------------------------------------------------}

-- | The size of a mempool.
data MempoolSize = MempoolSize
  { msNumTxs :: !Word32
  -- ^ The number of transactions in the mempool.
  , msNumBytes :: !ByteSize32
  -- ^ The summed byte size of all the transactions in the mempool.
  }
  deriving (Eq, Show, Generic, NoThunks)

instance Semigroup MempoolSize where
  MempoolSize xt xb <> MempoolSize yt yb = MempoolSize (xt + yt) (xb <> yb)

instance Monoid MempoolSize where
  mempty = MempoolSize{msNumTxs = 0, msNumBytes = ByteSize32 0}
  mappend = (<>)
