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
import qualified Data.Measure as Measure
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
  = -- | Use 2x the maximum transaction capacity of a block plus, under Leios,
    -- an endorser block's closure. This will change dynamically with the
    -- protocol parameters adopted in the current ledger.
    NoMempoolCapacityBytesOverride
  | -- | Use the least multiple of the block (plus endorser-block closure)
    -- capacity that is no less than this size.
    MempoolCapacityBytesOverride !ByteSize32
  deriving (Eq, Show)

-- | Create an override for the mempool capacity using the provided number of
-- bytes.
mkCapacityBytesOverride :: ByteSize32 -> MempoolCapacityBytesOverride
mkCapacityBytesOverride = MempoolCapacityBytesOverride

-- | If no override is provided, calculate the default mempool capacity as 2x
-- what one forging opportunity can drain from the mempool: the current
-- ledger's maximum transaction capacity of a block plus, under Leios, of an
-- endorser block's closure ('ebClosureCapacityTxMeasure', zero elsewhere).
--
-- If an override is present, reinterpret it as a number of such forging units
-- (rounded up), and then simply multiply the unit capacity by that number.
--
-- Note that admission is bounded on the 'TxMeasure' components only: an
-- endorser block's references are bounded per fill (by 'ebCapacityTxMeasure'
-- at forge time), never here, so no endorser-block parameter can gate what
-- enters the mempool.
computeMempoolCapacity ::
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  TickedLedgerState blk mk ->
  MempoolCapacityBytesOverride ->
  TxMeasure blk
computeMempoolCapacity cfg st override =
  capacity
 where
  oneUnit =
    blockCapacityTxMeasure cfg st
      `Measure.plus` ebClosureCapacityTxMeasure cfg st
  ByteSize32 oneUnitBytes = txMeasureByteSize oneUnit

  unitCount = case override of
    NoMempoolCapacityBytesOverride -> 2
    MempoolCapacityBytesOverride (ByteSize32 x) ->
      -- This calculation is happening at Word32. If it was to overflow, it
      -- will round down instead.
      max 1 $
        if x + oneUnitBytes < x
          then x `div` oneUnitBytes
          else (x + oneUnitBytes - 1) `div` oneUnitBytes

  SemigroupViaMeasure capacity =
    stimes unitCount (SemigroupViaMeasure oneUnit)

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
