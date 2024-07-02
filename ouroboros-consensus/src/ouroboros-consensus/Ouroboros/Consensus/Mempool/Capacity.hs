{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Mempool capacity, size and transaction size datatypes.
--
-- This module also defines how to manually override the mempool capacity.
--
-- > import           Ouroboros.Consensus.Mempool.Capacity (Capacity)
-- > import qualified Ouroboros.Consensus.Mempool.Capacity as Capacity
module Ouroboros.Consensus.Mempool.Capacity (
    -- * Mempool Size
    MempoolSize (..)
  , (<=)
    -- * Restricting more strongly than the ledger's limits
  , TxOverrides
  , applyOverrides
  , getOverrides
  , mkOverrides
  , noOverridesMeasure
  ) where

import           Data.Coerce (coerce)
import           Data.Measure (BoundedMeasure, Measure)
import qualified Data.Measure as Measure
import           Data.Word (Word32)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Prelude hiding ((<=))

{-------------------------------------------------------------------------------
  Mempool size
-------------------------------------------------------------------------------}

-- | The size of a mempool.
data MempoolSize sz = MempoolSize
  { msNumTxs :: !Word32
    -- ^ The number of transactions in the mempool.
  , msSize   :: !sz
    -- ^ The summed measure of all the transactions in the mempool.
  } deriving (Eq, Functor, Show)

instance Measure sz => Semigroup (MempoolSize sz) where
  MempoolSize xt xb <> MempoolSize yt yb = MempoolSize (xt + yt) (xb `Measure.plus` yb)

instance Measure sz => Monoid (MempoolSize sz) where
  mempty = MempoolSize { msNumTxs = 0, msSize = Measure.zero }
  mappend = (<>)

-- | Is every component of the first value less-than-or-equal-to the
-- corresponding component of the second value?
(<=) :: Measure a => a -> a -> Bool
(<=) = (Measure.<=)

{-------------------------------------------------------------------------------
  Overrides
-------------------------------------------------------------------------------}

-- | An override that lowers a capacity limit
--
-- Specifically, we use this override to let the node operator limit the total
-- 'TxMeasure' of transactions in blocks even more severely than would the
-- ledger state's 'txsBlockCapacity'. The forge logic will use the 'Measure.min'
-- (ie the lattice's @meet@ operator) to combine this override with the capacity
-- given by the ledger state. More concretely, that will typically be a
-- componentwise minimum operation, along each of the components\/dimensions of
-- @'TxMeasure' blk@.
--
-- This newtype wrapper distinguishes the intention of this particular
-- 'TxMeasure' as such an override. We use 'TxMeasure' in different ways in this
-- code base. The newtype also allows us to distinguish the one most appropriate
-- monoid among many offered by the 'TxLimits' superclass constraints: it is the
-- monoid induced by the bounded meet-semilattice (see 'BoundedMeasure') that is
-- relevant to the notion of /overriding/ the ledger's block capacity.
newtype TxOverrides blk =
  -- This constructor is not exported.
  TxOverrides { getOverrides :: TxMeasure blk }

instance TxLimits blk => Monoid (TxOverrides blk) where
  mempty = TxOverrides noOverridesMeasure

instance TxLimits blk => Semigroup (TxOverrides blk) where
  (<>) = coerce $ Measure.min @(TxMeasure blk)

-- | @'applyOverrides' 'noOverrides' m = m@
noOverridesMeasure :: BoundedMeasure a => a
noOverridesMeasure = Measure.maxBound

-- | Smart constructor for 'Overrides'.
mkOverrides :: TxMeasure blk -> TxOverrides blk
mkOverrides = TxOverrides

-- | Apply the override
applyOverrides ::
     TxLimits blk
  => TxOverrides blk
  -> TxMeasure blk
  -> TxMeasure blk
applyOverrides (TxOverrides m') m = Measure.min m' m
