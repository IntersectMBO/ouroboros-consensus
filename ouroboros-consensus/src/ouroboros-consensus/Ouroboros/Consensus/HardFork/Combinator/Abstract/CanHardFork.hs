{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
  ( CanHardFork (..)
  , HashSizeOfHead
  ) where

import Data.Measure (Measure)
import Data.SOP.Constraint
import Data.SOP.NonEmpty
import qualified Data.SOP.Strict as SOP
import Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import Data.Typeable
import GHC.TypeNats (KnownNat)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (HashSize)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import Ouroboros.Consensus.HardFork.Combinator.Translation
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

-- | The hash size shared by all eras of a hard fork, represented by that of the
-- first era.
--
-- See 'EqualHashSizeOfHead' superclass constraint in 'CanHardFork' and the
-- @ConvertRawHash (HardForkBlock xs)@ instance.
type family HashSizeOfHead xs where
  HashSizeOfHead (x ': _) = HashSize x

-- | Witnesses that the hash size of @blk@ coincides with 'HashSizeOfHead' of
-- @xs@, i.e. with the hash size of the first era.
--
-- 'CanHardFork' requires @'All' ('EqualHashSizeOfHead' xs) xs@, which statically
-- guarantees that all eras of a hard fork use the same hash size. This lets the
-- @ConvertRawHash (HardForkBlock xs)@ instance enforce
-- @HashSize (HardForkBlock xs) = HashSizeOfHead xs@ without any runtime check.
class HashSize blk ~ HashSizeOfHead xs => EqualHashSizeOfHead xs blk

instance HashSize blk ~ HashSizeOfHead xs => EqualHashSizeOfHead xs blk

class
  ( All SingleEraBlock xs
  , All (EqualHashSizeOfHead xs) xs
  , KnownNat (HashSizeOfHead xs)
  , Typeable xs
  , IsNonEmpty xs
  , -- \* Phase1
    Measure (HardForkTxMeasurePhase1 xs)
  , HasByteSize (HardForkTxMeasurePhase1 xs)
  , NoThunks (HardForkTxMeasurePhase1 xs)
  , Show (HardForkTxMeasurePhase1 xs)
  , TxMeasurePhase1Metrics (HardForkTxMeasurePhase1 xs)
  , -- \* Phase2
    Measure (HardForkTxMeasurePhase2 xs)
  , NoThunks (HardForkTxMeasurePhase2 xs)
  , Show (HardForkTxMeasurePhase2 xs)
  , TxMeasurePhase2Metrics (HardForkTxMeasurePhase2 xs)
  ) =>
  CanHardFork xs
  where
  -- | A measure that can accurately represent the 'TxMeasure' of any era.
  --
  -- Usually, this can simply be the union of the sets of components of each
  -- individual era's 'TxMeasure'. (Which is too awkward of a type to express
  -- in Haskell.)
  type HardForkTxMeasurePhase1 xs

  type HardForkTxMeasurePhase2 xs

  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel :: Tails AcrossEraTiebreaker xs

  -- | This is ideally exact.
  --
  -- If that's not possible, the result must not be too small, since this is
  -- relied upon to determine which prefix of the mempool's txs will fit in a
  -- valid block.
  hardForkInjTxMeasurePhase1 :: SOP.NS WrapTxMeasurePhase1 xs -> HardForkTxMeasurePhase1 xs

  hardForkInjTxMeasurePhase2 :: SOP.NS WrapTxMeasurePhase2 xs -> HardForkTxMeasurePhase2 xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  type HardForkTxMeasurePhase1 '[blk] = TxMeasurePhase1 blk
  type HardForkTxMeasurePhase2 '[blk] = TxMeasurePhase2 blk

  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel = Tails.mk1

  hardForkInjTxMeasurePhase1 (SOP.Z (WrapTxMeasurePhase1 x)) = x
  hardForkInjTxMeasurePhase2 (SOP.Z (WrapTxMeasurePhase2 x)) = x
