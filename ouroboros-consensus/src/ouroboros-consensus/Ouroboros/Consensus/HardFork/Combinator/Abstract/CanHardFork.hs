{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork (..)) where

import Data.Kind
import Data.Measure (Measure)
import Data.SOP.Constraint
import Data.SOP.Functors (Product2)
import Data.SOP.InPairs (InPairs, RequiringBoth)
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.NonEmpty
import qualified Data.SOP.Strict as SOP
import Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import Data.Typeable
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import Ouroboros.Consensus.HardFork.Combinator.Translation
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class
  ( All SingleEraBlock xs
  , Typeable xs
  , IsNonEmpty xs
  , Measure (HardForkTxMeasure xs)
  , HasByteSize (HardForkTxMeasure xs)
  , NoThunks (HardForkTxMeasure xs)
  , Show (HardForkTxMeasure xs)
  , TxMeasureMetrics (HardForkTxMeasure xs)
  ) =>
  CanHardFork xs
  where
  -- | A measure that can accurately represent the 'TxMeasure' of any era.
  --
  -- Usually, this can simply be the union of the sets of components of each
  -- individual era's 'TxMeasure'. (Which is too awkward of a type to express
  -- in Haskell.)
  type HardForkTxMeasure xs

  -- | The HFC's projection of
  -- 'Ouroboros.Consensus.Ledger.Basics.LedgerTablesFactory' for the
  -- @HardForkBlock xs@ — i.e. the factory used by era-translation
  -- functions to materialise the destination era's 'LedgerTablesHandle'
  -- when ticking crosses an era boundary.
  --
  -- The bridging equation
  -- @LedgerTablesFactory m (HardForkBlock xs) = HFLedgerTablesFactory m xs@
  -- lives in the HFC's 'BlockSupportsLedgerHD' instance: the two names
  -- denote the same concept, separated only because the HFC machinery
  -- (in particular 'hardForkStateHandleTranslation') lives on
  -- 'CanHardFork' while the abstract factory lives on
  -- 'BlockSupportsLedgerHD'.
  --
  -- For Cardano this is @MkHandle m@. For the trivial single-era
  -- HFC instance below, it falls through to the era's own
  -- 'LedgerTablesFactory'.
  type HFLedgerTablesFactory (m :: Type -> Type) xs
  type HFLedgerTablesFactory m xs = ()

  hardForkEraTranslation :: EraTranslation xs
  hardForkStateHandleTranslation ::
    MonadThrow m => HFLedgerTablesFactory m xs -> StateHandleTranslation m xs
  hardForkChainSel :: Tails AcrossEraTiebreaker xs
  hardForkInjectTxs ::
    InPairs
      ( RequiringBoth
          WrapLedgerConfig
          (Product2 InjectTx InjectValidatedTx)
      )
      xs

  -- | This is ideally exact.
  --
  -- If that's not possible, the result must not be too small, since this is
  -- relied upon to determine which prefix of the mempool's txs will fit in a
  -- valid block.
  hardForkInjTxMeasure :: SOP.NS WrapTxMeasure xs -> HardForkTxMeasure xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  type HardForkTxMeasure '[blk] = TxMeasure blk
  type HFLedgerTablesFactory m '[blk] = LedgerTablesFactory m blk

  hardForkEraTranslation = trivialEraTranslation
  hardForkStateHandleTranslation = const trivialStateHandleTranslation
  hardForkChainSel = Tails.mk1
  hardForkInjectTxs = InPairs.mk1

  hardForkInjTxMeasure (SOP.Z (WrapTxMeasure x)) = x
