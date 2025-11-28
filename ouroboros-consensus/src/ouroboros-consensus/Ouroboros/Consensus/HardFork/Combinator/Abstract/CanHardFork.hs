{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (PrefixC, CanHardFork (..)) where

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
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import Ouroboros.Consensus.HardFork.Combinator.Translation
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.TypeLevel

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class PrefixI (TablesForBlock x) (TablesForBlock y) => PrefixC x y
instance PrefixI (TablesForBlock x) (TablesForBlock y) => PrefixC x y

class ToAllDict (KVConstraintsMK x DiffMK) (TablesForBlock x) => ToAllDictTables x
instance ToAllDict (KVConstraintsMK x DiffMK) (TablesForBlock x) => ToAllDictTables x

class
  ( All SingleEraBlock xs
  , All (HasLedgerTables LedgerState) xs
  , All (HasTickedLedgerTables LedgerState) xs
  , Typeable xs
  , IsNonEmpty xs
  , Measure (HardForkTxMeasure xs)
  , HasByteSize (HardForkTxMeasure xs)
  , NoThunks (HardForkTxMeasure xs)
  , Show (HardForkTxMeasure xs)
  , TxMeasureMetrics (HardForkTxMeasure xs)
  , InPairs.InPairsC PrefixC xs
  , All ToAllDictTables xs
  ) =>
  CanHardFork xs
  where
  -- | A measure that can accurately represent the 'TxMeasure' of any era.
  --
  -- Usually, this can simply be the union of the sets of components of each
  -- individual era's 'TxMeasure'. (Which is too awkward of a type to express
  -- in Haskell.)
  type HardForkTxMeasure xs

  hardForkEraTranslation :: EraTranslation xs
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

  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel = Tails.mk1
  hardForkInjectTxs = InPairs.mk1

  hardForkInjTxMeasure (SOP.Z (WrapTxMeasure x)) = x
