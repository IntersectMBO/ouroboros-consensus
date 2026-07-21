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
  , rawHashNS
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.Function (on)
import Data.Measure (Measure)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Constraint
import Data.SOP.Functors (Product2)
import Data.SOP.InPairs (InPairs, RequiringBoth)
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.NonEmpty
import qualified Data.SOP.Strict as SOP
import Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import Data.Typeable
import GHC.TypeNats (KnownNat)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (HashSize)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.InjectTxs
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

  -- | Whether two transaction ids of @xs@ are equal, ignoring which era each
  -- sits in. Two txids in different eras can be equal; see the
  -- 'Ouroboros.Consensus.HardFork.Combinator.AcrossEras.OneEraGenTxId' 'Eq'
  -- instance.
  --
  -- Runs on every mempool lookup, so instances should avoid allocation.
  -- 'rawHashNS' is the reference implementation and allocates; the Cardano
  -- instance overrides it with an allocation-free walk. There is no class
  -- default: every instance names its body explicitly.
  hardForkEqGenTxId :: SOP.NS WrapGenTxId xs -> SOP.NS WrapGenTxId xs -> Bool

  -- | Order two transaction ids of @xs@. See 'hardForkEqGenTxId'.
  hardForkCompareGenTxId ::
    SOP.NS WrapGenTxId xs -> SOP.NS WrapGenTxId xs -> Ordering

-- | The raw hash of an era sum, era ignored.
--
-- The reference comparison for transaction ids. Non-optimizing 'CanHardFork'
-- instances implement 'hardForkEqGenTxId'\/'hardForkCompareGenTxId' by comparing
-- this hash. It serialises each id via 'toRawTxIdHash', which allocates; the
-- Cardano instance overrides both methods with an allocation-free walk.
rawHashNS :: All SingleEraBlock xs => SOP.NS WrapGenTxId xs -> ShortByteString
rawHashNS = SOP.hcollapse . SOP.hcmap proxySingle (K . toRawTxIdHash . unwrapGenTxId)

instance SingleEraBlock blk => CanHardFork '[blk] where
  type HardForkTxMeasure '[blk] = TxMeasure blk

  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel = Tails.mk1
  hardForkInjectTxs = InPairs.mk1

  hardForkInjTxMeasure (SOP.Z (WrapTxMeasure x)) = x

  hardForkEqGenTxId = (==) `on` rawHashNS
  hardForkCompareGenTxId = compare `on` rawHashNS
