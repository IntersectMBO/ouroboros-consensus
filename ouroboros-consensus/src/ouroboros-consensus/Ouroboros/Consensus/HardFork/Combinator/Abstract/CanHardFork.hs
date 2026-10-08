{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
  ( CanHardFork (..)
  , HashSizeOfHead
  , EqualHashSizeOfHead
  , rawHashNS
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.Function (on)
import Data.Measure (Measure)
import Data.SOP.BasicFunctors (K (..))
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
  ( All Top xs
  , All SingleEraBlock xs
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
  , -- \* Endorser block
    Measure (HardForkTxEbMeasure xs)
  , NoThunks (HardForkTxEbMeasure xs)
  , Eq (HardForkTxEbMeasure xs)
  , Show (HardForkTxEbMeasure xs)
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

  -- | One measure that can accurately represent the 'TxEbMeasure' of every era
  -- in @xs@.
  --
  -- Usually, this can simply be the union of the sets of components of each
  -- individual era's 'TxEbMeasure'.
  type HardForkTxEbMeasure xs

  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel :: Tails AcrossEraTiebreaker xs

  -- | This is ideally exact.
  --
  -- If that's not possible, the result must not be too small, since this is
  -- relied upon to determine which prefix of the mempool's txs will fit in a
  -- valid block.
  hardForkInjTxMeasurePhase1 :: SOP.NS WrapTxMeasurePhase1 xs -> HardForkTxMeasurePhase1 xs

  hardForkInjTxMeasurePhase2 :: SOP.NS WrapTxMeasurePhase2 xs -> HardForkTxMeasurePhase2 xs

  hardForkInjTxEbMeasure :: SOP.NS WrapTxEbMeasure xs -> HardForkTxEbMeasure xs

  -- | Projects a combined phase 1 measure to every era position. Projecting
  -- the injection of a measure gives back that measure.
  --
  -- For every era position @idx@ and every measure @m@ of that era:
  --
  -- > projectNP idx (hardForkProjTxMeasurePhase1 (hardForkInjTxMeasurePhase1 (injectNS idx m))) == m
  --
  -- The argument can also be the injection of a measure of an earlier era.
  -- Then the value at position @idx@ must be that measure, with zero in each
  -- field that the earlier era lacks. After a hard fork, the mempool keeps the
  -- measure that a transaction got in the era in which the mempool added it.
  -- 'Ouroboros.Consensus.HardFork.Combinator.Forging.projectMempoolSnapshot'
  -- projects that measure with the projection of the new era.
  --
  -- The result is a strict 'SOP.NP'. A caller that picks one era with
  -- 'Data.SOP.Index.projectNP' still evaluates every position. So every
  -- position must give a result for any input. It must never call 'error' or
  -- assert.
  hardForkProjTxMeasurePhase1 :: HardForkTxMeasurePhase1 xs -> SOP.NP WrapTxMeasurePhase1 xs

  -- | Projects a combined phase 2 measure to every era position. The rules of
  -- 'hardForkProjTxMeasurePhase1' hold, with 'hardForkInjTxMeasurePhase2' as
  -- the injection.
  hardForkProjTxMeasurePhase2 :: HardForkTxMeasurePhase2 xs -> SOP.NP WrapTxMeasurePhase2 xs

  -- | Projects a combined endorser-block measure to every era position. The
  -- rules of 'hardForkProjTxMeasurePhase1' hold, with 'hardForkInjTxEbMeasure'
  -- as the injection.
  --
  -- 'Ouroboros.Consensus.HardFork.Combinator.Forging.hardForkBlockForging'
  -- also projects sums of 'hardForkTxEbMeasure' results. Such a sum need not be
  -- the injection of any era's measure. So the projection keeps the fields that
  -- the era measures and ignores the rest. It does not check that the rest is
  -- zero.
  hardForkProjTxEbMeasure :: HardForkTxEbMeasure xs -> SOP.NP WrapTxEbMeasure xs

  -- | 'txEbMeasure' for the hard fork block.
  --
  -- The two arguments are the fields of its 'TxMeasure'. Naming that type
  -- needs @HardForkBlock xs@, which is defined downstream of this module.
  hardForkTxEbMeasure ::
    proxy xs ->
    HardForkTxMeasurePhase1 xs ->
    HardForkTxMeasurePhase2 xs ->
    HardForkTxEbMeasure xs

  -- | 'mempoolEbReservation' for the hard fork block.
  --
  -- The result is the fields of its 'TxMeasure', for the reason given at
  -- 'hardForkTxEbMeasure'.
  hardForkMempoolEbReservation ::
    proxy xs ->
    HardForkTxEbMeasure xs ->
    (HardForkTxMeasurePhase1 xs, HardForkTxMeasurePhase2 xs)

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
-- this hash. It serialises each id via 'toRawTxIdHash', which allocates.
rawHashNS :: All SingleEraBlock xs => SOP.NS WrapGenTxId xs -> ShortByteString
rawHashNS = SOP.hcollapse . SOP.hcmap proxySingle (K . toRawTxIdHash . unwrapGenTxId)

instance SingleEraBlock blk => CanHardFork '[blk] where
  type HardForkTxMeasurePhase1 '[blk] = TxMeasurePhase1 blk
  type HardForkTxMeasurePhase2 '[blk] = TxMeasurePhase2 blk
  type HardForkTxEbMeasure '[blk] = TxEbMeasure blk

  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel = Tails.mk1

  hardForkInjTxMeasurePhase1 (SOP.Z (WrapTxMeasurePhase1 x)) = x
  hardForkInjTxMeasurePhase2 (SOP.Z (WrapTxMeasurePhase2 x)) = x
  hardForkInjTxEbMeasure (SOP.Z (WrapTxEbMeasure x)) = x

  hardForkProjTxMeasurePhase1 x = WrapTxMeasurePhase1 x SOP.:* SOP.Nil
  hardForkProjTxMeasurePhase2 x = WrapTxMeasurePhase2 x SOP.:* SOP.Nil
  hardForkProjTxEbMeasure x = WrapTxEbMeasure x SOP.:* SOP.Nil

  hardForkTxEbMeasure _ p1 p2 = txEbMeasure (Proxy @blk) (TxMeasure p1 p2)

  hardForkMempoolEbReservation _ eb =
    let TxMeasure p1 p2 = mempoolEbReservation (Proxy @blk) eb in (p1, p2)

  -- No production code uses a single-era hard fork, so an allocating raw-hash
  -- comparison is fine here.
  --
  -- NOTE: if some production code ever uses a single-era hard fork, it may
  -- want an allocation-free comparator here, as the Cardano instance has.
  hardForkEqGenTxId = (==) `on` rawHashNS
  hardForkCompareGenTxId = compare `on` rawHashNS
