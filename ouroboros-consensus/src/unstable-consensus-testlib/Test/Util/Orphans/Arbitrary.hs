{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.Arbitrary
  ( SmallDiffTime (..)
  , genLimitedEpochSize
  , genLimitedSlotNo
  , genSmallEpochNo
  , genSmallSlotNo

    -- * Time
  , genNominalDiffTime50Years
  , genUTCTime50Years
  ) where

import Cardano.Ledger.BaseTypes (NonZero (..), unsafeNonZero)
import Data.Coerce (coerce)
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Dict (Dict (..), all_NP, mapAll)
import Data.SOP.Functors (Flip (..))
import Data.SOP.NonEmpty
  ( IsNonEmpty
  , ProofNonEmpty (..)
  , checkIsNonEmpty
  , isNonEmpty
  )
import Data.SOP.Sing
import Data.SOP.Strict
import Data.Time
import Data.Word (Word64)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.HardFork.Combinator
  ( HardForkBlock
  , HardForkChainDepState
  , HardForkState (..)
  , LedgerEraInfo (..)
  , LedgerState (..)
  , Mismatch (..)
  , MismatchEraInfo (..)
  , SingleEraBlock (..)
  , SingleEraInfo
  , Telescope (..)
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.State
  ( Current (..)
  , Past (..)
  )
import Ouroboros.Consensus.HardFork.History (Bound (..))
import Ouroboros.Consensus.HardFork.History.EraParams
import Ouroboros.Consensus.HeaderValidation (TipInfo)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck
  ( ClockSkew
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepState
  , SecurityParam (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.API (LoE (..))
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
  ( ChunkNo (..)
  , ChunkSize (..)
  , RelativeSlot (..)
  )
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (Flag (..))
import Ouroboros.Network.SizeInBytes
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Slotting.Arbitrary ()
import Test.QuickCheck hiding (Fixed (..))
import Test.QuickCheck.Instances ()
import Test.Util.Time (dawnOfTime)

minNumCoreNodes :: Word64
minNumCoreNodes = 2

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

-- | Picks time span between 0 seconds and (roughly) 50 years
--
-- /Note/ - Arbitrary instance for `NominalDiffTime` comes from @quickcheck-instances@ and
-- it uses a much wider timespan.
genNominalDiffTime50Years :: Gen NominalDiffTime
genNominalDiffTime50Years = conv <$> choose (0, 50 * daysPerYear * secondsPerDay)
 where
  conv :: Double -> NominalDiffTime
  conv = realToFrac

-- | Picks moment between 'dawnOfTime' and (roughly) 50 years later
--
-- /Note/ - Arbitrary instance for `UTCTime` comes from @quickcheck-instances@ and it uses
-- a much wider timespan.
genUTCTime50Years :: Gen UTCTime
genUTCTime50Years = (`addUTCTime` dawnOfTime) <$> genNominalDiffTime50Years

-- | Length between 0.001 and 20 seconds, millisecond granularity
instance Arbitrary SlotLength where
  arbitrary = slotLengthFromMillisec <$> choose (1, 20 * 1_000)

  -- Try to shrink the slot length to just "1", for tests where the slot length
  -- really doesn't matter very much
  shrink slotLen = if slotLen /= oneSec then [oneSec] else []
   where
    oneSec = slotLengthFromSec 1

instance Arbitrary RelativeSlot where
  arbitrary = RelativeSlot <$> arbitrary <*> arbitrary <*> arbitrary

-- | The functions 'slotAtTime' and 'timeUntilNextSlot' suffer from arithmetic
-- overflow for very large values, so generate values that avoid overflow when
-- used in these two functions. The largest value generated is still sufficently
-- large to allow for 5e12 years worth of slots at a slot interval of 20
-- seconds.
genLimitedSlotNo :: Gen SlotNo
genLimitedSlotNo =
  SlotNo <$> arbitrary `suchThat` (< 0x8000000000000000)

-- | Generate a small SlotNo for the state machine tests. The runtime of the
-- StateMachine prop_sequential tests is proportional the the upper bound.
genSmallSlotNo :: Gen SlotNo
genSmallSlotNo =
  SlotNo <$> choose (0, 1000)

-- | The tests for 'CumulEpochSizes' requires that the sum of a list of these
-- values does not overflow.
--
-- An epoch size must be > 0.
genLimitedEpochSize :: Gen EpochSize
genLimitedEpochSize =
  EpochSize <$> choose (1, 100_000)

genSmallEpochNo :: Gen EpochNo
genSmallEpochNo =
  EpochNo <$> choose (0, 10000)

-- | This picks an 'EpochNo' between 0 and 10000
--
-- We don't pick larger values because we're not interested in testing overflow
-- due to huge epoch numbers and even huger slot numbers.
instance Arbitrary ChunkNo where
  arbitrary = ChunkNo <$> choose (0, 10000)
  shrink = genericShrink

-- | Picks a 'ChunkSize' between 1 and 100, and randomly choose to enable EBBs
instance Arbitrary ChunkSize where
  arbitrary = ChunkSize <$> arbitrary <*> choose (1, 100)
  shrink = genericShrink

instance Arbitrary ChunkSlot where
  arbitrary = UnsafeChunkSlot <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ClockSkew where
  arbitrary = InFutureCheck.clockSkewInSeconds <$> choose (0, 5)
  shrink skew =
    concat
      [ -- Shrink to some simple values, including 0
        -- (it would be useful to know if a test fails only when having non-zero
        -- clock skew)
        [skew0 | skew0 < skew]
      , [skew1 | skew1 < skew]
      ]
   where
    skew0, skew1 :: ClockSkew
    skew0 = InFutureCheck.clockSkewInSeconds 0
    skew1 = InFutureCheck.clockSkewInSeconds 1

deriving newtype instance Arbitrary SizeInBytes

{-------------------------------------------------------------------------------
  SmallDiffTime
-------------------------------------------------------------------------------}

-- | Wrapper around NominalDiffTime with custom 'Arbitrary' instance
--
-- The default 'Arbitrary' instance for 'NominalDiffTime' isn't very useful:
--
-- * It tends to pick huge values
-- * It tends not to pick integer values
-- * It does not shrink
--
-- Our custom instance
--
-- * Picks values between 0 and (1000 * 20 * 10) seconds:
--   - Maximum segment length: 1000
--   - Maximum slot length: 20 seconds
--   - Maximum number of segments: 10
-- * With a 0.1 second precision
-- * Shrinks
newtype SmallDiffTime = SmallDiffTime NominalDiffTime
  deriving Show

instance Arbitrary SmallDiffTime where
  arbitrary = conv <$> choose (0, 1000 * 20 * 10 * 10)
   where
    -- NominalDiffTime conversion functions treat it as seconds
    conv :: Integer -> SmallDiffTime
    conv n = SmallDiffTime $ realToFrac seconds
     where
      seconds :: Double
      seconds = fromInteger n / 10

  -- try to shrink to some small, simple values
  -- (include 1.5 so that we can shrink to a simple, but yet not whole, value)
  shrink (SmallDiffTime d) =
    map SmallDiffTime $
      filter (< d) [1, 1.5, 2, 3, 100]

{-------------------------------------------------------------------------------
  Auxiliary: time
-------------------------------------------------------------------------------}

-- | Average number of days per year
--
-- <https://en.wikipedia.org/wiki/Year>
daysPerYear :: Double
daysPerYear = 365.2425

-- | Seconds per day
secondsPerDay :: Double
secondsPerDay = 24 * 60 * 60

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

-- | Forwarding
instance
  Arbitrary (ChainDepState (BlockProtocol blk)) =>
  Arbitrary (WrapChainDepState blk)
  where
  arbitrary = WrapChainDepState <$> arbitrary
  shrink x = WrapChainDepState <$> shrink (unwrapChainDepState x)

-- | Forwarding
instance
  Arbitrary (HeaderHash blk) =>
  Arbitrary (WrapHeaderHash blk)
  where
  arbitrary = WrapHeaderHash <$> arbitrary
  shrink x = WrapHeaderHash <$> shrink (unwrapHeaderHash x)

-- | Forwarding
instance
  Arbitrary (TipInfo blk) =>
  Arbitrary (WrapTipInfo blk)
  where
  arbitrary = WrapTipInfo <$> arbitrary
  shrink x = WrapTipInfo <$> shrink (unwrapTipInfo x)

-- | Forwarding
instance Arbitrary a => Arbitrary (I a) where
  arbitrary = I <$> arbitrary
  shrink x = I <$> shrink (unI x)

-- | Forwarding
instance
  Arbitrary (ApplyTxErr blk) =>
  Arbitrary (WrapApplyTxErr blk)
  where
  arbitrary = WrapApplyTxErr <$> arbitrary
  shrink x = WrapApplyTxErr <$> shrink (unwrapApplyTxErr x)

{-------------------------------------------------------------------------------
  NS
-------------------------------------------------------------------------------}

instance
  (All (Arbitrary `Compose` f) xs, IsNonEmpty xs) =>
  Arbitrary (NS f xs)
  where
  arbitrary = case isNonEmpty (Proxy @xs) of
    ProofNonEmpty _ pf -> case checkIsNonEmpty pf of
      Nothing -> Z <$> arbitrary
      Just (ProofNonEmpty _ pf') ->
        frequency
          [ (1, Z <$> arbitrary)
          , -- Use the number of remaining cases (one less than @xs@) as the
            -- weight so that the distribution is uniform
            (lengthSList pf', S <$> arbitrary)
          ]
  shrink = hctraverse' (Proxy @(Arbitrary `Compose` f)) shrink

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

instance Arbitrary EraParams where
  arbitrary = EraParams <$> arbitrary <*> arbitrary <*> arbitrary <*> (GenesisWindow <$> arbitrary)

instance Arbitrary SafeZone where
  arbitrary =
    oneof
      [ StandardSafeZone <$> arbitrary
      , return UnsafeIndefiniteSafeZone
      ]

{-------------------------------------------------------------------------------
  Telescope & HardForkState
-------------------------------------------------------------------------------}

instance Arbitrary (f y x) => Arbitrary (Flip f (x :: kx) (y :: ky)) where
  arbitrary = Flip <$> arbitrary

instance Arbitrary Bound where
  arbitrary =
    Bound
      <$> (RelativeTime <$> arbitrary)
      <*> (SlotNo <$> arbitrary)
      <*> (EpochNo <$> arbitrary)

instance Arbitrary (K Past blk) where
  arbitrary = K <$> (Past <$> arbitrary <*> arbitrary)

instance Arbitrary (f blk) => Arbitrary (Current f blk) where
  arbitrary = Current <$> arbitrary <*> arbitrary

instance
  ( IsNonEmpty xs
  , All (Arbitrary `Compose` f) xs
  , All (Arbitrary `Compose` g) xs
  ) =>
  Arbitrary (Telescope g f xs)
  where
  arbitrary = case isNonEmpty (Proxy @xs) of
    ProofNonEmpty _ pf -> case checkIsNonEmpty pf of
      Nothing -> TZ <$> arbitrary
      Just (ProofNonEmpty _ pf') ->
        frequency
          [ (1, TZ <$> arbitrary)
          , (lengthSList pf', TS <$> arbitrary <*> arbitrary)
          ]
  shrink = hctraverse' (Proxy @(Arbitrary `Compose` f)) shrink

instance
  (IsNonEmpty xs, SListI xs, All (Arbitrary `Compose` Flip LedgerState mk) xs) =>
  Arbitrary (LedgerState (HardForkBlock xs) mk)
  where
  arbitrary = case (dictKPast, dictCurrentLedgerState) of
    (Dict, Dict) -> inj <$> arbitrary
   where
    inj ::
      Telescope (K Past) (Current (Flip LedgerState mk)) xs ->
      LedgerState (HardForkBlock xs) mk
    inj = coerce

    dictKPast :: Dict (All (Arbitrary `Compose` (K Past))) xs
    dictKPast = all_NP $ hpure Dict

    dictCurrentLedgerState ::
      Dict (All (Arbitrary `Compose` (Current (Flip LedgerState mk)))) xs
    dictCurrentLedgerState =
      mapAll
        @(Arbitrary `Compose` Flip LedgerState mk)
        @(Arbitrary `Compose` Current (Flip LedgerState mk))
        (\Dict -> Dict)
        Dict

instance
  (IsNonEmpty xs, SListI xs, All (Arbitrary `Compose` WrapChainDepState) xs) =>
  Arbitrary (HardForkChainDepState xs)
  where
  arbitrary = case (dictKPast, dictCurrentWrapChainDepState) of
    (Dict, Dict) -> inj <$> arbitrary
   where
    inj ::
      Telescope (K Past) (Current WrapChainDepState) xs ->
      HardForkChainDepState xs
    inj = coerce

    dictKPast :: Dict (All (Arbitrary `Compose` (K Past))) xs
    dictKPast = all_NP $ hpure Dict

    dictCurrentWrapChainDepState ::
      Dict (All (Arbitrary `Compose` (Current WrapChainDepState))) xs
    dictCurrentWrapChainDepState =
      mapAll
        @(Arbitrary `Compose` WrapChainDepState)
        @(Arbitrary `Compose` Current WrapChainDepState)
        (\Dict -> Dict)
        Dict

{-------------------------------------------------------------------------------
  Mismatch & MismatchEraInfo
-------------------------------------------------------------------------------}

instance
  ( IsNonEmpty xs
  , All (Arbitrary `Compose` f) (x ': xs)
  , All (Arbitrary `Compose` g) (x ': xs)
  ) =>
  Arbitrary (Mismatch f g (x ': xs))
  where
  arbitrary = case isNonEmpty (Proxy @xs) of
    ProofNonEmpty _ pf ->
      frequency $
        mconcat
          [ -- length (x ': xs) = n + 1
            -- This line: n cases, the line below: also n cases.

            [ (1, ML <$> arbitrary <*> arbitrary)
            , (1, MR <$> arbitrary <*> arbitrary)
            ]
          , case checkIsNonEmpty pf of
              Nothing -> []
              -- The line below: n * (n - 1) cases. We want the weights to be
              -- proportional so that the distribution is uniform. We divide each
              -- weight by n to get 1 and 1 for the ML and MR cases above and n - 1 (=
              -- lengthSList pxs') for the MS case below.
              Just (ProofNonEmpty _ pxs') -> [(lengthSList pxs', MS <$> arbitrary)]
          ]

instance SingleEraBlock blk => Arbitrary (SingleEraInfo blk) where
  arbitrary = return $ singleEraInfo (Proxy @blk)

instance SingleEraBlock blk => Arbitrary (LedgerEraInfo blk) where
  arbitrary = return $ LedgerEraInfo $ singleEraInfo (Proxy @blk)

instance
  (All SingleEraBlock (x ': xs), IsNonEmpty xs) =>
  Arbitrary (MismatchEraInfo (x ': xs))
  where
  arbitrary =
    case (dictSingleEraInfo, dictLedgerEraInfo) of
      (Dict, Dict) -> MismatchEraInfo <$> arbitrary
   where
    dictSingleEraInfo ::
      Dict (All (Arbitrary `Compose` SingleEraInfo)) (x ': xs)
    dictSingleEraInfo = all_NP $ hcpure proxySingle Dict

    dictLedgerEraInfo ::
      Dict (All (Arbitrary `Compose` LedgerEraInfo)) (x ': xs)
    dictLedgerEraInfo = all_NP $ hcpure proxySingle Dict

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

instance Arbitrary QueryVersion where
  arbitrary = arbitraryBoundedEnum
  shrink v = if v == minBound then [] else [pred v]

instance
  Arbitrary (SomeBlockQuery (BlockQuery blk)) =>
  Arbitrary (SomeSecond Query blk)
  where
  arbitrary = do
    SomeBlockQuery someBlockQuery <- arbitrary
    return (SomeSecond (BlockQuery someBlockQuery))

instance Arbitrary Index.CacheConfig where
  arbitrary = do
    pastChunksToCache <-
      frequency
        -- Pick small values so that we exercise cache eviction
        [ (1, return 1)
        , (1, return 2)
        , (1, choose (3, 10))
        ]
    -- TODO create a Cmd that advances time, so this is being exercised too.
    expireUnusedAfter <- (fromIntegral :: Int -> DiffTime) <$> choose (1, 100)
    return Index.CacheConfig{Index.pastChunksToCache, Index.expireUnusedAfter}

{-------------------------------------------------------------------------------
  LoE
-------------------------------------------------------------------------------}

instance Arbitrary a => Arbitrary (LoE a) where
  arbitrary = oneof [pure LoEDisabled, LoEEnabled <$> arbitrary]
  shrink LoEDisabled = []
  shrink (LoEEnabled x) = LoEDisabled : map LoEEnabled (shrink x)

{-------------------------------------------------------------------------------
  SecurityParam
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam . unsafeNonZero <$> choose (1, 6)
  shrink (SecurityParam k) = [SecurityParam (unsafeNonZero x) | x <- shrink (unNonZero k), x > 0]

deriving newtype instance Arbitrary (Flag symbol)
