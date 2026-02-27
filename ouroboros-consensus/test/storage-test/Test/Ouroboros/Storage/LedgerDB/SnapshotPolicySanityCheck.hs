module Test.Ouroboros.Storage.LedgerDB.SnapshotPolicySanityCheck (tests) where

import Cardano.Ledger.BaseTypes (unsafeNonZero)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Word (Word64)
import Ouroboros.Consensus.Block.SupportsSanityCheck
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "SnapshotPolicySanityCheck"
    [ testProperty "SnapshotNumZero fires iff spaNum overridden to 0" $
        prop_num_zero_iff
    , testProperty
        "SnapshotDelayRangeInverted fires iff minimumDelay > maximumDelay (given minimumDelay >= 0)"
        $ prop_delay_range_inverted_iff
    , testProperty "SnapshotDelayRangeNegativeMinimum fires iff minimumDelay < 0" $
        prop_delay_range_negative_minimum_iff
    , testProperty "SnapshotRateLimitDisabled fires iff sfaRateLimit overridden to <= 0" $
        prop_rate_limit_disabled_iff
    , testProperty
        "SnapshotRateLimitSuspiciouslyLarge fires iff sfaRateLimit overridden to > 86400s (given > 0)"
        $ prop_rate_limit_large_iff
    , testProperty "SnapshotIntervalNotDivisorOfEpoch fires iff 432000 mod interval /= 0" $
        prop_mithril_divisibility_iff
    , testProperty "no frequency issues emitted under DisableSnapshots" $
        prop_disable_snapshots_no_frequency_issues
    ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | 'SnapshotNumZero' fires if and only if 'spaNum' is overridden to 0.
-- Uses 'DisableSnapshots' so that no other checks interfere.
prop_num_zero_iff :: Property
prop_num_zero_iff =
  forAll (arbitrary :: Gen Word) $ \n ->
    let issues =
          sanityCheckSnapshotPolicyArgs
            SnapshotPolicyArgs{spaFrequency = DisableSnapshots, spaNum = Override n}
     in (SnapshotNumZero `elem` issues) === (n == 0)

-- | 'SnapshotDelayRangeInverted' fires if and only if @minimumDelay > maximumDelay@,
-- when @minimumDelay >= 0@ (avoiding the 'SnapshotDelayRangeNegativeMinimum' case).
prop_delay_range_inverted_iff :: Property
prop_delay_range_inverted_iff =
  forAll genNonNegativeDiffTime $ \mn ->
    forAll genDiffTime $ \mx ->
      let issues = sanityCheckSnapshotPolicyArgs (withDelayRange (SnapshotDelayRange mn mx))
          fired = any isInverted issues
       in fired === (mn > mx)
 where
  isInverted (SnapshotDelayRangeInverted _ _) = True
  isInverted _ = False

-- | 'SnapshotDelayRangeNegativeMinimum' fires if and only if @minimumDelay < 0@.
prop_delay_range_negative_minimum_iff :: Property
prop_delay_range_negative_minimum_iff =
  forAll genDiffTime $ \mn ->
    let issues = sanityCheckSnapshotPolicyArgs (withDelayRange (SnapshotDelayRange mn 0))
        fired = any isNegativeMin issues
     in fired === (mn < 0)
 where
  isNegativeMin (SnapshotDelayRangeNegativeMinimum _) = True
  isNegativeMin _ = False

-- | 'SnapshotRateLimitDisabled' fires if and only if 'sfaRateLimit' is overridden
-- to a non-positive value.
prop_rate_limit_disabled_iff :: Property
prop_rate_limit_disabled_iff =
  forAll genDiffTime $ \rl ->
    let issues = sanityCheckSnapshotPolicyArgs (withRateLimit rl)
     in (SnapshotRateLimitDisabled `elem` issues) === (rl <= 0)

-- | 'SnapshotRateLimitSuspiciouslyLarge' fires if and only if 'sfaRateLimit' is
-- overridden to a value greater than 86400s. We restrict to positive rate limits
-- to avoid the 'SnapshotRateLimitDisabled' check interfering.
prop_rate_limit_large_iff :: Property
prop_rate_limit_large_iff =
  forAll genPositiveDiffTime $ \rl ->
    let issues = sanityCheckSnapshotPolicyArgs (withRateLimit rl)
        fired = any isLarge issues
     in fired === (rl > 86400)
 where
  isLarge (SnapshotRateLimitSuspiciouslyLarge _) = True
  isLarge _ = False

-- | 'SnapshotIntervalNotDivisorOfEpoch' fires if and only if
-- @'mithrilEpochSize' \`mod\` interval /= 0@.
prop_mithril_divisibility_iff :: Property
prop_mithril_divisibility_iff =
  forAll genNonZeroWord64 $ \n ->
    let issues = sanityCheckSnapshotPolicyArgs (withInterval n)
        fired = any isMithrilIssue issues
     in fired === (mithrilEpochSize `mod` n /= 0)
 where
  isMithrilIssue (SnapshotIntervalNotDivisorOfEpoch _) = True
  isMithrilIssue _ = False

-- | With 'DisableSnapshots', no frequency-related issues are ever emitted,
-- regardless of what 'spaNum' is set to.
prop_disable_snapshots_no_frequency_issues :: Property
prop_disable_snapshots_no_frequency_issues =
  forAll (arbitrary :: Gen Word) $ \n ->
    let issues =
          sanityCheckSnapshotPolicyArgs
            SnapshotPolicyArgs{spaFrequency = DisableSnapshots, spaNum = Override n}
     in filter isFrequencyIssue issues === []
 where
  -- SnapshotNumZero is the only non-frequency issue that can appear here;
  -- everything else would require a SnapshotFrequency.
  isFrequencyIssue SnapshotNumZero = False
  isFrequencyIssue InconsistentSecurityParam{} = False
  isFrequencyIssue _ = True

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- | Generate an arbitrary 'DiffTime' (integer seconds, possibly negative).
genDiffTime :: Gen DiffTime
genDiffTime = secondsToDiffTime <$> arbitrary

-- | Generate a non-negative 'DiffTime'.
genNonNegativeDiffTime :: Gen DiffTime
genNonNegativeDiffTime = secondsToDiffTime . abs <$> arbitrary

-- | Generate a strictly positive 'DiffTime'.
genPositiveDiffTime :: Gen DiffTime
genPositiveDiffTime = secondsToDiffTime . getPositive <$> arbitrary

-- | Generate a non-zero 'Word64'.
genNonZeroWord64 :: Gen Word64
genNonZeroWord64 = getPositive <$> arbitrary

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Build a 'SnapshotPolicyArgs' with a specific 'SnapshotDelayRange' override.
withDelayRange :: SnapshotDelayRange -> SnapshotPolicyArgs
withDelayRange sdr =
  SnapshotPolicyArgs
    { spaFrequency =
        SnapshotFrequency
          SnapshotFrequencyArgs
            { sfaInterval = UseDefault
            , sfaOffset = UseDefault
            , sfaRateLimit = UseDefault
            , sfaDelaySnapshotRange = Override sdr
            }
    , spaNum = UseDefault
    }

-- | Build a 'SnapshotPolicyArgs' with a specific 'sfaRateLimit' override.
withRateLimit :: DiffTime -> SnapshotPolicyArgs
withRateLimit rl =
  SnapshotPolicyArgs
    { spaFrequency =
        SnapshotFrequency
          SnapshotFrequencyArgs
            { sfaInterval = UseDefault
            , sfaOffset = UseDefault
            , sfaRateLimit = Override rl
            , sfaDelaySnapshotRange = UseDefault
            }
    , spaNum = UseDefault
    }

-- | Build a 'SnapshotPolicyArgs' with a specific 'sfaInterval' override.
withInterval :: Word64 -> SnapshotPolicyArgs
withInterval n =
  SnapshotPolicyArgs
    { spaFrequency =
        SnapshotFrequency
          SnapshotFrequencyArgs
            { sfaInterval = Override (unsafeNonZero n)
            , sfaOffset = UseDefault
            , sfaRateLimit = UseDefault
            , sfaDelaySnapshotRange = UseDefault
            }
    , spaNum = UseDefault
    }
