{-# LANGUAGE LambdaCase #-}

-- | This module adds support for sanity checking consensus configuration
--   on node startup. These checks should primarily look for unusual
--   configuration choices that may point to an accidentally-misconfigured node
--   and quietly cause problems, rather than incoherent configurations that will
--   result in fatal errors at a later point.
--
--   While in most situations they can be handled as fatal issues, there are
--   situations when intentionally configuring a node "weirdly" can be useful,
--   and so the user should be able to opt out of the sanity checks at their
--   own peril.
module Ouroboros.Consensus.Block.SupportsSanityCheck
  ( BlockSupportsSanityCheck (..)
  , SanityCheckIssue (..)
  , checkSecurityParamConsistency
  , sanityCheckConfig
  ) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Time.Clock (DiffTime)
import Data.Word (Word64)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Config.SecurityParam

-- | An issue found in the consensus configuration. See 'displayException'
--   for human-readable descriptions of each of these cases, especially when
--   presenting these to users.
data SanityCheckIssue
  = -- | Configuration contains multiple security parameters. This may cause
    --   strange behaviour around era boundaries.
    InconsistentSecurityParam (NonEmpty SecurityParam)
  | -- | The configured 'minimumDelay' in 'SnapshotDelayRange' is greater than
    --   'maximumDelay'. The random snapshot delay will be sampled from an
    --   inverted range, which is almost certainly a misconfiguration.
    SnapshotDelayRangeInverted
      -- | The configured minimumDelay (the larger value)
      !DiffTime
      -- | The configured maximumDelay (the smaller value)
      !DiffTime
  | -- | The configured 'minimumDelay' in 'SnapshotDelayRange' is negative.
    --   A negative delay has no meaningful interpretation.
    SnapshotDelayRangeNegativeMinimum
      -- | The negative minimumDelay
      !DiffTime
  | -- | The configured 'sfaRateLimit' is non-positive, which disables snapshot
    --   rate limiting entirely. Without a rate limit, snapshots may be taken
    --   very frequently during bulk sync, causing excessive disk I/O.
    SnapshotRateLimitDisabled
  | -- | The configured 'sfaRateLimit' exceeds 24 hours. At steady state, the
    --   node may go more than a day between snapshots, significantly increasing
    --   replay time after an unclean restart.
    SnapshotRateLimitSuspiciouslyLarge
      -- | The configured rate limit
      !DiffTime
  | -- | The configured number of on-disk snapshots to keep is zero. Snapshots
    --   will be written to disk and then immediately deleted, leaving nothing
    --   for crash recovery. The node will have to replay from genesis on every
    --   unclean restart.
    SnapshotNumZero
  | -- | The configured snapshot interval does not divide 432000 (the Cardano
    --   mainnet epoch length in slots). Snapshots will not land on epoch
    --   boundaries, breaking Mithril compatibility.
    SnapshotIntervalNotDivisorOfEpoch
      -- | The configured interval in slots
      !Word64
  deriving (Show, Eq)

instance Exception SanityCheckIssue where
  displayException = \case
    InconsistentSecurityParam ks ->
      mconcat
        [ "InconsistentSecurityParam: "
        , "SecurityParams (K) were found to be inconsistent between constituent "
        , "eras of a HardForkBlock: "
        , show (NonEmpty.toList ks)
        ]
    SnapshotDelayRangeInverted mn mx ->
      mconcat
        [ "SnapshotDelayRangeInverted: "
        , "The configured snapshot delay range has minimumDelay ("
        , show mn
        , ") greater than maximumDelay ("
        , show mx
        , "). The random snapshot delay will be sampled from an inverted range. "
        , "Please ensure minimumDelay <= maximumDelay in sfaDelaySnapshotRange."
        ]
    SnapshotDelayRangeNegativeMinimum mn ->
      mconcat
        [ "SnapshotDelayRangeNegativeMinimum: "
        , "The configured snapshot delay range has a negative minimumDelay: "
        , show mn
        , ". A negative delay has no meaningful interpretation. "
        , "Please set minimumDelay to a non-negative value in sfaDelaySnapshotRange."
        ]
    SnapshotRateLimitDisabled ->
      mconcat
        [ "SnapshotRateLimitDisabled: "
        , "The configured sfaRateLimit is non-positive, which disables snapshot "
        , "rate limiting entirely. Without a rate limit, snapshots may be taken "
        , "very frequently during bulk sync, causing excessive disk I/O. "
        , "The default rate limit is 10 minutes."
        ]
    SnapshotRateLimitSuspiciouslyLarge rl ->
      mconcat
        [ "SnapshotRateLimitSuspiciouslyLarge: "
        , "The configured sfaRateLimit ("
        , show rl
        , ") exceeds 24 hours. At steady state, the node may go more than a day "
        , "between snapshots, significantly increasing replay time after an "
        , "unclean restart. The default rate limit is 10 minutes."
        ]
    SnapshotNumZero ->
      mconcat
        [ "SnapshotNumZero: "
        , "The configured number of on-disk snapshots to keep (spaNum) is 0. "
        , "Snapshots will be written to disk and immediately deleted, leaving "
        , "nothing for crash recovery. The node will have to replay the chain "
        , "from genesis on every unclean restart. "
        , "Consider setting spaNum to at least 2 (the default)."
        ]
    SnapshotIntervalNotDivisorOfEpoch interval ->
      mconcat
        [ "SnapshotIntervalNotDivisorOfEpoch: "
        , "The configured sfaInterval ("
        , show interval
        , " slots) does not evenly divide the Cardano mainnet epoch length "
        , "(432000 slots). Snapshots will not consistently land on epoch "
        , "boundaries, which breaks Mithril compatibility. "
        , "Consider using an interval that divides 432000 evenly, "
        , "such as 4320 (the default, = 2k for k=2160)."
        ]

-- | 'BlockSupportsSanityCheck' provides evidence that a block can be sanity
--   checked for common issues on node startup. 'sanityCheckConfig', which runs
--   performs each check and returns a list with each 'SanityCheckIssue' found,
--   should be preferred over using these methods directly.
class BlockSupportsSanityCheck blk where
  -- | Generate a 'NonEmpty' list of security parameters for a given block type.
  --   For individual eras' block types, this is simply a singleton list
  --   containing the chosen 'SecurityParam', but combined block types (i.e.
  --   the 'HardForkCombinator') will return all of their constituent eras'
  --   configurations' security parameters.
  configAllSecurityParams ::
    TopLevelConfig blk ->
    NonEmpty SecurityParam

-- | Check a 'TopLevelConfig' for any inconsistency in constituent choices for
--   'SecurityParam' (colloquially @k@). For a block type to be considered
--   "sane" in this regard, its configuration's security parameter as well as
--   all of its childrens' configurations (if applicable) should be the same.
checkSecurityParamConsistency ::
  BlockSupportsSanityCheck blk =>
  TopLevelConfig blk ->
  Maybe SanityCheckIssue
checkSecurityParamConsistency cfg = do
  let allParams = configAllSecurityParams cfg
  if allSame allParams
    then Nothing
    else Just (InconsistentSecurityParam allParams)

allSame :: Eq a => NonEmpty a -> Bool
allSame (x :| xs) = all (x ==) xs

-- | Run all supported sanity checks on a given 'TopLevelConfig'.
sanityCheckConfig ::
  BlockSupportsSanityCheck blk =>
  TopLevelConfig blk ->
  [SanityCheckIssue]
sanityCheckConfig cfg =
  catMaybes [checkSecurityParamConsistency cfg]
