{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Peras.Util.Internal where

import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word (Word64)
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..))
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..), SystemTime (..), WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId)
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import Ouroboros.Network.Block (Point (..), SlotNo (..))
import Ouroboros.Network.Point (Block (..), WithOrigin (..))
import Test.Consensus.Committee.Utils (mkPoolId)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, NonEmptyList (getNonEmpty))
import Test.QuickCheck.Gen (frequency, listOf, listOf1)
import Test.Util.TestBlock (TestBlock, TestHash (..))

genRoundNo :: Gen PerasRoundNo
genRoundNo = PerasRoundNo <$> arbitrary

genSeatIndex :: Gen PerasSeatIndex
genSeatIndex = PerasSeatIndex <$> arbitrary

genPoolId :: Gen PoolId
genPoolId = mkPoolId <$> arbitrary

genLedgerStake :: Gen LedgerStake
genLedgerStake = LedgerStake <$> arbitrary

newtype ListWithUniqueIds a = ListWithUniqueIds [a]
  deriving (Eq, Show, Ord)

newtype NonEmptyListWithUniqueIds a = NonEmptyListWithUniqueIds (NonEmpty a)
  deriving (Eq, Show, Ord)

genListWithUniqueIds :: Ord idTy => (a -> idTy) -> Gen a -> Gen (ListWithUniqueIds a)
genListWithUniqueIds getId genObject = ListWithUniqueIds . nubOrdOn getId <$> listOf genObject

genNonEmptyListWithUniqueIds ::
  Ord idTy => (a -> idTy) -> Gen a -> Gen (NonEmptyListWithUniqueIds a)
genNonEmptyListWithUniqueIds getId genObject = NonEmptyListWithUniqueIds . NonEmpty.fromList . nubOrdOn getId <$> listOf1 genObject

nonEmptyListOf :: Gen a -> Gen (NonEmpty a)
nonEmptyListOf genObject = NonEmpty.fromList <$> listOf1 genObject

{-------------------------------------------------------------------------------
  Shared generators for Peras smoke tests
-------------------------------------------------------------------------------}

genRelativeTime :: Gen RelativeTime
genRelativeTime = RelativeTime . fromIntegral <$> arbitrary @Word64

genWithArrivalTime :: Gen a -> Gen (WithArrivalTime a)
genWithArrivalTime genA = WithArrivalTime <$> genRelativeTime <*> genA

genPointTestBlock :: Gen (Point TestBlock)
genPointTestBlock =
  -- Sometimes pick the genesis point
  frequency
    [ (1, pure $ Point Origin)
    ,
      ( 50
      , do
          slotNo <- SlotNo <$> arbitrary
          hash <- TestHash . NonEmpty.fromList . getNonEmpty <$> arbitrary
          pure $ Point (At (Block slotNo hash))
      )
    ]

-- | A static 'SystemTime' returning a constant time. The canonical mock
-- system time lives in 'Test.Util.LogicalClock.mockSystemTime', but it
-- is a field of 'LogicalClock' which requires a 'ResourceRegistry' and
-- a background tick thread — too heavyweight for simple property tests
-- that don't need time progression.
mockSystemTime :: Applicative m => SystemTime m
mockSystemTime =
  SystemTime
    { systemTimeCurrent = pure (RelativeTime 0)
    , systemTimeWait = pure ()
    }

-- * Tabulators

mkBucket :: Int -> Int -> String -> String
mkBucket bucketSize x suffix
  | lower == upper = show lower <> suffix
  | otherwise = show lower <> "-" <> show upper <> suffix
 where
  lower = (x `div` bucketSize) * bucketSize
  upper = lower + bucketSize
