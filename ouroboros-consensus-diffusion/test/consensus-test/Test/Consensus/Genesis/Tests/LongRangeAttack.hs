{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Ouroboros.Consensus.Block.Abstract (HeaderHash)
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)

tests :: TestTree
tests = testProperty "long range attack" prop_longRangeAttack

genChainsAndSchedule :: Word -> ScheduleType -> QC.Gen (GenesisTest, PointSchedule)
genChainsAndSchedule numAdversaries scheduleType =
  unsafeMapSuchThatJust do
    gt <- genChains numAdversaries
    pure $ ((gt,) <$> genSchedule scheduleType (gtBlockTree gt))

prop_longRangeAttack :: QC.Gen QC.Property
prop_longRangeAttack = do
  (genesisTest, schedule) <- genChainsAndSchedule 1 FastAdversary
  let cls = classifiers genesisTest

  pure $ withMaxSuccess 10 $ runSimOrThrow $
    runTest genesisTest schedule $ \fragment ->
        classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection"
        -- This is the expected behavior of Praos to be reversed with Genesis.
        -- But we are testing Praos for the moment
        $ existsSelectableAdversary cls ==> not $ isHonestTestFragH fragment
        -- TODO
        --  $ not (existsSelectableAdversary cls) ==> immutableTipBeforeFork fragment
  where
    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
