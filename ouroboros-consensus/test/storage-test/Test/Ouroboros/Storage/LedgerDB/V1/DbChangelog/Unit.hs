{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.V1.DbChangelog.Unit (tests) where

import           Cardano.Slotting.Slot (WithOrigin (..), withOrigin)
import           Control.Monad hiding (ap)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict hiding (state)
import           Data.Foldable
import qualified Data.Map.Diff.Strict.Internal as Diff
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ledger.Basics hiding (Key, LedgerState)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import           Ouroboros.Consensus.Ledger.Tables.Diff (fromAntiDiff)
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
                     (DbChangelog (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbChangelog
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (HeaderHash, Point (..), SlotNo (..),
                     StandardHash, castPoint, pattern GenesisPoint)
import qualified Ouroboros.Network.Point as Point
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.QuickCheck (frequency', oneof')
import           Text.Show.Pretty (ppShow)

samples :: Int
samples = 1000

tests :: TestTree
tests = testGroup "DbChangelog"
      [ testProperty "flushing" $ verboseShrinking $ withMaxSuccess samples $ conjoin
        [ counterexample "flushing keeps immutable tip"
          prop_flushingSplitsTheChangelog
        ]
      , testProperty "rolling back" $ withMaxSuccess samples $ conjoin
        [ counterexample "rollback after extension is noop"
          prop_rollbackAfterExtendIsNoop
        , counterexample "prefixing back to anchor is rolling back volatile states"
          prop_prefixBackToAnchorIsRollingBackVolatileStates
        , counterexample "prefix back to volatile tip is a noop"
          prop_rollBackToVolatileTipIsNoop
        ]
      , testProperty "extending adds head to volatile states"
        $ withMaxSuccess samples prop_extendingAdvancesTipOfVolatileStates
      , testProperty "pruning leaves at most maxRollback volatile states"
        $ withMaxSuccess samples prop_pruningLeavesAtMostMaxRollbacksVolatileStates
      ]



{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestLedger (mk :: MapKind) = TestLedger {
  tlUtxos :: mk Key Int,
  tlTip   :: Point TestLedger
}

nextState :: DbChangelog TestLedger -> TestLedger DiffMK
nextState dblog = TestLedger {
              tlTip = pointAtSlot $ nextSlot (getTipSlot old)
            , tlUtxos = DiffMK mempty
            }
  where
    old = DbChangelog.current $ anchorlessChangelog dblog
    nextSlot = At . withOrigin 1 (+1)


deriving instance Show (mk Key Int) => Show (TestLedger mk)

instance GetTip TestLedger where
  getTip = castPoint . tlTip

data H = H deriving (Eq, Ord, Show, Generic)
deriving anyclass instance NoThunks H
type instance HeaderHash TestLedger = H

instance StandardHash TestLedger

deriving instance Eq (TestLedger EmptyMK)

type instance Ledger.Key   TestLedger = Key
type instance Ledger.Value TestLedger = Int

instance HasLedgerTables TestLedger where
  projectLedgerTables                     = LedgerTables . tlUtxos
  withLedgerTables st    (LedgerTables x) = st { tlUtxos = x }

data DbChangelogTestSetup = DbChangelogTestSetup {
  -- The operations are applied on the right, i.e., the newest operation is at the head of the list.
    operations          :: [Operation TestLedger]
  , dbChangelogStartsAt :: WithOrigin SlotNo
  }

data Operation l = Extend (l DiffMK) | Prune SecurityParam
deriving instance Show (l DiffMK) => Show (Operation l)

data DbChangelogTestSetupWithRollbacks = DbChangelogTestSetupWithRollbacks
  { testSetup :: DbChangelogTestSetup
  , rollbacks :: Int
  } deriving (Show)

instance Show DbChangelogTestSetup where
  show = ppShow . operations

instance Arbitrary DbChangelogTestSetup where
  arbitrary = sized $ \n -> do
    slotNo <- oneof [pure Origin, At . SlotNo <$> chooseEnum (1, 1000)]
    ops <- genOperations slotNo n
    pure $ DbChangelogTestSetup
      { operations = ops
      , dbChangelogStartsAt = slotNo
      }

  -- TODO: Shrinking might not be optimal. Shrinking finds the shortest prefix of the list of
  -- operations that result in a failed property, by simply testing prefixes in increasing order.
  shrink setup = reverse $ takeWhileJust $ drop 1 (iterate reduce (Just setup))
    where
      reduce (Just (DbChangelogTestSetup (_:ops) dblog)) = Just $ DbChangelogTestSetup ops dblog
      reduce _ = Nothing
      takeWhileJust = catMaybes . takeWhile isJust

instance Arbitrary DbChangelogTestSetupWithRollbacks where
  arbitrary = do
    setup <- arbitrary
    let dblog = resultingDbChangelog setup
    rolls <- chooseInt (0, AS.length (DbChangelog.adcStates $ DbChangelog.anchorlessChangelog dblog))
    pure $ DbChangelogTestSetupWithRollbacks
      { testSetup = setup
      , rollbacks = rolls
      }

  shrink setupWithRollback = toWithRollbacks <$> setups
    where
      setups = shrink (testSetup setupWithRollback)
      shrinkRollback :: DbChangelogTestSetup -> Int -> Int
      shrinkRollback setup rollback =
        AS.length (DbChangelog.adcStates $ DbChangelog.anchorlessChangelog $ resultingDbChangelog setup) `min` rollback
      toWithRollbacks setup = DbChangelogTestSetupWithRollbacks {
           testSetup = setup
         , rollbacks = shrinkRollback setup (rollbacks setupWithRollback)
         }

resultingDbChangelog :: DbChangelogTestSetup -> DbChangelog TestLedger
resultingDbChangelog setup = applyOperations (operations setup) originalDbChangelog
  where
    originalDbChangelog = DbChangelog.empty $ TestLedger EmptyMK anchor
    anchor = pointAtSlot (dbChangelogStartsAt setup)

applyOperations :: (HasLedgerTables l, GetTip l)
  => [Operation l] -> DbChangelog l -> DbChangelog l
applyOperations ops dblog = foldr' apply' dblog ops
  where apply' (Extend newState) dblog' = DbChangelog.onChangelog (DbChangelog.extend newState) dblog'
        apply' (Prune sp) dblog'        = DbChangelog.onChangelog (DbChangelog.prune sp) dblog'

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Changelog states and diffs appear in one either the changelog to flush or the changelog to
-- keep, moreover, the to flush changelog has no volatile states, and the to keep changelog has no
-- immutable states.
prop_flushingSplitsTheChangelog :: DbChangelogTestSetup -> Property
prop_flushingSplitsTheChangelog setup = isNothing toFlush .||.
    (    toKeepTip            === At toFlushTip
    .&&. fromAntiDiff (cumulativeDiff diffs) === toFlushDiffs <> fromAntiDiff (cumulativeDiff toKeepDiffs)
    )
  where
    dblog                                    = resultingDbChangelog setup
    (toFlush, toKeep)                        = DbChangelog.splitForFlushing dblog
    toFlushTip                               = maybe undefined DbChangelog.toFlushSlot toFlush
    toKeepTip                                = DbChangelog.immutableTipSlot $ anchorlessChangelog toKeep
    LedgerTables (SeqDiffMK toKeepDiffs)  = DbChangelog.adcDiffs $ anchorlessChangelog toKeep
    LedgerTables (DiffMK toFlushDiffs)    = maybe undefined DbChangelog.toFlushDiffs toFlush
    LedgerTables (SeqDiffMK diffs)        = DbChangelog.adcDiffs $ anchorlessChangelog dblog

-- | Extending the changelog adds the correct head to the volatile states.
prop_extendingAdvancesTipOfVolatileStates :: DbChangelogTestSetup -> Property
prop_extendingAdvancesTipOfVolatileStates setup =
  property $ tlTip state == tlTip new
  where
    dblog  = resultingDbChangelog setup
    state  = nextState dblog
    dblog' = DbChangelog.onChangelog (DbChangelog.extend state) dblog
    new    = AS.headAnchor (DbChangelog.adcStates $ anchorlessChangelog dblog')

-- | Rolling back n extensions is the same as doing nothing.
prop_rollbackAfterExtendIsNoop :: DbChangelogTestSetup -> Positive Int -> Property
prop_rollbackAfterExtendIsNoop setup (Positive n) =
    property (dblog == fromJust (DbChangelog.onChangelogM (DbChangelog.rollbackN (fromIntegral n)) $ nExtensions n dblog))
  where
    dblog = resultingDbChangelog setup

-- | The number of volatile states left after pruning is at most the maximum number of rollbacks.
prop_pruningLeavesAtMostMaxRollbacksVolatileStates ::
  DbChangelogTestSetup -> SecurityParam -> Property
prop_pruningLeavesAtMostMaxRollbacksVolatileStates setup sp@(SecurityParam k) =
  property $ AS.length (DbChangelog.adcStates $ anchorlessChangelog dblog') <= fromIntegral k
  where
    dblog = resultingDbChangelog setup
    dblog' = DbChangelog.onChangelog (DbChangelog.prune sp) dblog

-- | The prefixBackToAnchor function rolls back all volatile states.
prop_prefixBackToAnchorIsRollingBackVolatileStates :: DbChangelogTestSetup -> Property
prop_prefixBackToAnchorIsRollingBackVolatileStates setup =
  property $ rolledBack == toAnchor
  where
    dblog = resultingDbChangelog setup
    n = AS.length (DbChangelog.adcStates $ anchorlessChangelog dblog)
    rolledBack = fromJust $ DbChangelog.onChangelogM (DbChangelog.rollbackN (fromIntegral n)) dblog
    toAnchor = DbChangelog.onChangelog DbChangelog.rollbackToAnchor dblog

-- | Rolling back to the last state is the same as doing nothing.
prop_rollBackToVolatileTipIsNoop ::
  Positive Int -> DbChangelogTestSetup -> Property
prop_rollBackToVolatileTipIsNoop (Positive n) setup = property $ Just dblog == dblog'
  where
    dblog = resultingDbChangelog setup
    pt = getTip $ DbChangelog.current $ anchorlessChangelog dblog
    dblog' = DbChangelog.onChangelogM (DbChangelog.rollbackToPoint pt) $ nExtensions n dblog

nExtensions :: Int -> DbChangelog TestLedger -> DbChangelog TestLedger
nExtensions n dblog = iterate ext dblog !! n
  where ext dblog' = DbChangelog.onChangelog (DbChangelog.extend (nextState dblog')) dblog'

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

pointAtSlot :: WithOrigin SlotNo -> Point TestLedger
pointAtSlot = Point.withOrigin GenesisPoint (\slotNo -> Point $ At $ Point.Block slotNo H)

type Key = String

data GenOperationsState = GenOperationsState {
    gosSlotNo            :: !(WithOrigin SlotNo)
  , gosOps               :: ![Operation TestLedger]
  , gosActiveUtxos       :: !(Map Key Int)
  , gosPendingInsertions :: !(Map Key Int)
  , gosConsumedUtxos     :: !(Set Key)
  } deriving (Show)

applyPending :: GenOperationsState -> GenOperationsState
applyPending gosState = gosState
  { gosActiveUtxos = Map.union (gosActiveUtxos gosState) (gosPendingInsertions gosState)
  , gosPendingInsertions = Map.empty
  }

genOperations :: WithOrigin SlotNo -> Int -> Gen [Operation TestLedger]
genOperations slotNo nOps = gosOps <$> execStateT (replicateM_ nOps genOperation) initState
  where
    initState = GenOperationsState {
        gosSlotNo = slotNo
      , gosActiveUtxos = Map.empty
      , gosPendingInsertions = Map.empty
      , gosConsumedUtxos = Set.empty
      , gosOps = []
      }

    genOperation :: StateT GenOperationsState Gen ()
    genOperation = do
      op <- frequency' [ (1, genPrune), (10, genExtend) ]
      modify' $ \st -> st { gosOps = op:gosOps st }

    genPrune :: StateT GenOperationsState Gen (Operation TestLedger)
    genPrune = Prune . SecurityParam <$> lift (chooseEnum (0, 10))

    genExtend :: StateT GenOperationsState Gen (Operation TestLedger)
    genExtend = do
      nextSlotNo <- advanceSlotNo =<< lift (chooseEnum (1, 5))
      d <- genUtxoDiff
      pure $ Extend $ TestLedger (DiffMK $ fromAntiDiff d) (castPoint $ pointAtSlot nextSlotNo)

    advanceSlotNo :: SlotNo -> StateT GenOperationsState Gen (WithOrigin SlotNo)
    advanceSlotNo by = do
      nextSlotNo <- gets (At . Point.withOrigin by (+ by) . gosSlotNo)
      modify' $ \st -> st { gosSlotNo = nextSlotNo }
      pure nextSlotNo

    genUtxoDiff :: StateT GenOperationsState Gen (Diff.Diff Key Int)
    genUtxoDiff = do
      nEntries <- lift $ chooseInt (1, 10)
      entries <- replicateM nEntries genUtxoDiffEntry
      modify' applyPending
      pure $ Diff.fromList entries

    genUtxoDiffEntry :: StateT GenOperationsState Gen (Key, Diff.Delta Int)
    genUtxoDiffEntry = do
      activeUtxos <- gets gosActiveUtxos
      consumedUtxos <- gets gosConsumedUtxos
      oneof' $ catMaybes [
        genDelEntry activeUtxos,
        genInsertEntry consumedUtxos]

    genDelEntry :: Map Key Int -> Maybe (StateT GenOperationsState Gen (Key, Diff.Delta Int))
    genDelEntry activeUtxos =
      if Map.null activeUtxos then Nothing
      else Just $ do
        (k, _) <- lift $ elements (Map.toList activeUtxos)
        modify' $ \st -> st
          { gosActiveUtxos = Map.delete k (gosActiveUtxos st)
          }
        pure (k, Diff.Delete)

    genInsertEntry :: Set Key -> Maybe (StateT GenOperationsState Gen (Key, Diff.Delta Int))
    genInsertEntry consumedUtxos = Just $ do
      k <- lift $ genKey `suchThat` (`Set.notMember` consumedUtxos)
      v <- lift arbitrary
      modify' $ \st -> st
        { gosPendingInsertions = Map.insert k v (gosPendingInsertions st)
        , gosConsumedUtxos = Set.insert k (gosConsumedUtxos st)
        }
      pure (k, Diff.Insert v)

genKey :: Gen Key
genKey = replicateM 2 $ elements ['A'..'Z']
