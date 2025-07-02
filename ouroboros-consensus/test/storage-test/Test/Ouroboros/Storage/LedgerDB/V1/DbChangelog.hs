{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Db changelog ledger DB tests.
--
-- The in-memory component of the ledger DB is a bit tricky: it stores only a
-- few snapshots of the ledger state, in order to reduce memory footprint, but
-- must nonetheless be able to construct any ledger state (within @k@ blocks
-- from the chain tip) efficiently. The properties we are verify here are
-- various invariants of this data type, things such as
--
-- * Rolling back and then reapplying the same blocks is an identity operation
--   (provided the rollback is not too far)
-- * The shape of the datatype (where we store snapshots and how many we store)
--   always matches the policy set by the user, and is invariant under any of
--   the operations (add a block, switch to a fork, etc.)
-- * The maximum rollback supported is always @k@ (unless we are near genesis)
-- * etc.
module Test.Ouroboros.Storage.LedgerDB.V1.DbChangelog (tests) where

import Cardano.Ledger.BaseTypes (NonZero (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad hiding (ap)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict hiding (state)
import Data.Foldable
import qualified Data.Map.Diff.Strict.Internal as Diff
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog hiding
  ( tip
  )
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq as DS
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IndexedMemPack
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Block (Point (..))
import qualified Ouroboros.Network.Point as Point
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck hiding (elements)
import Test.Util.Orphans.Arbitrary ()
import Test.Util.QuickCheck
import qualified Test.Util.TestBlock as TestBlock
import Text.Show.Pretty (ppShow)

samples :: Int
samples = 1000

tests :: TestTree
tests =
  testGroup
    "DbChangelog"
    [ testGroup
        "Genesis"
        [ testProperty "current" prop_genesisCurrent
        ]
    , testGroup
        "Push"
        [ testProperty "expectedLedger" prop_pushExpectedLedger
        ]
    , testGroup
        "Rollback"
        [ testProperty "maxRollbackGenesisZero" prop_maxRollbackGenesisZero
        , testProperty "switchSameChain" prop_switchSameChain
        , testProperty "switchExpectedLedger" prop_switchExpectedLedger
        ]
    , testProperty "flushing" $
        withMaxSuccess samples $
          conjoin
            [ counterexample
                "flushing keeps immutable tip"
                prop_flushingSplitsTheChangelog
            ]
    , testProperty "rolling back" $
        withMaxSuccess samples $
          conjoin
            [ counterexample
                "rollback after extension is noop"
                prop_rollbackAfterExtendIsNoop
            , counterexample
                "prefixing back to anchor is rolling back volatile states"
                prop_rollbackToAnchorIsRollingBackVolatileStates
            , counterexample
                "prefix back to volatile tip is a noop"
                prop_rollBackToVolatileTipIsNoop
            ]
    , testProperty "extending adds head to volatile states" $
        withMaxSuccess samples prop_extendingAdvancesTipOfVolatileStates
    , testProperty "pruning before a slot works as expected" $
        withMaxSuccess samples prop_pruningBeforeSlotCorrectness
    ]

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

prop_genesisCurrent :: Property
prop_genesisCurrent =
  current genSnaps === convertMapKind TestBlock.testInitLedger
 where
  genSnaps = empty (convertMapKind TestBlock.testInitLedger)

{-------------------------------------------------------------------------------
  Constructing snapshots
-------------------------------------------------------------------------------}

prop_pushExpectedLedger :: ChainSetup -> Property
prop_pushExpectedLedger setup@ChainSetup{..} =
  classify (chainSetupSaturated setup) "saturated" $
    conjoin
      [ l
          === convertMapKind
            (refoldLedger OmitLedgerEvents cfg (expectedChain o) (convertMapKind TestBlock.testInitLedger))
      | (o, l) <- snapshots csPushed
      ]
 where
  expectedChain :: Word64 -> [TestBlock.TestBlock]
  expectedChain o = take (fromIntegral (csNumBlocks - o)) csChain

  cfg :: LedgerConfig TestBlock.TestBlock
  cfg = ledgerDbCfg (csBlockConfig setup)

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

prop_maxRollbackGenesisZero :: Property
prop_maxRollbackGenesisZero =
  maxRollback (empty (convertMapKind TestBlock.testInitLedger))
    === 0

prop_switchSameChain :: SwitchSetup -> Property
prop_switchSameChain setup@SwitchSetup{..} =
  classify (switchSetupSaturated setup) "saturated" $
    switch' (csBlockConfig ssChainSetup) ssNumRollback blockInfo csPushed
      === Just csPushed
 where
  ChainSetup{csPushed} = ssChainSetup
  blockInfo = ssRemoved

prop_switchExpectedLedger :: SwitchSetup -> Property
prop_switchExpectedLedger setup@SwitchSetup{..} =
  classify (switchSetupSaturated setup) "saturated" $
    conjoin
      [ l
          === convertMapKind
            (refoldLedger OmitLedgerEvents cfg (expectedChain o) (convertMapKind TestBlock.testInitLedger))
      | (o, l) <- snapshots ssSwitched
      ]
 where
  expectedChain :: Word64 -> [TestBlock.TestBlock]
  expectedChain o = take (fromIntegral (ssNumBlocks - o)) ssChain

  cfg :: LedgerConfig TestBlock.TestBlock
  cfg = ledgerDbCfg (csBlockConfig ssChainSetup)

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ChainSetup = ChainSetup
  { csSecParam :: SecurityParam
  -- ^ Security parameter
  , csNumBlocks :: Word64
  -- ^ Number of blocks applied
  , csPrefixLen :: Word64
  -- ^ Some prefix of the chain
  --
  -- Although we choose this to be less than or equal to 'csNumBlocks',
  -- we don't guarantee this during shrinking. If 'csPrefixLen' is larger
  -- than 'csNumBlocks', the prefix should simply be considered to be the
  -- entire chain.
  , csGenSnaps :: DbChangelog (LedgerState TestBlock.TestBlock)
  -- ^ Derived: genesis snapshots
  , csChain :: [TestBlock.TestBlock]
  -- ^ Derived: the actual blocks that got applied (old to new)
  , csPushed :: DbChangelog (LedgerState TestBlock.TestBlock)
  -- ^ Derived: the snapshots after all blocks were applied
  }
  deriving Show

csBlockConfig :: ChainSetup -> LedgerDbCfg (LedgerState TestBlock.TestBlock)
csBlockConfig = csBlockConfig' . csSecParam

csBlockConfig' :: SecurityParam -> LedgerDbCfg (LedgerState TestBlock.TestBlock)
csBlockConfig' secParam =
  LedgerDbCfg
    { ledgerDbCfgSecParam = secParam
    , ledgerDbCfg =
        TestBlock.testBlockLedgerConfigFrom $
          HardFork.defaultEraParams secParam slotLength
    , ledgerDbCfgComputeLedgerEvents = OmitLedgerEvents
    }
 where
  slotLength = slotLengthFromSec 20

chainSetupSaturated :: ChainSetup -> Bool
chainSetupSaturated ChainSetup{..} = isSaturated csSecParam csPushed

data SwitchSetup = SwitchSetup
  { ssChainSetup :: ChainSetup
  -- ^ Chain setup
  , ssNumRollback :: Word64
  -- ^ Number of blocks to roll back
  , ssNumNew :: Word64
  -- ^ Number of new blocks (to be applied after the rollback)
  , ssPrefixLen :: Word64
  -- ^ Prefix of the new chain
  --
  -- See also 'csPrefixLen'
  , ssNumBlocks :: Word64
  -- ^ Derived: number of blocks in the new chain
  , ssRemoved :: [TestBlock.TestBlock]
  -- ^ Derived: the blocks that were removed
  , ssNewBlocks :: [TestBlock.TestBlock]
  -- ^ Derived: the new blocks themselves
  , ssChain :: [TestBlock.TestBlock]
  -- ^ Derived: the full chain after switching to this fork
  , ssSwitched :: DbChangelog (LedgerState TestBlock.TestBlock)
  -- ^ Derived; the snapshots after the switch was performed
  }
  deriving Show

switchSetupSaturated :: SwitchSetup -> Bool
switchSetupSaturated = chainSetupSaturated . ssChainSetup

mkTestSetup :: SecurityParam -> Word64 -> Word64 -> ChainSetup
mkTestSetup csSecParam csNumBlocks csPrefixLen =
  ChainSetup{..}
 where
  csGenSnaps = empty (convertMapKind TestBlock.testInitLedger)
  csChain =
    take (fromIntegral csNumBlocks) $
      iterate TestBlock.successorBlock (TestBlock.firstBlock 0)
  csPushed = reapplyThenPushMany' (csBlockConfig' csSecParam) csChain csGenSnaps

mkRollbackSetup :: ChainSetup -> Word64 -> Word64 -> Word64 -> SwitchSetup
mkRollbackSetup ssChainSetup ssNumRollback ssNumNew ssPrefixLen =
  SwitchSetup{..}
 where
  ChainSetup{..} = ssChainSetup

  ssNumBlocks = csNumBlocks - ssNumRollback + ssNumNew
  ssRemoved = takeLast ssNumRollback csChain
  ssNewBlocks =
    let afterRollback = dropLast ssNumRollback csChain
        firstAfterRollback =
          case lastMaybe afterRollback of
            Nothing -> TestBlock.firstBlock 1
            Just b -> TestBlock.modifyFork (+ 1) $ TestBlock.successorBlock b
     in take (fromIntegral ssNumNew) $
          iterate TestBlock.successorBlock firstAfterRollback
  ssChain =
    concat
      [ take (fromIntegral (csNumBlocks - ssNumRollback)) csChain
      , ssNewBlocks
      ]
  ssSwitched = fromJust $ switch' (csBlockConfig ssChainSetup) ssNumRollback ssNewBlocks csPushed

instance Arbitrary ChainSetup where
  arbitrary = do
    secParam <- arbitrary
    let k = unNonZero $ maxRollbacks secParam
    numBlocks <- choose (0, k * 2)
    prefixLen <- choose (0, numBlocks)
    return $ mkTestSetup secParam numBlocks prefixLen

  shrink ChainSetup{..} =
    concat
      [ -- Shrink the policy
        [ mkTestSetup csSecParam' csNumBlocks csPrefixLen
        | csSecParam' <- shrink csSecParam
        ]
      , -- Reduce number of blocks
        [ mkTestSetup csSecParam csNumBlocks' csPrefixLen
        | csNumBlocks' <- shrink csNumBlocks
        ]
      ]

instance Arbitrary SwitchSetup where
  arbitrary = do
    chainSetup <- arbitrary
    numRollback <- choose (0, maxRollback (csPushed chainSetup))
    numNew <- choose (numRollback, 2 * numRollback)
    prefixLen <- choose (0, csNumBlocks chainSetup - numRollback + numNew)
    return $ mkRollbackSetup chainSetup numRollback numNew prefixLen

  shrink SwitchSetup{..} =
    concat
      [ -- If we shrink the chain setup, we might restrict max rollback
        [ mkRollbackSetup ssChainSetup' ssNumRollback ssNumNew ssPrefixLen
        | ssChainSetup' <- shrink ssChainSetup
        , ssNumRollback <= maxRollback (csPushed ssChainSetup')
        ]
      , -- Number of new blocks must be at least the rollback
        [ mkRollbackSetup ssChainSetup ssNumRollback ssNumNew' ssPrefixLen
        | ssNumNew' <- shrink ssNumNew
        , ssNumNew' >= ssNumRollback
        ]
      , -- But rolling back less is always possible
        [ mkRollbackSetup ssChainSetup ssNumRollback' ssNumNew ssPrefixLen
        | ssNumRollback' <- shrink ssNumRollback
        ]
      ]

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestLedger (mk :: MapKind) = TestLedger
  { tlUtxos :: mk Key Int
  , tlTip :: Point TestLedger
  }

nextState :: DbChangelog TestLedger -> TestLedger DiffMK
nextState dblog =
  TestLedger
    { tlTip = pointAtSlot $ nextSlot (getTipSlot old)
    , tlUtxos = DiffMK mempty
    }
 where
  old = DbChangelog.current dblog
  nextSlot = At . withOrigin 1 (+ 1)

deriving instance Show (mk Key Int) => Show (TestLedger mk)

instance GetTip TestLedger where
  getTip = castPoint . tlTip

data H = H deriving (Eq, Ord, Show, Generic)
deriving anyclass instance NoThunks H
type instance HeaderHash TestLedger = H

instance StandardHash TestLedger

deriving instance Eq (TestLedger EmptyMK)

type instance TxIn TestLedger = Key
type instance TxOut TestLedger = Int

instance HasLedgerTables TestLedger where
  projectLedgerTables = LedgerTables . tlUtxos
  withLedgerTables st (LedgerTables x) = st{tlUtxos = x}

instance IndexedMemPack (TestLedger EmptyMK) Int where
  indexedTypeName _ = typeName @Int
  indexedPackedByteCount _ = packedByteCount
  indexedPackM _ = packM
  indexedUnpackM _ = unpackM

data DbChangelogTestSetup = DbChangelogTestSetup
  { -- The operations are applied on the right, i.e., the newest operation is at the head of the list.
    operations :: [Operation TestLedger]
  , dbChangelogStartsAt :: WithOrigin SlotNo
  }

data Operation l = Extend (l DiffMK) | Prune
deriving instance Show (l DiffMK) => Show (Operation l)

data DbChangelogTestSetupWithRollbacks = DbChangelogTestSetupWithRollbacks
  { testSetup :: DbChangelogTestSetup
  , rollbacks :: Int
  }
  deriving Show

instance Show DbChangelogTestSetup where
  show = ppShow . operations

instance Arbitrary DbChangelogTestSetup where
  arbitrary = sized $ \n -> do
    slotNo <- oneof [pure Origin, At . SlotNo <$> chooseEnum (1, 1000)]
    ops <- genOperations slotNo n
    pure $
      DbChangelogTestSetup
        { operations = ops
        , dbChangelogStartsAt = slotNo
        }

  -- Shrinking finds the shortest prefix of the list of operations that result
  -- in a failed property, by simply testing prefixes in increasing order.
  shrink setup = reverse $ takeWhileJust $ drop 1 (iterate reduce (Just setup))
   where
    reduce (Just (DbChangelogTestSetup (_ : ops) dblog)) = Just $ DbChangelogTestSetup ops dblog
    reduce _ = Nothing
    takeWhileJust = catMaybes . takeWhile isJust

instance Arbitrary DbChangelogTestSetupWithRollbacks where
  arbitrary = do
    setup <- arbitrary
    let dblog = resultingDbChangelog setup
    rolls <- chooseInt (0, AS.length (DbChangelog.changelogStates dblog))
    pure $
      DbChangelogTestSetupWithRollbacks
        { testSetup = setup
        , rollbacks = rolls
        }

  shrink setupWithRollback = toWithRollbacks <$> setups
   where
    setups = shrink (testSetup setupWithRollback)
    shrinkRollback :: DbChangelogTestSetup -> Int -> Int
    shrinkRollback setup rollbacks =
      AS.length (DbChangelog.changelogStates $ resultingDbChangelog setup) `min` rollbacks
    toWithRollbacks setup =
      DbChangelogTestSetupWithRollbacks
        { testSetup = setup
        , rollbacks = shrinkRollback setup (rollbacks setupWithRollback)
        }

resultingDbChangelog :: DbChangelogTestSetup -> DbChangelog TestLedger
resultingDbChangelog setup = applyOperations (operations setup) originalDbChangelog
 where
  originalDbChangelog = DbChangelog.empty $ TestLedger EmptyMK theAnchor
  theAnchor = pointAtSlot (dbChangelogStartsAt setup)

applyOperations ::
  (HasLedgerTables l, GetTip l) =>
  [Operation l] -> DbChangelog l -> DbChangelog l
applyOperations ops dblog = foldr' apply' dblog ops
 where
  apply' (Extend newState) dblog' = DbChangelog.extend newState dblog'
  apply' Prune dblog' = DbChangelog.pruneToImmTipOnly dblog'

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Changelog states and diffs appear in one either the changelog to flush or the changelog to
-- keep, moreover, the to flush changelog has no volatile states, and the to keep changelog has no
-- immutable states.
prop_flushingSplitsTheChangelog :: DbChangelogTestSetup -> Property
prop_flushingSplitsTheChangelog setup =
  isNothing toFlush
    .||. ( toKeepTip === At toFlushTip
             .&&. DS.fromAntiDiff (DS.cumulativeDiff diffs)
               === toFlushDiffs <> DS.fromAntiDiff (DS.cumulativeDiff toKeepDiffs)
         )
 where
  dblog = resultingDbChangelog setup
  (toFlush, toKeep) = DbChangelog.splitForFlushing dblog
  toFlushTip = maybe undefined DbChangelog.toFlushSlot toFlush
  toKeepTip = DbChangelog.immutableTipSlot toKeep
  LedgerTables (SeqDiffMK toKeepDiffs) = DbChangelog.changelogDiffs toKeep
  LedgerTables (DiffMK toFlushDiffs) = maybe undefined DbChangelog.toFlushDiffs toFlush
  LedgerTables (SeqDiffMK diffs) = DbChangelog.changelogDiffs dblog

-- | Extending the changelog adds the correct head to the volatile states.
prop_extendingAdvancesTipOfVolatileStates :: DbChangelogTestSetup -> Property
prop_extendingAdvancesTipOfVolatileStates setup =
  property $ tlTip state == tlTip new
 where
  dblog = resultingDbChangelog setup
  state = nextState dblog
  dblog' = DbChangelog.extend state dblog
  new = AS.headAnchor (DbChangelog.changelogStates dblog')

-- | Rolling back n extensions is the same as doing nothing.
prop_rollbackAfterExtendIsNoop :: DbChangelogTestSetup -> Positive Int -> Property
prop_rollbackAfterExtendIsNoop setup (Positive n) =
  property (dblog == fromJust (DbChangelog.rollbackN (fromIntegral n) $ nExtensions n dblog))
 where
  dblog = resultingDbChangelog setup

-- | When pruning after a slot, all (non-anchor) states are not older than this
-- slot, and the anchor /is/ older (unless nothing was pruned).
prop_pruningBeforeSlotCorrectness ::
  DbChangelogTestSetup -> Property
prop_pruningBeforeSlotCorrectness setup =
  counterexample ("dblog: " <> show dblog) $ forAll genPruneSlot $ \pruneSlot ->
    let dblog' = DbChangelog.prune (LedgerDbPruneBeforeSlot pruneSlot) dblog
     in counterexample ("pruned dblog: " <> show dblog') $
          conjoin
            [ counterexample "State not pruned unexpectedly" $
                conjoin
                  [ (NotOrigin pruneSlot `le` getTipSlot st)
                  | (_, st) <-
                      DbChangelog.snapshots dblog'
                  ]
            , counterexample "Anchor too old" $
                let nothingPruned = DbChangelog.maxRollback dblog == DbChangelog.maxRollback dblog'
                 in if nothingPruned
                      then property ()
                      else
                        getTipSlot (DbChangelog.anchor dblog') `lt` NotOrigin pruneSlot
            ]
 where
  dblog = resultingDbChangelog setup

  genPruneSlot = chooseEnum (lb, ub)
   where
    jitter = 5
    lb
      | anchorSlot >= jitter = anchorSlot - jitter
      | otherwise = 0
     where
      anchorSlot = succWithOrigin $ getTipSlot $ DbChangelog.anchor dblog
    ub = succWithOrigin (pointSlot (DbChangelog.tip dblog)) + jitter

-- | The rollbackToAnchor function rolls back all volatile states.
prop_rollbackToAnchorIsRollingBackVolatileStates :: DbChangelogTestSetup -> Property
prop_rollbackToAnchorIsRollingBackVolatileStates setup =
  property $ rolledBack == toAnchor
 where
  dblog = resultingDbChangelog setup
  n = AS.length (DbChangelog.changelogStates dblog)
  rolledBack = fromJust $ DbChangelog.rollbackN (fromIntegral n) dblog
  toAnchor = DbChangelog.rollbackToAnchor dblog

-- | Rolling back to the last state is the same as doing nothing.
prop_rollBackToVolatileTipIsNoop ::
  Positive Int -> DbChangelogTestSetup -> Property
prop_rollBackToVolatileTipIsNoop (Positive n) setup = property $ Just dblog == dblog'
 where
  dblog = resultingDbChangelog setup
  pt = getTip $ DbChangelog.current dblog
  dblog' = DbChangelog.rollbackToPoint pt $ nExtensions n dblog

nExtensions :: Int -> DbChangelog TestLedger -> DbChangelog TestLedger
nExtensions n dblog = iterate ext dblog !! n
 where
  ext dblog' = DbChangelog.extend (nextState dblog') dblog'

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

pointAtSlot :: WithOrigin SlotNo -> Point TestLedger
pointAtSlot = Point.withOrigin GenesisPoint (\slotNo -> Point $ At $ Point.Block slotNo H)

type Key = T.Text

data GenOperationsState = GenOperationsState
  { gosSlotNo :: !(WithOrigin SlotNo)
  -- ^ The current slot number on the sequence of generated operations
  , gosOps :: ![Operation TestLedger]
  -- ^ Accumulation of operations
  , gosActiveUtxos :: !(Map Key Int)
  -- ^ UTxOs in the UTxO set
  , gosPendingInsertions :: !(Map Key Int)
  -- ^ UTxOs for which an insertion has been generated
  --
  -- Just after generation, they will be moved to 'gosActiveUtxos'
  , gosConsumedUtxos :: !(Set Key)
  -- ^ UTxOs for which a delete has been generated
  }
  deriving Show

applyPending :: GenOperationsState -> GenOperationsState
applyPending gosState =
  gosState
    { gosActiveUtxos = Map.union (gosActiveUtxos gosState) (gosPendingInsertions gosState)
    , gosPendingInsertions = Map.empty
    }

genOperations :: WithOrigin SlotNo -> Int -> Gen [Operation TestLedger]
genOperations slotNo nOps = gosOps <$> execStateT (replicateM_ nOps genOperation) initState
 where
  initState =
    GenOperationsState
      { gosSlotNo = slotNo
      , gosActiveUtxos = Map.empty
      , gosPendingInsertions = Map.empty
      , gosConsumedUtxos = Set.empty
      , gosOps = []
      }

  genOperation :: StateT GenOperationsState Gen ()
  genOperation = do
    op <- frequency' [(1, pure Prune), (20, genExtend)]
    modify' $ \st -> st{gosOps = op : gosOps st}

  genExtend :: StateT GenOperationsState Gen (Operation TestLedger)
  genExtend = do
    nextSlotNo <- advanceSlotNo =<< lift (chooseEnum (1, 5))
    d <- genUtxoDiff
    pure $ Extend $ TestLedger (DiffMK $ DS.fromAntiDiff d) (castPoint $ pointAtSlot nextSlotNo)

  advanceSlotNo :: SlotNo -> StateT GenOperationsState Gen (WithOrigin SlotNo)
  advanceSlotNo by = do
    nextSlotNo <- gets (At . Point.withOrigin by (+ by) . gosSlotNo)
    modify' $ \st -> st{gosSlotNo = nextSlotNo}
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
    oneof' $
      catMaybes
        [ genDelEntry activeUtxos
        , genInsertEntry consumedUtxos
        ]

  genDelEntry :: Map Key Int -> Maybe (StateT GenOperationsState Gen (Key, Diff.Delta Int))
  genDelEntry activeUtxos =
    if Map.null activeUtxos
      then Nothing
      else Just $ do
        (k, _) <- lift $ elements (Map.toList activeUtxos)
        modify' $ \st ->
          st
            { gosActiveUtxos = Map.delete k (gosActiveUtxos st)
            }
        pure (k, Diff.Delete)

  genInsertEntry :: Set Key -> Maybe (StateT GenOperationsState Gen (Key, Diff.Delta Int))
  genInsertEntry consumedUtxos = Just $ do
    k <- lift $ genKey `suchThat` (`Set.notMember` consumedUtxos)
    v <- lift arbitrary
    modify' $ \st ->
      st
        { gosPendingInsertions = Map.insert k v (gosPendingInsertions st)
        , gosConsumedUtxos = Set.insert k (gosConsumedUtxos st)
        }
    pure (k, Diff.Insert v)

genKey :: Gen Key
genKey = T.pack <$> replicateM 2 (elements ['A' .. 'Z'])
