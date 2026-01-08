{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that ledger snapshots are performed at /predictable/ points on the
-- immutable chain (modulo rate limiting).
--
-- We open a ChainDB and add to it a (shuffled) list of blocks such that the
-- immutable chain is predetermined. Then, we check that ledger snapshots were
-- created for precisely the points we expect given the configured
-- 'SnapshotFrequencyArgs'.
module Test.Ouroboros.Storage.ChainDB.LedgerSnapshots (tests) where

import Cardano.Ledger.BaseTypes.NonZero
import Control.Monad (guard, replicateM)
import Control.Monad.IOSim (runSim)
import Control.ResourceRegistry
import Control.Tracer
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Time (secondsToDiffTime)
import Data.Traversable (for)
import Data.Word (Word64)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Args (LedgerDbBackendArgs)
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.Snapshots as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as LedgerDB.V1.InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Backend as LedgerDB.V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as LedgerDB.V2.InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LedgerDB.V2.LSM
import Ouroboros.Consensus.Util (dropLast)
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.Enclose (Enclosing' (FallingEdgeWith))
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import System.FS.API (SomeHasFS)
import System.FS.API.Types (mkFsPath)
import System.FS.BlockIO.Sim (simHasBlockIO')
import qualified System.FS.Sim.MockFS as MockFS
import Test.Tasty
import Test.Tasty.QuickCheck hiding (NonZero)
import Test.Util.ChainDB
import Test.Util.Orphans.IOLike ()
import Test.Util.QuickCheck
import Test.Util.TestBlock
import Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests =
  testGroup
    "LedgerSnapshots"
    [ testProperty "InMemV1" $ prop_ledgerSnapshots inMemV1
    , testProperty "InMemV2" $ prop_ledgerSnapshots inMemV2
    , testProperty "LSM" $ \salt -> prop_ledgerSnapshots (lsm salt)
    ]
 where
  inMemV1, inMemV2 :: IOLike m => LedgerDbBackendArgs m TestBlock
  inMemV1 =
    LedgerDB.LedgerDbBackendArgsV1 $
      LedgerDB.V1.V1Args LedgerDB.V1.DisableFlushing $
        LedgerDB.V1.SomeBackendArgs LedgerDB.V1.InMemory.InMemArgs
  inMemV2 =
    LedgerDB.LedgerDbBackendArgsV2 $
      LedgerDB.V2.SomeBackendArgs LedgerDB.V2.InMemory.InMemArgs

  lsm ::
    IOLike m =>
    LedgerDB.V2.LSM.Salt ->
    LedgerDbBackendArgs m TestBlock
  lsm salt =
    LedgerDB.LedgerDbBackendArgsV2 $
      LedgerDB.V2.SomeBackendArgs $
        LedgerDB.V2.LSM.LSMArgs (mkFsPath []) salt mkSimBlockIOFS
   where
    mkSimBlockIOFS registry = allocate registry (\_ -> mk) (\_ -> pure ())
     where
      mk =
        uncurry LedgerDB.V2.LSM.SomeHasFSAndBlockIO
          <$> simHasBlockIO' MockFS.empty

prop_ledgerSnapshots ::
  (forall m. IOLike m => LedgerDbBackendArgs m TestBlock) ->
  TestSetup ->
  Property
prop_ledgerSnapshots lgrDbBackendArgs testSetup =
  case runSim (runTest lgrDbBackendArgs testSetup) of
    Right testOutcome -> checkTestOutcome testSetup testOutcome
    Left err -> counterexample ("Failure: " <> show err) False

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { tsSecParam :: SecurityParam
  , tsMainChain :: AnchoredFragment TestBlock
  , tsForks :: [AnchoredFragment TestBlock]
  -- ^ Forks anchored in the immutable prefix of the main chain. Must be of
  -- length at most @k@.
  , tsPerm :: Permutation
  -- ^ Shuffle the blocks when adding them to the ChainDB, see 'tsBlocksToAdd'.
  , tsTestSnapshotPolicyArgs :: TestSnapshotPolicyArgs
  }
  deriving stock Show

data TestSnapshotPolicyArgs = TestSnapshotPolicyArgs
  { tspaNum :: Word
  , tspaInterval :: NonZero Word64
  , tspaOffset :: SlotNo
  , tspaRateLimit :: DiffTime
  , tspaDelaySnapshotRange :: (DiffTime, DiffTime)
  }
  deriving stock Show

instance Arbitrary TestSnapshotPolicyArgs where
  arbitrary = do
    tspaNum <- choose (1, 10)
    tspaInterval <- choose (1, 10) `suchThatMap` nonZero
    tspaOffset <- SlotNo <$> choose (1, 20)
    tspaRateLimit <-
      frequency
        [ (2, pure 0)
        , (1, secondsToDiffTime <$> choose (1, 10))
        ]
    tspaDelaySnapshotRange <- oneof [arbitraryDelaySnapshotRange, pure (0, 0)]
    pure
      TestSnapshotPolicyArgs
        { tspaNum
        , tspaInterval
        , tspaOffset
        , tspaRateLimit
        , tspaDelaySnapshotRange
        }
    where
      arbitraryDelaySnapshotRange = do
        minimumDelay <- fromInteger <$> choose (floor fiveMinutes, floor tenMinutes)
        additionalDelay <- fromInteger <$> choose (0, floor fiveMinutes)
        pure (minimumDelay, minimumDelay + additionalDelay)

      fiveMinutes :: DiffTime
      fiveMinutes = 5 * 60

      tenMinutes :: DiffTime
      tenMinutes = 10 * 60

-- | Add blocks to the ChainDB in this order.
tsBlocksToAdd :: TestSetup -> [TestBlock]
tsBlocksToAdd testSetup =
  permute tsPerm $
    foldMap AF.toOldestFirst (tsMainChain : tsForks)
 where
  TestSetup{tsMainChain, tsForks, tsPerm} = testSetup

tsSnapshotPolicyArgs :: TestSetup -> SnapshotPolicyArgs
tsSnapshotPolicyArgs TestSetup{tsTestSnapshotPolicyArgs} =
  SnapshotPolicyArgs
    { spaFrequency
    , spaNum = Override $ tspaNum tsTestSnapshotPolicyArgs
    }
 where
  spaFrequency =
    SnapshotFrequency
      SnapshotFrequencyArgs
        { sfaInterval = Override $ tspaInterval tsTestSnapshotPolicyArgs
        , sfaOffset = Override $ tspaOffset tsTestSnapshotPolicyArgs
        , sfaRateLimit = Override $ tspaRateLimit tsTestSnapshotPolicyArgs
        , sfaDelaySnapshotRange = Override $ tspaDelaySnapshotRange tsTestSnapshotPolicyArgs
        }

instance Arbitrary TestSetup where
  arbitrary = do
    k <- choose (1, 6)
    let
      -- Generate an anchored fragment of the given length starting from the
      -- given block, with random slot gaps.
      genChain ::
        Int -> -- Length of the chain
        Word64 -> -- Fork number
        Anchor TestBlock ->
        Gen (AnchoredFragment TestBlock)
      genChain len forkNo anchor =
        go 0 (AF.Empty anchor)
       where
        go n acc
          | n >= len = pure acc
          | otherwise = do
              slotOffset <- SlotNo <$> choose (1, 10)
              let blk = modifyFork (\_ -> forkNo) $
                    (\b -> b{tbSlot = tbSlot b + slotOffset}) $
                      case AF.headPoint acc of
                        GenesisPoint -> firstBlock forkNo
                        BlockPoint slot hash ->
                          (successorBlockWithPayload hash slot ())
              go (n + 1) (acc AF.:> blk)

    immutableLength <- choose (0, 20)
    tsMainChain <- genChain (immutableLength + k) 0 AF.AnchorGenesis
    let immChain = AF.dropNewest k tsMainChain
        immAnchors = AF.anchor immChain : (AF.anchorFromBlock <$> AF.toOldestFirst immChain)
    numForks <- choose (0, 5)
    forkAnchors <- replicateM numForks $ elements immAnchors
    tsForks <- for ([1 ..] `zip` forkAnchors) $ \(forkNo, forkAnchor) -> do
      forkLength <- choose (1, k)
      genChain forkLength forkNo forkAnchor

    tsPerm <- arbitrary
    tsTestSnapshotPolicyArgs <- arbitrary
    pure
      TestSetup
        { tsSecParam = SecurityParam $ unsafeNonZero $ fromIntegral k
        , tsMainChain
        , tsForks
        , tsPerm
        , tsTestSnapshotPolicyArgs
        }

  shrink testSetup@TestSetup{tsSecParam, tsMainChain, tsForks} =
    [ testSetup
        { tsMainChain = tsMainChain'
        , tsForks = filter isStillAnchoredOnImmChain tsForks
        }
    | tsMainChain' <- [AF.dropNewest 1 tsMainChain | not $ AF.null tsMainChain]
    , let k = unNonZero $ maxRollbacks tsSecParam
          immChain' = AF.dropNewest (fromIntegral k) tsMainChain'
          isStillAnchoredOnImmChain f =
            AF.withinFragmentBounds (AF.anchorPoint f) immChain'
    ]

{-------------------------------------------------------------------------------
  Run test
-------------------------------------------------------------------------------}

data TestOutcome = TestOutcome
  { toutImmutableTip :: Anchor TestBlock
  , toutTrace :: [(Time, ChainDB.TraceEvent TestBlock)]
  , toutFinalSnapshots :: [DiskSnapshot]
  }
  deriving stock Show

runTest ::
  forall m.
  IOLike m =>
  LedgerDbBackendArgs m TestBlock ->
  TestSetup ->
  m TestOutcome
runTest lgrDbBackendArgs testSetup = withRegistry \registry -> do
  (withTime -> tracer, getTrace) <- recordingTracerTVar

  isSnapshottingTMVar :: StrictTMVar m () <- newEmptyTMVarIO

  (chainDB, lgrHasFS) <- openChainDB registry (tracer <> isSnapshottingTracer isSnapshottingTMVar)

  for_ (tsBlocksToAdd testSetup) \blk -> do
    ChainDB.addBlock_ chainDB Punishment.noPunishment blk
    threadDelay 1
    atomically $ isEmptyTMVar isSnapshottingTMVar >>= guard

  toutImmutableTip <-
    AF.castAnchor . AF.anchor <$> atomically (ChainDB.getCurrentChain chainDB)
  toutTrace <- getTrace
  toutFinalSnapshots <- LedgerDB.defaultListSnapshots lgrHasFS -- TODO
  pure
    TestOutcome
      { toutImmutableTip
      , toutTrace
      , toutFinalSnapshots
      }
 where
  openChainDB ::
    ResourceRegistry m ->
    Tracer m (ChainDB.TraceEvent TestBlock) ->
    m (ChainDB m TestBlock, SomeHasFS m)
  openChainDB registry cdbTracer = do
    chainDbArgs <- do
      mcdbNodeDBs <- emptyNodeDBs
      let mcdbTopLevelConfig = singleNodeTestConfigWithK (tsSecParam testSetup)
          cdbArgs =
            fromMinimalChainDbArgs
              MinimalChainDbArgs
                { mcdbTopLevelConfig
                , mcdbNodeDBs
                , mcdbChunkInfo = mkTestChunkInfo mcdbTopLevelConfig
                , mcdbInitLedger = testInitExtLedger
                , mcdbRegistry = registry
                }
          updLgrDbArgs a =
            a
              { ChainDB.cdbLgrDbArgs =
                  (ChainDB.cdbLgrDbArgs a)
                    { LedgerDB.lgrBackendArgs = lgrDbBackendArgs
                    , LedgerDB.lgrSnapshotPolicyArgs = tsSnapshotPolicyArgs testSetup
                    }
              }
      pure $ updLgrDbArgs $ ChainDB.updateTracer cdbTracer cdbArgs
    (_, chainDB) <-
      allocate
        registry
        (\_ -> ChainDB.openDB chainDbArgs)
        (ChainDB.closeDB)
    pure (chainDB, LedgerDB.lgrHasFS . ChainDB.cdbLgrDbArgs $ chainDbArgs)

  withTime = contramapM \ev -> (,ev) <$> getMonotonicTime

  isSnapshottingTracer :: StrictTMVar m () -> Tracer m (ChainDB.TraceEvent TestBlock)
  isSnapshottingTracer tmvar = Tracer \case
    ChainDB.TraceLedgerDBEvent (LedgerDB.LedgerDBSnapshotEvent (SnapshotRequestDelayed _ _ _)) ->
      atomically $ putTMVar tmvar ()
    ChainDB.TraceLedgerDBEvent (LedgerDB.LedgerDBSnapshotEvent SnapshotRequestCompleted) ->
      atomically $ takeTMVar tmvar
    _ -> pure ()

{-------------------------------------------------------------------------------
  Assess a test outcome
-------------------------------------------------------------------------------}

checkTestOutcome :: TestSetup -> TestOutcome -> Property
checkTestOutcome testSetup testOutcome =
  withLabelling . withTrace $
    conjoin
      [ counterexample "Unexpected immutable tip" $
          toutImmutableTip === AF.headAnchor immChain
      , counterexample "Snapshots not strictly increasing" $
          strictlyIncreasing (snd <$> actualSnapshots)
      , counterexample ("Unexpected number of on-disk snapshots " <> show toutFinalSnapshots) $
          length toutFinalSnapshots
            === min (length actualSnapshots) (fromIntegral tspaNum)
      , counterexample ("Rate limit not respected...") $
          conjoin
            [ counterexample ("...between " <> condense pt1 <> " and " <> condense pt2) $
                tspaRateLimit `le` diffTime t2 t1
            | ((t1, pt1), (t2, pt2)) <- actualSnapshots `zip` drop 1 actualSnapshots
            ]
      , counterexample "Unexpected snapshots performed" $
          counterexample ("Policy: " <> show policyArgs) $ do
            let actual = Set.fromList (snd <$> actualSnapshots)
                expect = Set.fromList expectedSnapshots
            counterexample ("Not expected: " <> condense (actual Set.\\ expect)) $
              if tspaRateLimit <= 0
                then
                  counterexample ("Expected, but missing: " <> condense (expect Set.\\ actual)) $
                    actual === expect
                else
                  property $ actual `Set.isSubsetOf` expect
      ]
 where
  TestSetup
    { tsSecParam = unNonZero . maxRollbacks -> k
    , tsMainChain
    , tsTestSnapshotPolicyArgs =
      policyArgs@TestSnapshotPolicyArgs
        { tspaNum
        , tspaInterval
        , tspaOffset
        , tspaRateLimit
        }
    } = testSetup

  immChain = AF.dropNewest (fromIntegral k) tsMainChain

  ppTrace (time, ev) = show time <> ": " <> show ev

  isTookSnapshot :: ChainDB.TraceEvent blk -> Maybe SlotNo
  isTookSnapshot = \case
    ChainDB.TraceLedgerDBEvent
      ( LedgerDB.LedgerDBSnapshotEvent
          (LedgerDB.TookSnapshot _ pt FallingEdgeWith{})
        ) -> pure $ realPointSlot pt
    _ -> Nothing

  TestOutcome
    { toutImmutableTip
    , toutTrace
    , toutFinalSnapshots
    } = testOutcome

  actualSnapshots :: [(Time, SlotNo)]
  actualSnapshots = mapMaybe (traverse isTookSnapshot) toutTrace

  -- Group on @(s1 - offset) / interval@ and take the last entry from each group
  -- (apart from the last one).
  expectedSnapshots :: [SlotNo]
  expectedSnapshots =
    fmap NE.last
      -- For the last group, it is not yet necessarily clear what the last
      -- immutable block will be. (If there is a block in the last slot of a
      -- group, ie the predecessor of @offset + n * interval@ for some @n@,
      -- there can't be, but it doesn't seem important to handle this case in a
      -- special way.)
      . dropLast 1
      . NE.groupWith snapshotGroup
      . fmap blockSlot
      . AF.toOldestFirst
      $ immChain
   where
    snapshotGroup s1
      | s1 < tspaOffset = Nothing
      | otherwise = Just $ unSlotNo (s1 - tspaOffset) `div` unNonZero tspaInterval

  withTrace =
    counterexample ("Trace:\n" <> unlines (ppTrace <$> toutTrace))
      . counterexample ("Actual snapshots: " <> condense actualSnapshots)
      . counterexample ("Actual immutable tip: " <> condense (AF.anchorToPoint toutImmutableTip))
      . counterexample ("Immutable chain: " <> condense immChain)

  withLabelling =
    tabulate "# actual snapshots" [show (length actualSnapshots)]
      . tabulate "length of immutable chain" [show (AF.anchorToBlockNo toutImmutableTip)]
