{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.ChainDB.Unit (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad (replicateM, unless, void)
import Control.Monad.Except
  ( Except
  , ExceptT (..)
  , MonadError
  , runExcept
  , runExceptT
  , throwError
  )
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.ResourceRegistry (closeRegistry, unsafeNewRegistry)
import Data.Maybe (isJust)
import Ouroboros.Consensus.Block.RealPoint
  ( RealPoint (..)
  , blockRealPoint
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  )
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import Ouroboros.Consensus.Storage.ChainDB.Impl (TraceEvent)
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import Ouroboros.Consensus.Storage.Common
  ( StreamFrom (..)
  , StreamTo (..)
  )
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks as ImmutableDB
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (ChainUpdate (..), Point, blockPoint, genesisPoint)
import qualified Ouroboros.Network.Mock.Chain as Mock
import Test.Ouroboros.Storage.ChainDB.Model (Model)
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import Test.Ouroboros.Storage.ChainDB.StateMachine
  ( AllComponents
  , ChainDBEnv (..)
  , ChainDBState (..)
  , TestConstraints
  , close
  , mkTestCfg
  , open
  )
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as SM
import Test.Ouroboros.Storage.TestBlock
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Util.ChainDB
  ( MinimalChainDbArgs (..)
  , emptyNodeDBs
  , fromMinimalChainDbArgs
  , nodeDBsVol
  )
import Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ testGroup
        "First follower instruction isJust on empty ChainDB"
        [ testCase "model" $ runModelIO API.LoEDisabled followerInstructionOnEmptyChain
        , testCase "system" $ runSystemIO followerInstructionOnEmptyChain
        ]
    , testGroup
        "Follower switches to new chain"
        [ testCase "model" $ runModelIO API.LoEDisabled followerSwitchesToNewChain
        , testCase "system" $ runSystemIO followerSwitchesToNewChain
        ]
    , testGroup
        (ouroborosNetworkIssue 4183)
        [ testCase "model" $ runModelIO API.LoEDisabled ouroboros_network_4183
        , testCase "system" $ runSystemIO ouroboros_network_4183
        ]
    , testGroup
        (ouroborosNetworkIssue 3999)
        [ testCase "model" $ runModelIO API.LoEDisabled ouroboros_network_3999
        , testCase "system" $ runSystemIO ouroboros_network_3999
        ]
    , testGroup
        "ChainDB.waitForImmutableBlock"
        [ testGroup
            "Existing block, returns same"
            [testCase "system" $ runSystemIO waitForImmutableBlock_existingBlock]
        , testGroup
            "Existing block, returns same, call 'wait' concurrently with adding blocks"
            [testCase "system" $ runSystemIO waitForImmutableBlock_existingBlockConcurrent]
        , testGroup
            "Wrong hash, returns the actual block at slot"
            [testCase "system" $ runSystemIO waitForImmutableBlock_wrongHash]
        , testGroup
            "Empty slot, returns block at next filled slot"
            [testCase "system" $ runSystemIO waitForImmutableBlock_emptySlot]
        ]
    , testGroup
        "Interaction of ImmutableDB, wiping the VolatileDB and ledger state snapshots"
        [ testGroup
            "Chain not long enough to take a snapshot, so blocks are not persisted into ImmutableDB and are lost."
            [testCase "system" $ runSystemIO updateLedgerSnapshots_WipeVolatileDB_withoutSnapshot]
        ]
    ]

followerInstructionOnEmptyChain :: (SupportsUnitTest m, MonadError TestFailure m) => m ()
followerInstructionOnEmptyChain = do
  f <- newFollower
  followerInstruction f >>= \case
    Right instr -> isJust instr `orFailWith` "Expecting a follower instruction"
    Left _ -> failWith $ "ChainDbError"

-- | Test that a follower starts following the newly selected fork.
-- The chain constructed in this example looks like:
--
--     G --- b1 --- b2
--            \
--             \--- b3 -- b4
followerSwitchesToNewChain ::
  (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m) => m ()
followerSwitchesToNewChain =
  let fork i = TestBody i True Nothing
   in do
        b1 <- addBlock $ firstBlock 0 $ fork 0 -- b1 on top of G
        b2 <- addBlock $ mkNextBlock b1 1 $ fork 0 -- b2 on top of b1
        f <- newFollower
        followerForward f [blockPoint b2] >>= \case
          Right (Just pt) -> assertEqual (blockPoint b2) pt "Expected to be at b2"
          _ -> failWith "Expecting a success"
        b3 <- addBlock $ mkNextBlock b1 2 $ fork 1 -- b3 on top of b1
        b4 <- addBlock $ mkNextBlock b3 3 $ fork 1 -- b4 on top of b3
        followerInstruction f >>= \case
          Right (Just (RollBack actual)) ->
            -- Expect to rollback to the intersection point between [b1, b2] and
            -- [b1, b3, b4]
            assertEqual (blockPoint b1) actual "Rollback to wrong point"
          _ -> failWith "Expecting a rollback"
        followerInstruction f >>= \case
          Right (Just (AddBlock actual)) ->
            assertEqual b3 (extractBlock actual) "Instructed to add wrong block"
          _ -> failWith "Expecting instruction to add a block"
        followerInstruction f >>= \case
          Right (Just (AddBlock actual)) ->
            assertEqual b4 (extractBlock actual) "Instructed to add wrong block"
          _ -> failWith "Expecting instruction to add a block"

ouroborosNetworkIssue :: Int -> String
ouroborosNetworkIssue n
  | n <= 0 = error "Issue number should be positive"
  | otherwise = "https://github.com/IntersectMBO/ouroboros-network/issues/" <> show n

ouroboros_network_4183 ::
  ( Block m ~ TestBlock
  , SupportsUnitTest m
  , MonadError TestFailure m
  ) =>
  m ()
ouroboros_network_4183 =
  let fork i = TestBody i True Nothing
   in do
        b1 <- addBlock $ firstEBB (const True) $ fork 0
        b2 <- addBlock $ mkNextBlock b1 0 $ fork 0
        b3 <- addBlock $ mkNextBlock b2 1 $ fork 1
        b4 <- addBlock $ mkNextBlock b2 1 $ fork 0
        f <- newFollower
        void $ followerForward f [blockPoint b1]
        void $ addBlock $ mkNextBlock b4 4 $ fork 0
        persistBlks
        void $ addBlock $ mkNextBlock b3 3 $ fork 1
        followerInstruction f >>= \case
          Right (Just (RollBack actual)) ->
            assertEqual (blockPoint b1) actual "Rollback to wrong point"
          _ -> failWith "Expecting a rollback"

-- | Test that iterators over dead forks that may have been garbage-collected
-- either stream the blocks in the dead fork normally, report that the blocks
-- have been garbage-collected, or return that the iterator is exhausted,
-- depending on when garbage collection happened. The result is
-- non-deterministic, since garbage collection happens in the background, and
-- hence, may not yet have happened when the next item in the iterator is
-- requested.
ouroboros_network_3999 ::
  ( Mock.HasHeader (Block m)
  , Block m ~ TestBlock
  , SupportsUnitTest m
  , MonadError TestFailure m
  ) =>
  m ()
ouroboros_network_3999 = do
  b1 <- addBlock $ firstBlock 0 $ fork 1
  b2 <- addBlock $ mkNextBlock b1 1 $ fork 1
  b3 <- addBlock $ mkNextBlock b2 2 $ fork 1
  i <- streamAssertSuccess (inclusiveFrom b1) (inclusiveTo b3)
  b4 <- addBlock $ mkNextBlock b1 3 $ fork 2
  b5 <- addBlock $ mkNextBlock b4 4 $ fork 2
  b6 <- addBlock $ mkNextBlock b5 5 $ fork 2
  void $ addBlock $ mkNextBlock b6 6 $ fork 2
  persistBlksThenGC

  -- The block b1 is part of the current chain, so should always be returned.
  result <- iteratorNextBlock i
  assertEqual (API.IteratorResult b1) result "Streaming first block"

  -- The remainder of the elements in the iterator are part of the dead fork,
  -- and may have been garbage-collected.
  let options =
        [ -- If the dead fork has been garbage-collected, the SUT, *given that
          -- the minimal chaindb args set the max blocks per file to 4* will
          -- close the iterator, as the block will really be GCed.
          [API.IteratorBlockGCed $ blockRealPoint b2, API.IteratorExhausted]
        , -- The model will always think that the block has been garbage
          -- collected, and will keep returning the same thing. This way we
          -- abstract away from how the implementation internally works
          -- (deleting whole files).
          [API.IteratorBlockGCed $ blockRealPoint b2, API.IteratorBlockGCed $ blockRealPoint b2]
        , -- The dead fork has not been garbage-collected yet.
          [API.IteratorResult b2, API.IteratorResult b3]
        ]

  actual <- replicateM 2 (iteratorNextBlock i)
  assertOneOf options actual "Streaming over dead fork"
 where
  fork i = TestBody i True Nothing

  iteratorNextBlock it = fmap extractBlock <$> iteratorNext it

  inclusiveFrom = StreamFromInclusive . blockRealPoint
  inclusiveTo = StreamToInclusive . blockRealPoint

-- | Tests that given an existing block, we get that same block back
waitForImmutableBlock_existingBlock ::
  forall m. (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m) => m ()
waitForImmutableBlock_existingBlock = do
  -- add three blocks, as @k@ is set to 2 in these test
  b1 <- addBlock $ firstBlock 0 $ fork0
  b2 <- addBlock $ mkNextBlock b1 1 $ fork0
  _b3 <- addBlock $ mkNextBlock b2 2 $ fork0
  -- copy the blocks older than @k@ into ImmutableDB,
  -- should copy only b1
  persistBlks
  -- request the immutable block
  waitForImmutableBlock (blockRealPoint b1) >>= \case
    Left e -> failWith (show e)
    Right result -> assertEqual result (blockRealPoint b1) ""
 where
  fork0 = TestBody 0 True Nothing

-- | Tests that given an existing block, we get that same block back,
--   but we wait first and then add the blocks to test the waiting behaviour
waitForImmutableBlock_existingBlockConcurrent ::
  forall m. (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m, MonadFork m) => m ()
waitForImmutableBlock_existingBlockConcurrent = do
  _ <- forkIO addBlocksConcurrently
  waitForImmutableBlock (blockRealPoint targetBlock) >>= \case
    Left e -> failWith (show e)
    Right result -> assertEqual result (blockRealPoint targetBlock) ""
 where
  addBlocksConcurrently :: m ()
  addBlocksConcurrently = do
    -- add three blocks, as @k@ is set to 2 in these test
    b1 <- addBlock $ firstBlock 0 $ fork0
    b2 <- addBlock $ mkNextBlock b1 1 $ fork0
    _b3 <- addBlock $ mkNextBlock b2 2 $ fork0
    -- copy the blocks older than @k@ into ImmutableDB,
    -- should copy only b1
    persistBlks

  targetBlock = firstBlock 0 fork0
  fork0 = TestBody 0 True Nothing

-- | Tests that given a block at a filled slot but with a wrong hash,
--   we get the actual block at that slot
waitForImmutableBlock_wrongHash ::
  forall m. (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m) => m ()
waitForImmutableBlock_wrongHash = do
  -- add four blocks, as @k@ is set to 2 in these test
  b1 <- addBlock $ firstBlock 0 $ fork0
  b2 <- addBlock $ mkNextBlock b1 1 $ fork0
  b3 <- addBlock $ mkNextBlock b2 2 $ fork0
  _b4 <- addBlock $ mkNextBlock b3 3 $ fork0
  -- copy the blocks older than @k@ into ImmutableDB,
  -- should copy only b1 and b2
  persistBlks
  -- request a block at a filled slot, but give the wrong hash
  let targetPoint = RealPoint 0 (TestHeaderHash 0)
  -- expect to get the block at slot 0 and the correct hash
  let expectedPoint = blockRealPoint b1
  waitForImmutableBlock targetPoint >>= \case
    Left e -> failWith (show e)
    Right result -> assertEqual result expectedPoint ""
 where
  fork0 = TestBody 0 True Nothing

-- | Tests that given an empty slot, we get a block
--   at the next filled slot
waitForImmutableBlock_emptySlot ::
  forall m. (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m) => m ()
waitForImmutableBlock_emptySlot = do
  -- add four blocks, as @k@ is set to 2 in these test
  b1 <- addBlock $ firstBlock 1 $ fork0
  b2 <- addBlock $ mkNextBlock b1 2 $ fork0
  b3 <- addBlock $ mkNextBlock b2 3 $ fork0
  _b4 <- addBlock $ mkNextBlock b3 4 $ fork0
  -- copy the blocks older than @k@ into ImmutableDB,
  -- should copy only b1
  persistBlks
  -- request a block at an empty slot, the hash doesn't matter
  let targetPoint = RealPoint 0 (TestHeaderHash 0)
  -- expect to get the block at slot 1 and the correct hash
  let expectedPoint = blockRealPoint b1
  waitForImmutableBlock targetPoint >>= \case
    Left e -> failWith (show e)
    Right result -> assertEqual result expectedPoint ""
 where
  fork0 = TestBody 0 True Nothing

-- | Taking a ledger state snapshot should only copy blocks to the
-- ImmutableDB when the snapshot policy selects slots for snapshotting. When the
-- immutable chain is too short, no blocks should be flushed, and WipeVolatileDB
-- should recover to the tip of the (empty) ImmutableDB.
updateLedgerSnapshots_WipeVolatileDB_withoutSnapshot ::
  forall m.
  ( Block m ~ TestBlock
  , SupportsUnitTest m
  , MonadError TestFailure m
  ) =>
  m ()
updateLedgerSnapshots_WipeVolatileDB_withoutSnapshot = do
  b1 <- addBlock $ firstBlock 1 $ fork0
  b2 <- addBlock $ mkNextBlock b1 3 $ fork0
  _b3 <- addBlock $ mkNextBlock b2 5 $ fork0

  -- With k=2, 3 blocks are not enough to trigger a snapshot,
  updateLedgerSnapshots

  tip <- wipeVolatileDB
  tip
    == genesisPoint
      `orFailWith` ("Expected ChainDB tip after wiping VolatileDB to be at Genesis, but got: " <> show tip)
 where
  fork0 = TestBody 1 True Nothing

{-------------------------------------------------------------------------------
  Helpers and testing infrastructure
-------------------------------------------------------------------------------}

streamAssertSuccess ::
  (MonadError TestFailure m, SupportsUnitTest m, Mock.HasHeader (Block m)) =>
  StreamFrom (Block m) -> StreamTo (Block m) -> m (IteratorId m)
streamAssertSuccess from to =
  stream from to >>= \case
    Left err -> failWith $ "Should be able to create iterator: " <> show err
    Right (Left err) -> failWith $ "Range should be valid: " <> show err
    Right (Right iteratorId) -> pure iteratorId

extractBlock :: AllComponents blk -> blk
extractBlock (blk, _, _, _, _, _, _, _, _, _, _) = blk

-- | Helper function to run the test against the model and translate to something
-- that HUnit likes.
runModelIO :: API.LoE () -> ModelM TestBlock a -> IO ()
runModelIO loe expr = toAssertion (runModel newModel topLevelConfig expr)
 where
  chunkInfo = ImmutableDB.simpleChunkInfo 100
  k = SecurityParam (knownNonZeroBounded @2)
  newModel = Model.empty loe testInitExtLedger
  topLevelConfig = mkTestCfg k chunkInfo

-- | Helper function to run the test against the actual chain database and
-- translate to something that HUnit likes.
runSystemIO :: SystemM TestBlock IO a -> IO ()
runSystemIO expr = runSystem withChainDbEnv expr >>= toAssertion
 where
  chunkInfo = ImmutableDB.simpleChunkInfo 100
  k = SecurityParam (knownNonZeroBounded @2)
  topLevelConfig = mkTestCfg k chunkInfo
  withChainDbEnv = withTestChainDbEnv topLevelConfig chunkInfo $ convertMapKind testInitExtLedger

newtype TestFailure = TestFailure String deriving Show

toAssertion :: Either TestFailure a -> Assertion
toAssertion (Left (TestFailure t)) = assertFailure t
toAssertion (Right _) = pure ()

orFailWith :: MonadError TestFailure m => Bool -> String -> m ()
orFailWith b msg = unless b $ failWith msg
infixl 1 `orFailWith`

failWith :: MonadError TestFailure m => String -> m a
failWith msg = throwError (TestFailure msg)

assertEqual ::
  (MonadError TestFailure m, Eq a, Show a) =>
  a -> a -> String -> m ()
assertEqual expected actual description = expected == actual `orFailWith` msg
 where
  msg =
    description
      <> "\n\t Expected: "
      <> show expected
      <> "\n\t Actual: "
      <> show actual

assertOneOf ::
  (MonadError TestFailure m, Eq a, Show a) =>
  [a] -> a -> String -> m ()
assertOneOf options actual description = actual `elem` options `orFailWith` msg
 where
  msg =
    description
      <> "\n\t Options: "
      <> show options
      <> "\n\t Actual: "
      <> show actual

-- | SupportsUnitTests for the test expression need to instantiate this class.
class SupportsUnitTest m where
  type FollowerId m
  type IteratorId m
  type Block m

  addBlock ::
    Block m -> m (Block m)

  newFollower ::
    m (FollowerId m)

  followerInstruction ::
    FollowerId m ->
    m
      ( Either
          (API.ChainDbError (Block m))
          (Maybe (ChainUpdate (Block m) (AllComponents (Block m))))
      )

  followerForward ::
    FollowerId m ->
    [Point (Block m)] ->
    m
      ( Either
          (API.ChainDbError (Block m))
          (Maybe (Point (Block m)))
      )

  persistBlks :: m ()

  persistBlksThenGC :: m ()

  stream ::
    StreamFrom (Block m) ->
    StreamTo (Block m) ->
    m
      ( Either
          (API.ChainDbError (Block m))
          (Either (API.UnknownRange (Block m)) (IteratorId m))
      )

  iteratorNext ::
    IteratorId m ->
    m (API.IteratorResult (Block m) (AllComponents (Block m)))

  updateLedgerSnapshots :: m ()

  wipeVolatileDB :: m (Point (Block m))

  waitForImmutableBlock ::
    RealPoint (Block m) -> m (Either API.SeekBlockError (RealPoint (Block m)))

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Tests against the model run in this monad.
newtype ModelM blk a = ModelM
  { runModelM :: StateT (Model blk) (ReaderT (TopLevelConfig blk) (Except TestFailure)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (TopLevelConfig blk)
    , MonadState (Model blk)
    , MonadError TestFailure
    )

runModel ::
  Model blk ->
  TopLevelConfig blk ->
  ModelM blk b ->
  Either TestFailure b
runModel model topLevelConfig expr =
  runExcept (runReaderT (evalStateT (runModelM expr) model) topLevelConfig)

-- | Run a 'Cmd' against the model via 'SM.runPure'.
runModelCmd ::
  TestConstraints blk =>
  SM.Cmd blk Model.IteratorId Model.FollowerId ->
  ModelM blk (SM.Success blk Model.IteratorId Model.FollowerId)
runModelCmd cmd = do
  model <- get
  cfg <- ask
  let (SM.Resp resp, model') = SM.runPure cfg cmd model
  put model'
  case resp of
    Left err -> failWith $ "runModelCmd: ChainDbError: " <> show err
    Right success -> pure success

instance
  (TestConstraints blk, LedgerTablesAreTrivial (LedgerState blk)) =>
  SupportsUnitTest (ModelM blk)
  where
  type FollowerId (ModelM blk) = Model.FollowerId
  type IteratorId (ModelM blk) = Model.IteratorId
  type Block (ModelM blk) = blk

  newFollower = do
    result <- runModelCmd (SM.NewFollower API.SelectedChain)
    case result of
      SM.Flr fid -> pure fid
      _ -> failWith $ "newFollower: unexpected result " <> show result

  followerInstruction followerId = do
    result <- runModelCmd (SM.FollowerInstruction followerId)
    case result of
      SM.MbChainUpdate mcu -> pure (Right mcu)
      _ -> failWith $ "followerInstruction: unexpected result" <> show result

  addBlock blk = do
    void $ runModelCmd (SM.AddBlock blk (SM.Persistent []))
    pure blk

  followerForward followerId points = do
    result <- runModelCmd (SM.FollowerForward followerId points)
    case result of
      SM.MbPoint mp -> pure (Right mp)
      _ -> failWith $ "followerForward: unexpected result" <> show result

  persistBlks =
    void $ runModelCmd SM.PersistBlks

  persistBlksThenGC =
    void $ runModelCmd SM.PersistBlksThenGC

  updateLedgerSnapshots =
    void $ runModelCmd SM.UpdateLedgerSnapshots

  wipeVolatileDB = do
    result <- runModelCmd SM.WipeVolatileDB
    case result of
      SM.Point p -> pure p
      _ -> error $ "wipeVolatileDB: unexpected result" <> show result

  stream from to = do
    result <- runModelCmd (SM.Stream from to)
    case result of
      SM.Iter iid -> pure (Right (Right iid))
      SM.UnknownRange ur -> pure (Right (Left ur))
      _ -> failWith $ "stream: unexpected result" <> show result

  iteratorNext iteratorId = do
    result <- runModelCmd (SM.IteratorNext iteratorId)
    case result of
      SM.IterResult ir -> pure ir
      _ -> failWith $ "iteratorNext: unexpected result" <> show result

  -- the implementation is intentionally left trivial
  -- cannot be implemented in terms of `runCmdModel`
  waitForImmutableBlock _ = pure . Left $ API.TargetNewerThanTip

{-------------------------------------------------------------------------------
  System
-------------------------------------------------------------------------------}

-- | Tests against the actual chain database run in this monad.
newtype SystemM blk m a = SystemM
  { runSystemM :: ReaderT (ChainDBEnv m blk) (ExceptT TestFailure m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ChainDBEnv m blk)
    , MonadError TestFailure
    , MonadThread
    , MonadFork
    )

-- this instance is needed for the concurrent tests of 'waitForImmutableBlock'
instance MonadThread m => MonadThread (ExceptT e m) where
  type ThreadId (ExceptT e m) = ThreadId m
  myThreadId = lift myThreadId
  labelThread t l = lift (labelThread t l)
  threadLabel t = lift (threadLabel t)

-- this instance is needed for the concurrent tests of 'waitForImmutableBlock',
-- but we only need 'forkIO'
instance MonadFork m => MonadFork (ExceptT e m) where
  forkIO (ExceptT action) = lift $ forkIO (void action)
  forkIOWithUnmask _ = error "Intentionally left unimplemented"
  forkOn = error "Intentionally left unimplemented"
  forkFinally = error "Intentionally left unimplemented"
  throwTo = error "Intentionally left unimplemented"
  yield = error "Intentionally left unimplemented"
  getNumCapabilities = error "Intentionally left unimplemented"

runSystem ::
  (forall a. (ChainDBEnv m blk -> m [TraceEvent blk] -> m a) -> m a) ->
  SystemM blk m b ->
  m (Either TestFailure b)
runSystem withChainDbEnv expr =
  withChainDbEnv $ \env _getTrace ->
    runExceptT $ runReaderT (runSystemM expr) env

-- | Provide a standard ChainDbEnv for testing.
withTestChainDbEnv ::
  (IOLike m, TestConstraints blk) =>
  TopLevelConfig blk ->
  ImmutableDB.ChunkInfo ->
  ExtLedgerState blk ValuesMK ->
  (ChainDBEnv m blk -> m [TraceEvent blk] -> m a) ->
  m a
withTestChainDbEnv topLevelConfig chunkInfo extLedgerState cont =
  bracket openChainDbEnv closeChainDbEnv (uncurry cont)
 where
  openChainDbEnv = do
    threadRegistry <- unsafeNewRegistry
    iteratorRegistry <- unsafeNewRegistry
    varNextId <- uncheckedNewTVarM 0
    varLoEFragment <- newTVarIO $ AF.Empty AF.AnchorGenesis
    nodeDbs <- emptyNodeDBs
    (tracer, getTrace) <- recordingTracerTVar
    let args = chainDbArgs threadRegistry nodeDbs tracer
    varDB <- open args >>= newTVarIO
    let env =
          ChainDBEnv
            { varDB
            , registry = iteratorRegistry
            , varNextId
            , varVolatileDbFs = nodeDBsVol nodeDbs
            , args
            , varLoEFragment
            }
    pure (env, getTrace)

  closeChainDbEnv (env, _) = do
    readTVarIO (varDB env) >>= close
    closeRegistry (registry env)
    closeRegistry (cdbsRegistry . cdbsArgs $ args env)

  chainDbArgs registry nodeDbs tracer =
    let args =
          fromMinimalChainDbArgs
            MinimalChainDbArgs
              { mcdbTopLevelConfig = topLevelConfig
              , mcdbChunkInfo = chunkInfo
              , mcdbInitLedger = extLedgerState
              , mcdbRegistry = registry
              , mcdbNodeDBs = nodeDbs
              }
     in updateTracer tracer args

-- | Run a 'Cmd' against the real ChainDB via 'SM.run'.
runCmd ::
  (IOLike m, TestConstraints blk) =>
  SM.Cmd blk (SM.TestIterator m blk) (SM.TestFollower m blk) ->
  SystemM blk m (SM.Success blk (SM.TestIterator m blk) (SM.TestFollower m blk))
runCmd cmd = do
  env <- ask
  let cfg = cdbsTopLevelConfig . cdbsArgs $ args env
  SystemM $ lift $ lift $ SM.run cfg env cmd

instance (IOLike m, TestConstraints blk) => SupportsUnitTest (SystemM blk m) where
  type IteratorId (SystemM blk m) = SM.TestIterator m blk
  type FollowerId (SystemM blk m) = SM.TestFollower m blk
  type Block (SystemM blk m) = blk

  addBlock blk = do
    void $ runCmd (SM.AddBlock blk (SM.Persistent []))
    pure blk

  persistBlks =
    void $ runCmd SM.PersistBlks

  persistBlksThenGC =
    void $ runCmd SM.PersistBlksThenGC

  updateLedgerSnapshots = do
    void $ runCmd SM.UpdateLedgerSnapshots

  wipeVolatileDB = do
    result <- runCmd SM.WipeVolatileDB
    case result of
      SM.Point p -> pure p
      _ -> error $ "wipeVolatileDB: unexpected result"

  newFollower = do
    result <- runCmd (SM.NewFollower API.SelectedChain)
    case result of
      SM.Flr fid -> pure fid
      _ -> error "newFollower: unexpected result"

  followerInstruction followerId = do
    result <- runCmd (SM.FollowerInstruction followerId)
    case result of
      SM.MbChainUpdate mcu -> pure (Right mcu)
      _ -> error "followerInstruction: unexpected result"

  followerForward followerId points = do
    result <- runCmd (SM.FollowerForward followerId points)
    case result of
      SM.MbPoint mp -> pure (Right mp)
      _ -> error "followerForward: unexpected result"

  stream from to = do
    result <- runCmd (SM.Stream from to)
    case result of
      SM.Iter iid -> pure (Right (Right iid))
      SM.UnknownRange ur -> pure (Right (Left ur))
      _ -> error "stream: unexpected result"

  iteratorNext iteratorId = do
    result <- runCmd (SM.IteratorNext iteratorId)
    case result of
      SM.IterResult ir -> pure ir
      _ -> error "iteratorNext: unexpected result"

  -- cannot be implemented in terms of `runCmd`
  waitForImmutableBlock targetPoint = do
    env <- ask
    SystemM $ lift $ lift $ do
      api <- chainDB <$> readTVarIO (varDB env)
      API.waitForImmutableBlock api targetPoint
