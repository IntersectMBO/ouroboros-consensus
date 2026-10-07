{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import qualified Control.Exception as Exception
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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word64)
import Ouroboros.Consensus.Block.RealPoint
  ( RealPoint (..)
  , blockRealPoint
  )
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import Ouroboros.Consensus.Storage.ChainDB.Impl (TraceEvent)
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import Ouroboros.Consensus.Storage.Common
  ( StreamFrom (..)
  , StreamTo (..)
  )
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks as ImmutableDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasImmutableCertDB as PerasImmutableCertDB
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (ChainUpdate (..), Point, blockPoint, genesisPoint)
import qualified Ouroboros.Network.Mock.Chain as Mock
import System.FS.API.Lazy
import System.FS.Sim.Error
  ( Errors (..)
  , emptyErrors
  , simErrorHasFS
  , withErrors
  )
import qualified System.FS.Sim.Stream as Stream
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
import qualified Test.QuickCheck as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.ChainDB
  ( MinimalChainDbArgs (..)
  , emptyNodeDBs
  , fromMinimalChainDbArgs
  , nodeDBsPerasImmutableCert
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
        "PerasImmutableCertDB handoff"
        [ testCase "canonical certificate is archived" $
            runSystemIO perasCanonicalCertArchived
        , testCase "all certificates for one immutable block are archived" $
            runSystemIO perasAllCertsForBlockArchived
        , testCase "certificate for a losing fork is not archived" $
            runSystemIO perasLosingForkCertNotArchived
        , testCase "persist-and-GC retains the historical certificate" $
            runSystemIO perasPersistThenGCRetainsCert
        , testCase "certificate older than the immutable tip is ignored" $
            runSystemIO perasLateCertIgnored
        , testCase "a handoff-boundary certificate is rejected or archived" $
            runSystemIO perasHandoffBoundaryCertNotLost
        , testCase "certificate for another block at the immutable-tip slot is ignored" $
            runSystemIO perasConflictingCertAtImmutableTipSlotIgnored
        , testCase "certificate received before its target block is archived" $
            runSystemIO perasCertBeforeTargetArchived
        , testProperty "sparse certificates across newly immutable blocks are archived" $
            QC.withMaxSuccess 50 propSparseMultiBlockHandoff
        , testGroup
            "handoff failure characterization"
            [ testCase "block can commit before its certificate write fails" $
                runSystemIOWithPerasImmutableErrors perasBlockCommittedBeforeCertFailure
            , testCase "one block can retain only a prefix of its certificates" $
                runSystemIOWithPerasImmutableErrors perasPartialMultiCertHandoff
            , testCase "a multi-block handoff can stop after a partially committed block" $
                runSystemIOWithPerasImmutableErrors perasFailureMidMultiBlockHandoff
            ]
        , testCase "conflicting certificates for one round preserve the first" $
            runSystemIO perasConflictingSameRoundFirstWins
        , testCase "an unknown target that later loses is not archived" $
            runSystemIOWithK
              (SecurityParam $ knownNonZeroBounded @10)
              perasUnknownTargetLoses
        ]
    , testGroup
        "Peras chain selection"
        [ testCase "late certificate causes a density-reducing rollback" $
            runSystemIOWithK
              (SecurityParam $ knownNonZeroBounded @20)
              perasBoostInducedDensityReduction
        , testCase "repeated certificate releases archive only the final canonical fork" $
            runSystemIOWithK
              (SecurityParam $ knownNonZeroBounded @10)
              perasRepeatedForkOscillation
        , testCase "additional positive boosts cannot reduce a chain's weight" $
            runSystemIOWithK
              (SecurityParam $ knownNonZeroBounded @20)
              perasWeightAdditionDoesNotWrap
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

-- | A certificate known while its target is volatile is copied once that
-- target becomes part of the canonical immutable chain.
perasCanonicalCertArchived :: SystemM TestBlock IO ()
perasCanonicalCertArchived = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  let cert = mkHistoricalCert 1 b1 1
  void $ addTestPerasCert cert
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)

  before <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [] before "Certificate was archived before the block was copied"

  persistBlks

  after <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [cert] after "Certificate was not archived with its immutable block"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Certificates from distinct rounds can boost the same block; all of them
-- must survive the handoff.
perasAllCertsForBlockArchived :: SystemM TestBlock IO ()
perasAllCertsForBlockArchived = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  let cert1 = mkHistoricalCert 1 b1 1
      cert2 = mkHistoricalCert 2 b1 1
  void $ addTestPerasCert cert1
  void $ addTestPerasCert cert2
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)

  persistBlks

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [cert1, cert2] archived "Not all certificates for the block were archived"
 where
  body forkNo = TestBody forkNo True Nothing

-- | A certificate for a block on a losing fork must not be copied merely
-- because some other block became immutable.
perasLosingForkCertNotArchived :: SystemM TestBlock IO ()
perasLosingForkCertNotArchived = do
  p <- addBlock $ firstBlock 0 (body 0)
  losing <- addBlock $ mkNextBlock p 1 (body 1)
  void $ addTestPerasCert (mkHistoricalCert 1 losing 1)

  h1 <- addBlock $ mkNextBlock p 2 (body 2)
  h2 <- addBlock $ mkNextBlock h1 3 (body 2)
  h3 <- addBlock $ mkNextBlock h2 4 (body 2)
  _h4 <- addBlock $ mkNextBlock h3 5 (body 2)

  persistBlks

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [] archived "A certificate for the losing fork was archived"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Garbage collection may discard the volatile copy, but not the historical
-- copy made during persistence.
perasPersistThenGCRetainsCert :: SystemM TestBlock IO ()
perasPersistThenGCRetainsCert = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  let cert = mkHistoricalCert 1 b1 1
  void $ addTestPerasCert cert
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)

  persistBlksThenGC

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [cert] archived "Historical certificate was lost after garbage collection"
 where
  body forkNo = TestBody forkNo True Nothing

-- | A certificate first received after its target is strictly older than the
-- immutable tip could not have influenced this node's chain selection.
perasLateCertIgnored :: SystemM TestBlock IO ()
perasLateCertIgnored = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  b3 <- addBlock $ mkNextBlock b2 2 (body 0)
  _b4 <- addBlock $ mkNextBlock b3 3 (body 0)
  persistBlks

  outcome <- addTestPerasCert (mkHistoricalCert 1 b1 1)
  assertEqual
    API.PerasCertIgnoredTooOld
    outcome
    "Certificate older than the immutable tip was accepted"

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [] archived "Late certificate was added to historical storage"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Model the post-snapshot side of the handoff race deterministically. Once a
-- block has crossed into the ImmutableDB, a certificate for it must either be
-- rejected or be copied directly to historical storage. Accepting it only into
-- the volatile certificate DB loses it permanently because that block will not
-- be handed off a second time.
perasHandoffBoundaryCertNotLost :: SystemM TestBlock IO ()
perasHandoffBoundaryCertNotLost = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  b3 <- addBlock $ mkNextBlock b2 2 (body 0)
  persistBlks

  let cert = mkHistoricalCert 1 b1 1
  outcome <- addTestPerasCert cert

  _b4 <- addBlock $ mkNextBlock b3 3 (body 0)
  persistBlks
  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10

  case outcome of
    API.PerasCertIgnoredTooOld ->
      assertEqual [] archived "A rejected certificate was nevertheless archived"
    API.PerasCertProcessed _ ->
      assertEqual
        [cert]
        archived
        "A certificate accepted after its block handoff was never archived"
    other ->
      failWith $
        "Unexpected result for a certificate at the handoff boundary: "
          <> show other
 where
  body forkNo = TestBody forkNo True Nothing

-- | A point at the immutable tip's slot but with another hash is already on an
-- unselectable fork, even though its slot is not strictly older.
perasConflictingCertAtImmutableTipSlotIgnored :: SystemM TestBlock IO ()
perasConflictingCertAtImmutableTipSlotIgnored = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  b3 <- addBlock $ mkNextBlock b2 2 (body 0)
  _b4 <- addBlock $ mkNextBlock b3 3 (body 0)
  persistBlks

  -- With k=2, b2 is now the immutable tip. This block has the same slot and
  -- predecessor as b2, but a different body and therefore a different hash.
  let competingAtImmutableTipSlot = mkNextBlock b1 1 (body 1)
  outcome <-
    addTestPerasCert $
      mkHistoricalCert 1 competingAtImmutableTipSlot 1
  assertEqual
    API.PerasCertIgnoredTooOld
    outcome
    "Certificate for a conflicting block at the immutable-tip slot was accepted"

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [] archived "Late certificate was added to historical storage"
 where
  body forkNo = TestBody forkNo True Nothing

-- | A certificate can arrive before its target block. If that block is later
-- received, selected, and made immutable, the certificate must still be found
-- by the handoff and archived.
perasCertBeforeTargetArchived :: SystemM TestBlock IO ()
perasCertBeforeTargetArchived = do
  parent <- addBlock $ firstBlock 0 (body 0)
  let target = mkNextBlock parent 1 (body 0)
      cert = mkHistoricalCert 1 target 1

  addTestPerasCert cert >>= \case
    API.PerasCertProcessed _ -> pure ()
    outcome ->
      failWith $
        "Certificate for an unknown target was not retained: " <> show outcome

  before <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [] before "Certificate was archived before its target block"

  target' <- addBlock target
  b2 <- addBlock $ mkNextBlock target' 2 (body 0)
  _b3 <- addBlock $ mkNextBlock b2 3 (body 0)
  persistBlks

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) 10
  assertEqual [cert] archived "Certificate received before its target was lost"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Generate a genuinely sparse distribution: at least one newly immutable
-- block has no certificates and at least one has one or more.
genSparseCertLayout :: QC.Gen [Int]
genSparseCertLayout = do
  blockCount <- QC.chooseInt (3, 8)
  QC.vectorOf blockCount (QC.chooseInt (0, 3))
    `QC.suchThat` \counts -> any (== 0) counts && any (> 0) counts

propSparseMultiBlockHandoff :: QC.Property
propSparseMultiBlockHandoff =
  QC.forAll genSparseCertLayout $ \certCounts ->
    QC.counterexample ("certificate counts per block: " <> show certCounts) $
      QC.ioProperty $ do
        runSystemIO (perasSparseMultiBlockHandoff certCounts)
        pure True

-- | One persistence pass may copy several blocks, with a sparse and nonuniform
-- collection of certificates spread over them. It must archive exactly those
-- certificates, in round order.
perasSparseMultiBlockHandoff :: [Int] -> SystemM TestBlock IO ()
perasSparseMultiBlockHandoff [] =
  failWith "perasSparseMultiBlockHandoff: empty generated layout"
perasSparseMultiBlockHandoff (firstCount : remainingCounts) = do
  first <- addBlock $ firstBlock 0 (body 0)
  (firstCerts, nextRound) <- addCerts first 1 firstCount
  (tip, expected, nextSlot) <-
    go first firstCerts 1 nextRound remainingCounts

  tail1 <- addBlock $ mkNextBlock tip (fromIntegral nextSlot) (body 0)
  _tail2 <- addBlock $ mkNextBlock tail1 (fromIntegral $ nextSlot + 1) (body 0)

  before <- getHistoricalCertsAfter (PerasRoundNo 0) maxBound
  assertEqual [] before "Certificates were archived before persistence"

  persistBlks

  archived <- getHistoricalCertsAfter (PerasRoundNo 0) maxBound
  assertEqual
    expected
    archived
    "Sparse multi-block handoff archived the wrong certificates"
 where
  body forkNo = TestBody forkNo True Nothing

  addCerts
    :: TestBlock
    -> Word64
    -> Int
    -> SystemM TestBlock IO ([ValidatedPerasCert TestBlock], Word64)
  addCerts target firstRound count = do
    let certs =
          [ mkHistoricalCert roundNo target 1
          | roundNo <- take count [firstRound ..]
          ]
    mapM_ (void . addTestPerasCert) certs
    pure (certs, firstRound + fromIntegral count)

  go
    :: TestBlock
    -> [ValidatedPerasCert TestBlock]
    -> Word64
    -> Word64
    -> [Int]
    -> SystemM
        TestBlock
        IO
        (TestBlock, [ValidatedPerasCert TestBlock], Word64)
  go tip expected nextSlot _nextRound [] =
    pure (tip, expected, nextSlot)
  go tip expected nextSlot nextRound (count : counts) = do
    block <- addBlock $ mkNextBlock tip (fromIntegral nextSlot) (body 0)
    (certs, nextRound') <- addCerts block nextRound count
    go
      block
      (expected <> certs)
      (nextSlot + 1)
      nextRound'
      counts

-- | Characterize the current non-atomic handoff: the block append and anchor
-- update happen before the historical certificate write. A failed certificate
-- rename therefore leaves the block immutable but its certificate absent, and
-- a later persistence pass has no block left to use as a retry trigger.
perasBlockCommittedBeforeCertFailure ::
  Strict.StrictTVar IO Errors ->
  SystemM TestBlock IO ()
perasBlockCommittedBeforeCertFailure errorsVar = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  let cert = mkHistoricalCert 1 b1 1
  void $ addTestPerasCert cert
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)

  expectPersistFsFailure errorsVar (renameFailureAt 1)
  assertBlockImmutable b1
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [] archived "Certificate survived its failed write"

  void $ runCmd SM.Close
  void $ runCmd SM.Reopen
  assertBlockImmutable b1
  persistBlks
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [] archived "A later persistence pass unexpectedly repaired the certificate"
 where
  body forkNo = TestBody forkNo True Nothing

-- | If several certificates boost one block, failure while writing a later
-- certificate leaves the already-renamed prefix committed. Restarting does not
-- complete the set because the block has already crossed the handoff boundary.
perasPartialMultiCertHandoff ::
  Strict.StrictTVar IO Errors ->
  SystemM TestBlock IO ()
perasPartialMultiCertHandoff errorsVar = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  let cert1 = mkHistoricalCert 1 b1 1
      cert2 = mkHistoricalCert 2 b1 1
  void $ addTestPerasCert cert1
  void $ addTestPerasCert cert2
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)

  expectPersistFsFailure errorsVar (renameFailureAt 2)
  assertBlockImmutable b1
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [cert1] archived "Unexpected partial certificate prefix"

  void $ runCmd SM.Close
  void $ runCmd SM.Reopen
  persistBlks
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [cert1] archived "Restart unexpectedly completed the certificate set"
 where
  body forkNo = TestBody forkNo True Nothing

-- | In a persistence pass spanning several blocks, a certificate failure on
-- the middle block leaves all preceding block/certificate pairs committed and
-- the middle block committed without its certificate. After restart, later
-- blocks can still be copied, but the missing certificates are not recovered.
perasFailureMidMultiBlockHandoff ::
  Strict.StrictTVar IO Errors ->
  SystemM TestBlock IO ()
perasFailureMidMultiBlockHandoff errorsVar = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  b3 <- addBlock $ mkNextBlock b2 2 (body 0)
  let cert1 = mkHistoricalCert 1 b1 1
      cert2 = mkHistoricalCert 2 b2 1
      cert3 = mkHistoricalCert 3 b3 1

  -- Register all three certificates while their targets are still at or
  -- above the logical immutable tip. Extending through b5 first makes b1 and
  -- b2 too old, so those certificates are ignored and there is no second
  -- historical rename on which to inject the failure.
  mapM_
    ( \cert ->
        addTestPerasCert cert
          >>= \outcome ->
            assertEqual
              (API.PerasCertProcessed PerasCertDB.AddedPerasCertToDB)
              outcome
              "Certificate needed by the handoff scenario was not retained"
    )
    [cert1, cert2, cert3]

  b4 <- addBlock $ mkNextBlock b3 3 (body 0)
  _b5 <- addBlock $ mkNextBlock b4 4 (body 0)

  expectPersistFsFailure errorsVar (renameFailureAt 2)
  assertBlockImmutable b2
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [cert1] archived "Unexpected archive after middle-block failure"

  void $ runCmd SM.Close
  void $ runCmd SM.Reopen
  persistBlks
  assertBlockImmutable b3
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [cert1] archived "Restart unexpectedly recovered skipped certificates"
 where
  body forkNo = TestBody forkNo True Nothing

-- | The volatile DB and the historical DB both use the round as a unique key.
-- A conflicting second certificate must not change fork choice or replace the
-- first certificate during persistence or reopen.
perasConflictingSameRoundFirstWins :: SystemM TestBlock IO ()
perasConflictingSameRoundFirstWins = do
  b1 <- addBlock $ firstBlock 0 (body 0)
  let first = mkHistoricalCert 7 b1 0
      conflicting = mkHistoricalCert 7 (firstBlock 0 $ body 1) 99

  addTestPerasCert first
    >>= \outcome ->
      assertEqual
        (API.PerasCertProcessed PerasCertDB.AddedPerasCertToDB)
        outcome
        "First certificate was not added"
  addTestPerasCert conflicting
    >>= \outcome ->
      assertEqual
        (API.PerasCertProcessed PerasCertDB.PerasCertAlreadyInDB)
        outcome
        "Conflicting certificate replaced the first certificate"

  b2 <- addBlock $ mkNextBlock b1 1 (body 0)
  _b3 <- addBlock $ mkNextBlock b2 2 (body 0)
  persistBlks
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [first] archived "Historical storage did not preserve the first certificate"

  void $ runCmd SM.Close
  void $ runCmd SM.Reopen
  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [first] archived "Reopen changed the winning certificate"
 where
  body forkNo = TestBody forkNo True Nothing

-- | A certificate may precede its target, but that alone must not cause the
-- certificate to be archived when the target later appears only on a losing
-- fork.
perasUnknownTargetLoses :: SystemM TestBlock IO ()
perasUnknownTargetLoses = do
  common <- addBlock $ firstBlock 0 (body 0)
  let losing1 = mkNextBlock common 1 (body 1)
      cert = mkHistoricalCert 1 losing1 1
  addTestPerasCert cert
    >>= \outcome ->
      assertEqual
        (API.PerasCertProcessed PerasCertDB.AddedPerasCertToDB)
        outcome
        "Certificate for the unknown target was not retained"

  h1 <- addBlock $ mkNextBlock common 2 (body 0)
  h2 <- addBlock $ mkNextBlock h1 3 (body 0)
  h3 <- addBlock $ mkNextBlock h2 4 (body 0)
  h4 <- addBlock $ mkNextBlock h3 5 (body 0)
  losing1' <- addBlock losing1
  _losing2 <- addBlock $ mkNextBlock losing1' 6 (body 1)

  getSelectedTip
    >>= \tip ->
      assertEqual (blockPoint h4) tip "The certified losing fork was selected"

  finalTip <- extendChainBy 12 20 (body 0) h4
  getSelectedTip
    >>= \tip ->
      assertEqual (blockPoint finalTip) tip "The canonical branch stopped being selected"
  persistBlks

  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual [] archived "Certificate for the losing fork was archived"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Alternate certificate releases between two forks. Only certificates on
-- the fork selected at the point it becomes immutable may cross into the
-- historical DB.
perasRepeatedForkOscillation :: SystemM TestBlock IO ()
perasRepeatedForkOscillation = do
  common <- addBlock $ firstBlock 0 (body 0)

  a1 <- addBlock $ mkNextBlock common 10 (body 1)
  a2 <- addBlock $ mkNextBlock a1 20 (body 1)
  a3 <- addBlock $ mkNextBlock a2 30 (body 1)
  a4 <- addBlock $ mkNextBlock a3 40 (body 1)

  b1 <- addBlock $ mkNextBlock common 11 (body 2)
  b2 <- addBlock $ mkNextBlock b1 21 (body 2)
  b3 <- addBlock $ mkNextBlock b2 31 (body 2)

  getSelectedTip >>= \tip ->
    assertEqual (blockPoint a4) tip "Longer fork A was not initially selected"

  let certB1 = mkHistoricalCert 1 b1 2
      certA1 = mkHistoricalCert 2 a1 2
      certB2 = mkHistoricalCert 3 b2 2
  void $ addTestPerasCert certB1
  getSelectedTip >>= \tip ->
    assertEqual (blockPoint b3) tip "First B certificate did not switch to fork B"
  void $ addTestPerasCert certA1
  getSelectedTip >>= \tip ->
    assertEqual (blockPoint a4) tip "A certificate did not switch back to fork A"
  void $ addTestPerasCert certB2
  getSelectedTip >>= \tip ->
    assertEqual (blockPoint b3) tip "Second B certificate did not restore fork B"

  finalTip <- extendChainBy 12 50 (body 2) b3
  getSelectedTip >>= \tip ->
    assertEqual (blockPoint finalTip) tip "Fork B was not final"
  persistBlks

  getHistoricalCertsAfter (PerasRoundNo 0) 10
    >>= \archived ->
      assertEqual
        [certB1, certB2]
        archived
        "Historical DB retained certificates from a noncanonical oscillation"
 where
  body forkNo = TestBody forkNo True Nothing

-- | Adding a positive boost must be monotone. This specifically guards the
-- Word64 boundary where modular addition would wrap a dominant chain back to a
-- tiny weight.
perasWeightAdditionDoesNotWrap :: SystemM TestBlock IO ()
perasWeightAdditionDoesNotWrap = do
  common <- addBlock $ firstBlock 0 (body 0)
  a1 <- addBlock $ mkNextBlock common 1 (body 1)
  a2 <- addBlock $ mkNextBlock a1 2 (body 1)
  b1 <- addBlock $ mkNextBlock common 3 (body 2)

  getSelectedTip >>= \tip ->
    assertEqual (blockPoint a2) tip "Longer unboosted fork was not selected"

  void $ addTestPerasCert $ mkHistoricalCert 1 b1 (maxBound - 2)
  getSelectedTip >>= \tip ->
    assertEqual (blockPoint b1) tip "Large boost did not select fork B"

  void $ addTestPerasCert $ mkHistoricalCert 2 b1 3
  getSelectedTip >>= \tip ->
    assertEqual
      (blockPoint b1)
      tip
      "An additional positive boost reduced the selected chain's weight"
 where
  body forkNo = TestBody forkNo True Nothing

extendChainBy ::
  Int ->
  Word64 ->
  TestBody ->
  TestBlock ->
  SystemM TestBlock IO TestBlock
extendChainBy 0 _ _ tip = pure tip
extendChainBy count slot body tip = do
  next <- addBlock $ mkNextBlock tip (fromIntegral slot) body
  extendChainBy (count - 1) (slot + 1) body next

assertBlockImmutable :: TestBlock -> SystemM TestBlock IO ()
assertBlockImmutable block =
  waitForImmutableBlock (blockRealPoint block)
    >>= \result ->
      assertEqual
        (Right $ blockRealPoint block)
        result
        "Block was not present in the ImmutableDB"

expectPersistFsFailure ::
  Strict.StrictTVar IO Errors ->
  Errors ->
  SystemM TestBlock IO ()
expectPersistFsFailure errorsVar injectedErrors = do
  env <- ask
  outcome <-
    SystemM $ lift $ lift $
      Exception.try @FsError $
        withErrors errorsVar injectedErrors $
          runExceptT $
            runReaderT (runSystemM persistBlks) env
  case outcome of
    Left _ -> pure ()
    Right (Left _) -> pure ()
    Right (Right ()) -> failWith "Expected historical certificate persistence to fail"

renameFailureAt :: Int -> Errors
renameFailureAt operation
  | operation <= 0 = error "renameFailureAt: operation must be positive"
  | otherwise =
      emptyErrors
        { renameFileE =
            Stream.unsafeMkFinite $
              replicate (operation - 1) Nothing <> [Just FsDeviceFull]
        }

addTestPerasCert ::
  ValidatedPerasCert TestBlock ->
  SystemM TestBlock IO API.AddPerasCertChainSelOutcome
addTestPerasCert cert =
  runCmd
    ( SM.AddPerasCert
        (WithArrivalTime (RelativeTime 0) cert)
        (SM.Persistent [])
    )
    >>= \case
      SM.PerasCertRes outcome -> pure outcome
      _ -> failWith "addTestPerasCert: unexpected response constructor"

getHistoricalCertsAfter ::
  PerasRoundNo ->
  Word64 ->
  SystemM TestBlock IO [ValidatedPerasCert TestBlock]
getHistoricalCertsAfter roundNo maxCerts = do
  env <- ask
  SystemM $ lift $ lift $ do
    db <-
      PerasImmutableCertDB.openDB $
        cdbPerasImmutableCertDbArgs (args env)
    PerasImmutableCertDB.getCertsAfter db roundNo maxCerts

mkHistoricalCert ::
  Word64 ->
  TestBlock ->
  Word64 ->
  ValidatedPerasCert TestBlock
mkHistoricalCert roundNo target boost =
  ValidatedPerasCert
    { vpcCert =
        MockPerasCert
          { mockCertRound = PerasRoundNo roundNo
          , mockCertBlock = blockPoint target
          , mockCertVoters =
              NESet.fromList (PerasSeatIndex 0 :| [])
          }
    , vpcCertBoost = PerasWeight boost
    }

-- | Exhibit the state needed by the boost-induced density-reduction attack:
-- one block tree is ordered differently depending only on whether the node
-- knows the certificate.
--
--                 h1 -- h2 -- h3 -- h4    four blocks, no boost
--                /
-- common --------+
--                \a1* -- a2              two blocks, boost three
--
-- The block-only view selects h4. Releasing the certificate for a1 makes the
-- shorter fork heavier, so ChainDB rolls back to the less dense branch.
perasBoostInducedDensityReduction :: SystemM TestBlock IO ()
perasBoostInducedDensityReduction = do
  common <- addBlock $ firstBlock 0 (body 0)

  h1 <- addBlock $ mkNextBlock common 10 (body 1)
  h2 <- addBlock $ mkNextBlock h1 30 (body 1)
  h3 <- addBlock $ mkNextBlock h2 50 (body 1)
  h4 <- addBlock $ mkNextBlock h3 80 (body 1)

  a1 <- addBlock $ mkNextBlock common 10 (body 2)
  a2 <- addBlock $ mkNextBlock a1 20 (body 2)

  getSelectedTip
    >>= \tip ->
      assertEqual
        (blockPoint h4)
        tip
        "Without the certificate, the denser fork should be selected"

  let cert = mkHistoricalCert 1 a1 3
  void $ addTestPerasCert cert

  getSelectedTip
    >>= \tip ->
      assertEqual
        (blockPoint a2)
        tip
        "The certificate should make the shorter fork heavier"

  -- Reopening reconstructs the same block tree from the VolatileDB, but the
  -- volatile certificate DB is empty. This models an observer that has the
  -- blocks but not the off-chain certificate.
  void $ runCmd SM.Close
  void $ runCmd SM.Reopen

  getSelectedTip
    >>= \tip ->
      assertEqual
        (blockPoint h4)
        tip
        "Without certificate history, reopening should select the denser fork"

  void $ addTestPerasCert cert
  getSelectedTip
    >>= \tip ->
      assertEqual
        (blockPoint a2)
        tip
        "Replaying the certificate should restore the weighted selection"
 where
  body forkNo = TestBody forkNo True Nothing

getSelectedTip :: SystemM TestBlock IO (Point TestBlock)
getSelectedTip =
  runCmd SM.GetTipPoint >>= \case
    SM.Point point -> pure point
    _ -> failWith "getSelectedTip: unexpected response constructor"

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
  newModel = Model.empty loe (testInitExtLedger (topLevelConfigLedger topLevelConfig))
  topLevelConfig = mkTestCfg k chunkInfo

-- | Helper function to run the test against the actual chain database and
-- translate to something that HUnit likes.
runSystemIO :: SystemM TestBlock IO a -> IO ()
runSystemIO =
  runSystemIOWithK $
    SecurityParam (knownNonZeroBounded @2)

runSystemIOWithK ::
  SecurityParam ->
  SystemM TestBlock IO a ->
  IO ()
runSystemIOWithK k expr =
  runSystem withChainDbEnv expr >>= toAssertion
 where
  chunkInfo = ImmutableDB.simpleChunkInfo 100
  topLevelConfig = mkTestCfg k chunkInfo

  withChainDbEnv ::
    forall b.
    (ChainDBEnv IO TestBlock -> IO [TraceEvent TestBlock] -> IO b) ->
    IO b
  withChainDbEnv =
    withTestChainDbEnv topLevelConfig chunkInfo $
      convertMapKind (testInitExtLedger (topLevelConfigLedger topLevelConfig))

runSystemIOWithPerasImmutableErrors ::
  (Strict.StrictTVar IO Errors -> SystemM TestBlock IO a) ->
  IO ()
runSystemIOWithPerasImmutableErrors expr = do
  errorsVar <- Strict.newTVarIO emptyErrors
  let withChainDbEnv ::
        forall b.
        (ChainDBEnv IO TestBlock -> IO [TraceEvent TestBlock] -> IO b) ->
        IO b
      withChainDbEnv =
        withTestChainDbEnvWithPerasImmutableErrors
          topLevelConfig
          chunkInfo
          (convertMapKind $ testInitExtLedger $ topLevelConfigLedger topLevelConfig)
          errorsVar
  runSystem withChainDbEnv (expr errorsVar) >>= toAssertion
 where
  chunkInfo = ImmutableDB.simpleChunkInfo 100
  k = SecurityParam (knownNonZeroBounded @2)
  topLevelConfig = mkTestCfg k chunkInfo

-- | Variant of 'withTestChainDbEnv' whose historical certificate filesystem
-- can be subjected to deterministic fs-sim failures without affecting the
-- ImmutableDB, VolatileDB, or LedgerDB filesystems.
withTestChainDbEnvWithPerasImmutableErrors ::
  (IOLike m, TestConstraints blk) =>
  TopLevelConfig blk ->
  ImmutableDB.ChunkInfo ->
  ExtLedgerState blk ValuesMK ->
  Strict.StrictTVar m Errors ->
  (ChainDBEnv m blk -> m [TraceEvent blk] -> m a) ->
  m a
withTestChainDbEnvWithPerasImmutableErrors
  topLevelConfig
  chunkInfo
  extLedgerState
  errorsVar
  cont =
    bracket openChainDbEnv closeChainDbEnv (uncurry cont)
   where
    openChainDbEnv = do
      threadRegistry <- unsafeNewRegistry
      iteratorRegistry <- unsafeNewRegistry
      varNextId <- uncheckedNewTVarM 0
      varLoEFragment <- newTVarIO $ AF.Empty AF.AnchorGenesis
      nodeDbs <- emptyNodeDBs
      (tracer, getTrace) <- recordingTracerTVar
      let baseArgs = chainDbArgs threadRegistry nodeDbs tracer
          perasImmutableArgs =
            (cdbPerasImmutableCertDbArgs baseArgs)
              { PerasImmutableCertDB.picdbaHasFS =
                  SomeHasFS $
                    simErrorHasFS
                      (nodeDBsPerasImmutableCert nodeDbs)
                      errorsVar
              }
          args =
            baseArgs
              { cdbPerasImmutableCertDbArgs = perasImmutableArgs
              }
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
  (TestConstraints blk, LedgerTablesAreTrivial LedgerState blk) =>
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
