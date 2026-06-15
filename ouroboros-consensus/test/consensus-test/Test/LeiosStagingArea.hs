{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Unit tests for 'LeiosStagingArea'.
--
-- Each test drives the staging gate ('wrappedChainDbView's
-- 'addBlockAsync') with a 'LeiosBlock', a hand-rolled block type
-- whose Leios state (carries a cert? announces what EB?) is
-- specified per block. Parents are placed on a fake ChainSync
-- candidate fragment so the gate's parent lookup finds the
-- announcement.
module Test.LeiosStagingArea (tests) where

import Cardano.Slotting.Slot (SlotNo (..), unSlotNo)
import Cardano.Slotting.Time (RelativeTime (RelativeTime))
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadAsync (wait, withAsync)
import Control.Monad.Class.MonadTimer.SI (DiffTime)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoDb
  ( LeiosDbHandle
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , newLeiosDBInMemory
  , withLeiosDb
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , PeerId
  , TxHash (..)
  , leiosEbBytesSize
  , leiosEbTxs
  )
import LeiosStagingArea
  ( LeiosStagingArea (..)
  , newLeiosStagingArea
  )
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( BlockNo (..)
  , ChainHash (..)
  , GetHeader (..)
  , GetPrevHash (..)
  , HasHeader (..)
  , Header
  , HeaderFields (..)
  , HeaderHash
  , Point (GenesisPoint)
  , StandardHash
  )
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
  ( ChainSyncJumpingState (..)
  , DisengagedInitState (..)
  )
import Ouroboros.Consensus.Peras.Weight (emptyPerasWeightSnapshot)
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.LedgerDB (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , atomically
  , modifyTVar
  , readTVar
  , uncheckedNewTVarM
  )
import Ouroboros.Consensus.Util.STM (Fingerprint (..), WithFingerprint (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (MaxSlotNo (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( ChainSelStarvation (..)
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "LeiosStagingArea"
    [ testCase
        "drain releases staged CertRB when AcquiredEbTxs fires"
        test_drainRelease
    , testCase
        "GC evicts staged CertRB when no peer knows it"
        test_gcEvict
    , testCase
        "two CertRBs over the same EB hash both release independently"
        test_multiSlotRelease
    , testCase
        "gate ignores currentChain when looking up parent"
        test_gateIgnoresCurrentChain
    ]

------------------------------------------------------------
-- Tests
------------------------------------------------------------

test_drainRelease :: IO ()
test_drainRelease = withRegistry $ \registry -> do
  fixture <- mkFixture largeGcInterval registry
  let ebPoint = MkLeiosPoint (SlotNo 1) (mkTestEbHash 1)
      eb = mkTestEb 1
      size = leiosEbBytesSize eb
      parent = mkParent 0 (Just (ebPoint, size))
      child = mkChild 1 parent
  addCandidateContaining fixture [parent]
  withAsync (addBlockViaGate fixture child) $ \stageJob -> do
    completeClosure fixture ebPoint eb
    promise <- wait stageJob
    result <- atomically (blockProcessed promise)
    case result of
      SuccesfullyAddedBlock _ -> pure ()
      FailedToAddBlock e -> assertFailure $ "expected success, got: " <> e
    admitted <- readAdmitted fixture
    length admitted @?= 1

test_gcEvict :: IO ()
test_gcEvict = withRegistry $ \registry -> do
  fixture <- mkFixture shortGcInterval registry
  let ebPoint = MkLeiosPoint (SlotNo 1) (mkTestEbHash 1)
      parent = mkParent 0 (Just (ebPoint, 1024))
      child = mkChild 1 parent
  -- Parent is on the candidate (so the gate finds its
  -- announcement); the child itself is not, so the GC sees no peer
  -- holding it.
  addCandidateContaining fixture [parent]
  promise <- addBlockViaGate fixture child
  result <- atomically (blockProcessed promise)
  case result of
    FailedToAddBlock _ -> pure ()
    SuccesfullyAddedBlock _ ->
      assertFailure "expected FailedToAddBlock from GC eviction, got success"
  admitted <- readAdmitted fixture
  admitted @?= []

-- | The gate must consult ChainSync candidate fragments only, not
-- the current selected chain. If the parent is on currentChain but
-- on no candidate, the gate must blind-admit (parent announcement
-- not visible), not stage.
test_gateIgnoresCurrentChain :: IO ()
test_gateIgnoresCurrentChain = withRegistry $ \registry -> do
  let ebPoint = MkLeiosPoint (SlotNo 1) (mkTestEbHash 1)
      parent = mkParent 0 (Just (ebPoint, 1024))
      child = mkChild 1 parent
  fixture <- mkFixture shortGcInterval registry
  -- Put the parent on the mocked current chain — but NOT on any
  -- ChainSync candidate. A gate that consults currentChain would
  -- find the announcement and stage; the post-fix gate only looks at
  -- candidates, doesn't find the announcement, and blind-admits.
  setCurrentChain fixture (AF.fromOldestFirst AF.AnchorGenesis [getHeader parent])
  promise <- addBlockViaGate fixture child
  result <- atomically (blockProcessed promise)
  case result of
    SuccesfullyAddedBlock _ -> pure ()
    FailedToAddBlock e ->
      assertFailure $
        "expected blind admit (gate ignores currentChain), got: " <> e
  admitted <- readAdmitted fixture
  length admitted @?= 1

test_multiSlotRelease :: IO ()
test_multiSlotRelease = withRegistry $ \registry -> do
  fixture <- mkFixture largeGcInterval registry
  let ebHash = mkTestEbHash 1
      point1 = MkLeiosPoint (SlotNo 1) ebHash
      point2 = MkLeiosPoint (SlotNo 2) ebHash
      eb = mkTestEb 1
      size = leiosEbBytesSize eb
      parent1 = mkParent 0 (Just (point1, size))
      parent2 = mkParent 1 (Just (point2, size))
      child1 = mkChild 2 parent1
      child2 = mkChild 3 parent2
  addCandidateContaining fixture [parent1, parent2]
  withAsync (addBlockViaGate fixture child1) $ \job1 ->
    withAsync (addBlockViaGate fixture child2) $ \job2 -> do
      withLeiosDb fixture.leiosDb $ \con -> do
        leiosDbInsertEbPoint con point1 size
        leiosDbInsertEbPoint con point2 size
        leiosDbInsertEbBody con point1 eb
        leiosDbInsertEbBody con point2 eb
        let txList = V.toList (leiosEbTxs eb)
        _ <- leiosDbInsertTxs con [(h, maxSizedTx) | (h, _) <- txList]
        pure ()
      promise1 <- wait job1
      promise2 <- wait job2
      result1 <- atomically (blockProcessed promise1)
      result2 <- atomically (blockProcessed promise2)
      assertSuccess "child1" result1
      assertSuccess "child2" result2
      admitted <- readAdmitted fixture
      length admitted @?= 2
 where
  assertSuccess tag = \case
    SuccesfullyAddedBlock _ -> pure ()
    FailedToAddBlock e ->
      assertFailure $ tag <> ": expected success, got " <> e

------------------------------------------------------------
-- Leios-aware test block
------------------------------------------------------------

-- | Block hash, a wrapping of 'Word64' so the type is local to this
-- test module.
newtype LeiosBlockHash = LeiosBlockHash Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

-- | A minimal block carrying just enough state to drive the staging
-- gate: a hash, a parent hash, a slot, a flag for "has Leios cert",
-- and an optional EB announcement (consulted by the gate when this
-- block is a parent of a child being staged).
data LeiosBlock = LeiosBlock
  { ltbHash :: !LeiosBlockHash
  , ltbPrev :: !(ChainHash LeiosBlock)
  , ltbSlot :: !SlotNo
  , ltbBlockNo :: !BlockNo
  , ltbHasCert :: !Bool
  , ltbAnnounces :: !(Maybe (LeiosPoint, BytesSize))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

type instance HeaderHash LeiosBlock = LeiosBlockHash

instance StandardHash LeiosBlock

newtype instance Header LeiosBlock = LeiosBlockHeader {unLeiosBlockHeader :: LeiosBlock}
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance HasHeader LeiosBlock where
  getHeaderFields b =
    HeaderFields
      { headerFieldHash = ltbHash b
      , headerFieldSlot = ltbSlot b
      , headerFieldBlockNo = ltbBlockNo b
      }

instance HasHeader (Header LeiosBlock) where
  getHeaderFields (LeiosBlockHeader b) =
    HeaderFields
      { headerFieldHash = ltbHash b
      , headerFieldSlot = ltbSlot b
      , headerFieldBlockNo = ltbBlockNo b
      }

instance GetHeader LeiosBlock where
  getHeader = LeiosBlockHeader
  blockMatchesHeader (LeiosBlockHeader b') b = b == b'
  headerIsEBB = const Nothing

instance GetPrevHash LeiosBlock where
  headerPrevHash = ltbPrev . unLeiosBlockHeader

instance ResolveLeiosBlock LeiosBlock where
  blockHasLeiosCert = ltbHasCert
  headerLeiosAnnouncement = ltbAnnounces . unLeiosBlockHeader

-- | A parent block that announces (or doesn't) an EB.
mkParent :: Word64 -> Maybe (LeiosPoint, BytesSize) -> LeiosBlock
mkParent forkNo announcement =
  LeiosBlock
    { ltbHash = LeiosBlockHash forkNo
    , ltbPrev = GenesisHash
    , ltbSlot = SlotNo 1
    , ltbBlockNo = BlockNo 1
    , ltbHasCert = False
    , ltbAnnounces = announcement
    }

-- | A CertRB child of the given parent.
mkChild :: Word64 -> LeiosBlock -> LeiosBlock
mkChild forkNo parent =
  LeiosBlock
    { ltbHash = LeiosBlockHash forkNo
    , ltbPrev = BlockHash (ltbHash parent)
    , ltbSlot = SlotNo (1 + unSlotNo (ltbSlot parent))
    , ltbBlockNo = BlockNo (1 + unBlockNo (ltbBlockNo parent))
    , ltbHasCert = True
    , ltbAnnounces = Nothing
    }

------------------------------------------------------------
-- Fixture
------------------------------------------------------------

type TestPeer = Int

data Fixture = Fixture
  { area :: LeiosStagingArea IO (PeerId TestPeer) LeiosBlock
  , leiosDb :: LeiosDbHandle IO
  , candidateVar :: StrictTVar IO (Map TestPeer (ChainSyncClientHandle IO LeiosBlock))
  , admittedVar :: StrictTVar IO [LeiosBlock]
  , currentChainVar ::
      StrictTVar IO (AF.AnchoredFragment (Header LeiosBlock))
  }

setCurrentChain :: Fixture -> AF.AnchoredFragment (Header LeiosBlock) -> IO ()
setCurrentChain fixture frag =
  atomically $ modifyTVar (currentChainVar fixture) (const frag)

mkFixture :: DiffTime -> ResourceRegistry IO -> IO Fixture
mkFixture gcInterval registry = do
  leiosDb <- newLeiosDBInMemory
  admittedVar <- uncheckedNewTVarM []
  candidateVar <- uncheckedNewTVarM Map.empty
  currentChainVar <- uncheckedNewTVarM (AF.Empty AF.AnchorGenesis)
  let handles = mockHandleCollection candidateVar
  ready <- MVar.newEmptyMVar
  let defView =
        BlockFetchClientInterface.ChainDbView
          { BlockFetchClientInterface.getCurrentChain =
              readTVar currentChainVar
          , BlockFetchClientInterface.getCurrentChainWithTime =
              pure (AF.Empty AF.AnchorGenesis)
          , BlockFetchClientInterface.getIsFetched =
              pure (const False)
          , BlockFetchClientInterface.getMaxSlotNo = pure NoMaxSlotNo
          , BlockFetchClientInterface.addBlockAsync = \_punish blk -> do
              atomically $ modifyTVar admittedVar (blk :)
              pure
                AddBlockPromise
                  { blockWrittenToDisk = pure True
                  , blockProcessed =
                      pure $ SuccesfullyAddedBlock GenesisPoint
                  }
          , BlockFetchClientInterface.getChainSelStarvation =
              pure ChainSelStarvationOngoing
          , BlockFetchClientInterface.getPerasWeightSnapshot =
              pure (WithFingerprint emptyPerasWeightSnapshot (Fingerprint 0))
          }
  area <-
    newLeiosStagingArea
      nullTracer
      registry
      leiosDb
      handles
      ready
      defView
      gcInterval
  pure Fixture{area, leiosDb, candidateVar, admittedVar, currentChainVar}

-- | A minimal 'ChainSyncClientHandleCollection' backed by a single
-- 'StrictTVar' map. Only the read-side ('cschcMap') and the
-- add-handle action are exercised by the staging area; the others
-- are no-ops.
mockHandleCollection ::
  StrictTVar IO (Map TestPeer (ChainSyncClientHandle IO LeiosBlock)) ->
  ChainSyncClientHandleCollection TestPeer IO LeiosBlock
mockHandleCollection v =
  ChainSyncClientHandleCollection
    { cschcMap = readTVar v
    , cschcSeq = pure Seq.empty
    , cschcAddHandle = \peer h -> modifyTVar v (Map.insert peer h)
    , cschcRemoveHandle = \peer -> modifyTVar v (Map.delete peer)
    , cschcRotateHandle = \_ -> pure ()
    , cschcRemoveAllHandles = modifyTVar v (const Map.empty)
    }

------------------------------------------------------------
-- Test helpers
------------------------------------------------------------

largeGcInterval, shortGcInterval :: DiffTime
largeGcInterval = 3600
shortGcInterval = 0.1

-- | Per-test timeout (in microseconds) waiting for the staging
-- gate's 'AddBlockPromise' to resolve. If the gate parks the calling
-- thread forever (e.g. the GC has been disabled), the test would
-- otherwise hang the whole suite.
testTimeoutMicros :: Int
testTimeoutMicros = 10_000_000

-- | Drive the gate. If the gate's promise doesn't resolve within the
-- per-test timeout, fail the assertion (the gate is parking the
-- thread but no drain / GC is filling its outcome handle).
addBlockViaGate ::
  Fixture ->
  LeiosBlock ->
  IO (AddBlockPromise IO LeiosBlock)
addBlockViaGate fixture blk = do
  m <-
    timeout testTimeoutMicros $
      BlockFetchClientInterface.addBlockAsync
        (wrappedChainDbView fixture.area)
        InvalidBlockPunishment.noPunishment
        blk
  case m of
    Just promise -> pure promise
    Nothing ->
      assertFailure
        "Staging gate did not return within the per-test timeout; \
        \expected the drain or GC to wake the parked thread."
        >> error "unreachable"

readAdmitted :: Fixture -> IO [LeiosBlock]
readAdmitted = atomically . readTVar . admittedVar

-- | Add a fake peer whose ChainSync candidate is the given chain of
-- blocks (anchored at genesis, oldest-first).
addCandidateContaining :: Fixture -> [LeiosBlock] -> IO ()
addCandidateContaining fixture blocks = do
  let frag =
        AF.fromOldestFirst
          AF.AnchorGenesis
          (map mkHeaderWithTime blocks)
  varState <-
    uncheckedNewTVarM
      ChainSyncState
        { csCandidate = frag
        , csIdling = False
        , csLatestSlot = pure (AF.headSlot frag)
        }
  varJumping <- uncheckedNewTVarM (Disengaged DisengagedDone)
  varJumpInfo <- uncheckedNewTVarM Nothing
  let handle =
        ChainSyncClientHandle
          { cschState = varState
          , cschOnGsmStateChanged = \_ _ -> pure ()
          , cschGDDKill = pure ()
          , cschJumping = varJumping
          , cschJumpInfo = varJumpInfo
          }
  atomically $ modifyTVar (candidateVar fixture) (Map.insert 0 handle)

-- | Wrap a 'LeiosBlock' in 'HeaderWithTime' using a placeholder
-- relative time. The staging gate only reads
-- 'headerLeiosAnnouncement', so the time field is irrelevant.
mkHeaderWithTime :: LeiosBlock -> HeaderWithTime LeiosBlock
mkHeaderWithTime b =
  HeaderWithTime
    { hwtHeader = getHeader b
    , hwtSlotRelativeTime = RelativeTime 0
    }

-- | Insert the EB body and every tx referenced by it, so the LeiosDb
-- fires @AcquiredEbTxs@ for the given point.
completeClosure :: Fixture -> LeiosPoint -> LeiosEb -> IO ()
completeClosure fixture point eb = withLeiosDb fixture.leiosDb $ \con -> do
  let size = leiosEbBytesSize eb
      txList = V.toList (leiosEbTxs eb)
  leiosDbInsertEbPoint con point size
  leiosDbInsertEbBody con point eb
  _ <- leiosDbInsertTxs con [(h, maxSizedTx) | (h, _) <- txList]
  pure ()

mkTestEbHash :: Word -> EbHash
mkTestEbHash seed = MkEbHash $ BS.pack $ replicate 32 (fromIntegral seed)

mkTestTxHash :: Word -> TxHash
mkTestTxHash seed = MkTxHash $ BS.pack $ replicate 32 (fromIntegral seed)

mkTestEb :: Int -> LeiosEb
mkTestEb numTxs =
  MkLeiosEb $
    V.fromList
      [ (mkTestTxHash (fromIntegral i), 100 + fromIntegral i)
      | i <- [0 .. numTxs - 1]
      ]

maxSizedTx :: BS.ByteString
maxSizedTx = BS.replicate 16_384 0
