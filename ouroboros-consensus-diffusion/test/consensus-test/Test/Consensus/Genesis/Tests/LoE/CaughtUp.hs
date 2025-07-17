{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This is a regression test for
-- <https://github.com/IntersectMBO/ouroboros-consensus/issues/1503>.
--
-- Concretely, consider @k = 1@ (security parameter), and a syncing Genesis
-- enabled.
--
-- Now consider the following block tree:
--
-- > G :> A >: C
-- >   :> B
--
-- Suppose that we have two peers, Peer 1 and Peer 2:
--
--  * Peer 1 first sends A, then C, then rolls back to A, and then idles.
--
--  * Peer 2 sends B and then idles.
--
-- In any possible interleaving (tested using IOSimPOR), the node should in the
-- end be caught-up and have selected C as it is the best chain.
--
-- To (somewhat) simplify the test setup boilerplate, we do not actually run
-- ChainSync and BlockFetch, but rather simulate their behavior by modifying the
-- ChainSync client state (eg candidate fragments) as well as adding blocks to
-- the ChainDB.
module Test.Consensus.Genesis.Tests.LoE.CaughtUp (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad (join)
import Control.Monad.Class.MonadTest (MonadTest (..))
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (exploreSimTrace, traceResult)
import Control.ResourceRegistry
import Control.Tracer (nullTracer)
import Data.Function (on)
import Data.Functor (void)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Genesis.Governor (gddWatcher)
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  , newChainSyncClientHandleCollection
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
  ( ChainSyncJumpingState (..)
  , DisengagedInitState (..)
  )
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis (setGetLoEFragment)
import Ouroboros.Consensus.Node.GsmState
import Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Util.AnchoredFragment
  ( preferAnchoredCandidate
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (forkLinkedWatcher)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.ChainDB
import Test.Util.Header
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock

tests :: TestTree
tests = testProperty "Select best chain when CaughtUp" prop_test

prop_test :: Property
prop_test =
  exploreSimTrace id (exploreRaces *> run) \_ tr ->
    case traceResult False tr of
      Right prop -> prop
      Left e -> counterexample ("Failure: " <> show e) False

run :: forall m. (IOLike m, SI.MonadTimer m) => m Property
run = withRegistry \registry -> do
  -- Setup
  varGsmState <- newTVarIO PreSyncing
  varLoEFragment <- newTVarIO $ AF.Empty AF.AnchorGenesis
  varGetLoEFragment <-
    newTVarIO $
      pure $
        ChainDB.LoEEnabled $
          AF.Empty AF.AnchorGenesis
  setGetLoEFragment
    (readTVar varGsmState)
    (readTVar varLoEFragment)
    varGetLoEFragment

  chainDB <- openChainDB registry (join $ readTVarIO varGetLoEFragment)
  let addBlk = ChainDB.addBlock_ chainDB Punishment.noPunishment

  chainSyncHandles <- atomically newChainSyncClientHandleCollection

  _ <-
    forkLinkedThread registry "GSM" $
      GSM.enterPreSyncing $
        mkGsmEntryPoints
          chainSyncHandles
          chainDB
          (atomically . writeTVar varGsmState)

  forkGDD
    registry
    chainSyncHandles
    chainDB
    (readTVar varGsmState)
    varLoEFragment

  -- Make sure that the ChainDB background thread, the GSM and the GDD are
  -- running (any positive amount should do).
  threadDelay 1

  -- Simulate receiving A, B, C and C being rolled back. In the real system,
  -- this would happen via ChainSync and BlockFetch.

  _ <- forkLinkedThread registry "Peer1" $ do
    -- First, let Peer1 connect, serving block A (without idling).
    let initialFrag =
          attachSlotTimeToFragment cfg $
            AF.Empty AF.AnchorGenesis AF.:> getHeader blkA
    hdl <- atomically $ mkTestChainSyncClientHandle initialFrag
    atomically $ cschcAddHandle chainSyncHandles peer1 hdl
    addBlk blkA

    -- Then, send C.
    atomically $ modifyTVar (cschState hdl) $ \s ->
      ChainSyncState
        { csCandidate = csCandidate s AF.:> attachSlotTime cfg (getHeader blkC)
        , csLatestSlot = pure $ NotOrigin $ blockSlot blkC
        , csIdling = csIdling s
        }
    addBlk blkC

    -- Finally, roll back to the initial fragment and idle.
    atomically $ modifyTVar (cschState hdl) $ \_s ->
      ChainSyncState
        { csCandidate = initialFrag
        , csLatestSlot = pure $ AF.headSlot initialFrag
        , csIdling = True
        }

  _ <- forkLinkedThread registry "Peer2" $ do
    -- Let Peer2 connect and send B.
    hdl <-
      atomically $
        mkTestChainSyncClientHandle $
          attachSlotTimeToFragment cfg $
            AF.Empty AF.AnchorGenesis AF.:> getHeader blkB
    atomically $ cschcAddHandle chainSyncHandles peer2 hdl
    addBlk blkB

    -- Finally, idle.
    atomically $ modifyTVar (cschState hdl) $ \s ->
      ChainSyncState
        { csCandidate = csCandidate s
        , csLatestSlot = csLatestSlot s
        , csIdling = True
        }

  -- Give time to process the new blocks (any positive amount should do).
  threadDelay 1

  gsmState <- atomically $ readTVar varGsmState
  tipPt <- atomically $ AF.headPoint <$> ChainDB.getCurrentChain chainDB
  pure $
    conjoin
      [ gsmState === CaughtUp
      , counterexample ("Selection tip is not C") $
          castPoint tipPt === blockPoint blkC
      ]
 where
  peer1, peer2 :: CoreNodeId
  peer1 = CoreNodeId 1
  peer2 = CoreNodeId 2

  blkA, blkB, blkC :: TestBlock
  blkA = firstBlock 1
  blkB = firstBlock 2
  blkC = successorBlock blkA

{-------------------------------------------------------------------------------
  Boilerplate for setting up the various test components
-------------------------------------------------------------------------------}

cfg :: TopLevelConfig TestBlock
cfg =
  singleNodeTestConfigWith
    TestBlockCodecConfig
    TestBlockStorageConfig
    -- To make the test as simple as possible (otherwise, "saturating" the LoE
    -- requires more blocks).
    (SecurityParam $ knownNonZeroBounded @1)
    -- large Genesis window to avoid disconnecting any peers
    (GenesisWindow 20)

mkTestChainSyncClientHandle ::
  forall m.
  IOLike m =>
  AnchoredFragment (HeaderWithTime TestBlock) ->
  STM m (ChainSyncClientHandle m TestBlock)
mkTestChainSyncClientHandle frag = do
  varState <-
    newTVar
      ChainSyncState
        { csCandidate = frag
        , csIdling = False
        , csLatestSlot = pure $ AF.headSlot frag
        }
  varJumping <- newTVar $ Disengaged DisengagedDone
  varJumpInfo <- newTVar Nothing
  pure
    ChainSyncClientHandle
      { cschState = varState
      , -- Irrelevant for this test (as we don't actually run ChainSync).
        cschOnGsmStateChanged = \_gsmState _curTime -> pure ()
      , cschGDDKill = pure ()
      , cschJumping = varJumping
      , cschJumpInfo = varJumpInfo
      }

openChainDB ::
  forall m.
  IOLike m =>
  ResourceRegistry m ->
  ChainDB.GetLoEFragment m TestBlock ->
  m (ChainDB m TestBlock)
openChainDB registry getLoEFragment = do
  chainDbArgs <- do
    mcdbNodeDBs <- emptyNodeDBs
    let mcdbTopLevelConfig = cfg
        configureLoE a =
          a
            { ChainDB.cdbsArgs =
                (ChainDB.cdbsArgs a){ChainDB.cdbsLoE = getLoEFragment}
            }
    pure $
      configureLoE $
        fromMinimalChainDbArgs
          MinimalChainDbArgs
            { mcdbChunkInfo = mkTestChunkInfo mcdbTopLevelConfig
            , mcdbInitLedger = testInitExtLedger
            , mcdbRegistry = registry
            , mcdbTopLevelConfig
            , mcdbNodeDBs
            }
  (_, (chainDB, ChainDB.Impl.Internal{ChainDB.Impl.intAddBlockRunner})) <-
    allocate
      registry
      (\_ -> ChainDB.Impl.openDBInternal chainDbArgs False)
      (ChainDB.closeDB . fst)
  _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
  pure chainDB

mkGsmEntryPoints ::
  forall m.
  (IOLike m, SI.MonadTimer m) =>
  ChainSyncClientHandleCollection CoreNodeId m TestBlock ->
  ChainDB m TestBlock ->
  (GsmState -> m ()) ->
  GSM.GsmEntryPoints m
mkGsmEntryPoints varChainSyncHandles chainDB writeGsmState =
  GSM.realGsmEntryPoints
    (id, nullTracer)
    GSM.GsmView
      { GSM.getCandidateOverSelection = pure candidateOverSelection
      , GSM.peerIsIdle = csIdling
      , GSM.equivalent = (==) `on` AF.headPoint
      , GSM.getChainSyncStates = fmap cschState <$> cschcMap varChainSyncHandles
      , GSM.getCurrentSelection = ChainDB.getCurrentChain chainDB
      , -- Make sure that we stay in CaughtUp for the duration of the test once we
        -- have entered it.
        GSM.minCaughtUpDuration = 10 -- seconds
      , GSM.writeGsmState
      , -- Not interesting for this test.
        GSM.antiThunderingHerd = Nothing
      , GSM.setCaughtUpPersistentMark = \_ -> pure ()
      , GSM.durationUntilTooOld = Nothing
      , GSM.isHaaSatisfied = pure True
      }
 where
  candidateOverSelection selection candidateState =
    case AF.intersectionPoint selection candFrag of
      Nothing -> GSM.CandidateDoesNotIntersect
      Just{} ->
        -- precondition requires intersection
        GSM.WhetherCandidateIsBetter $
          preferAnchoredCandidate (configBlock cfg) selection candFrag
   where
    candFrag = csCandidate candidateState

forkGDD ::
  forall m.
  IOLike m =>
  ResourceRegistry m ->
  ChainSyncClientHandleCollection CoreNodeId m TestBlock ->
  ChainDB m TestBlock ->
  STM m GsmState ->
  StrictTVar m (AnchoredFragment (HeaderWithTime TestBlock)) ->
  m ()
forkGDD registry varChainSyncHandles chainDB getGsmState varLoEFrag =
  void $
    forkLinkedWatcher registry "GDD" $
      gddWatcher
        cfg
        nullTracer
        chainDB
        (0 :: DiffTime) -- no rate limiting
        getGsmState
        (cschcMap varChainSyncHandles)
        varLoEFrag
