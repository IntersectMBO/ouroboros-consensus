module Test.Consensus.GSM.PeerState (tests) where

import qualified Control.Exception as E
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
import Ouroboros.Consensus.MiniProtocol.Util.Idling
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.GSM.PeerState
import Ouroboros.Consensus.Node.GsmState (GsmState (..))
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , atomically
  , modifyTVar
  , newTVar
  , newTVarIO
  , readTVar
  , withAsync
  , writeTVar
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.PerasSupport (PerasSupport (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests =
  testGroup
    "GSM.PeerState"
    [ testCase "certificate registration, idling, activity and removal" $
        runSimOrThrow lifecycle @?= [False, False, True, False, True, False, True, False]
    , testCase "certificate-first registration and connection-key isolation" $
        runSimOrThrow certificateFirst @?= [False, False, True, False]
    , testCase "ChainSync alone is sufficient only without negotiated Peras" $
        runSimOrThrow unsupported @?= [False, True]
    , testCase "real GSM waits for certificate idling before entering CaughtUp" $
        runSimOrThrow transition @?= [Syncing, Syncing, CaughtUp]
    , testCase "client failure removes its certificate handle" $ do
        (cs, cert) <- newHandles
        _ <- addChainSync cs 0 PerasSupported True
        result <-
          E.try
            ( bracketObjectDiffusionInbound cert 0 $ \view -> do
                idlingStart (odisvIdling view)
                allIdle cs cert >>= (@?= True)
                E.throwIO (userError "certificate client failed")
            ) ::
            IO (Either IOError ())
        case result of
          Left _ -> pure ()
          Right () -> assertFailure "expected the client exception"
        remaining <- atomically $ odihcMap cert
        assertBool "failed client retained a handle" (Map.null remaining)
        allIdle cs cert >>= (@?= False)
    ]

-- Use the real collections and predicate used by NodeKernel. In particular,
-- reading this STM action subscribes the GSM to both registration and idling.
type CsHandles m = ChainSyncClientHandleCollection Int m TestBlock
type CertHandles m = ObjectDiffusionInboundHandleCollection Int m TestBlock

newHandles :: IOLike m => m (CsHandles m, CertHandles m)
newHandles =
  atomically $
    (,) <$> newChainSyncClientHandleCollection <*> newObjectDiffusionInboundHandleCollection

addChainSync ::
  IOLike m => CsHandles m -> Int -> PerasSupport -> Bool -> m (ChainSyncClientHandle m TestBlock)
addChainSync handles peer support idle = atomically $ do
  state <-
    newTVar
      ChainSyncState
        { csCandidate = AF.Empty AF.AnchorGenesis
        , csLatestSlot = SNothing
        , csIdling = idle
        , csPerasSupport = support
        }
  jumping <- newTVar (Disengaged DisengagedDone)
  jumpInfo <- newTVar Nothing
  let handle =
        ChainSyncClientHandle
          { cschState = state
          , cschGDDKill = pure ()
          , cschOnGsmStateChanged = \_ _ -> pure ()
          , cschJumping = jumping
          , cschJumpInfo = jumpInfo
          }
  cschcAddHandle handles peer handle
  pure handle

setChainSyncIdle :: IOLike m => ChainSyncClientHandle m TestBlock -> Bool -> m ()
setChainSyncIdle handle idle = atomically $ modifyTVar (cschState handle) $ \s -> s{csIdling = idle}

allIdle :: IOLike m => CsHandles m -> CertHandles m -> m Bool
allIdle cs cert = atomically $ do
  peers <- mkGsmPeerStates cs cert
  pure (not (Map.null peers) && all gsmPeerIsIdle peers)

lifecycle :: IOSim s [Bool]
lifecycle = do
  (cs, cert) <- newHandles
  handle <- addChainSync cs 0 PerasSupported True
  missing <- allIdle cs cert
  during <- bracketObjectDiffusionInbound cert 0 $ \view -> do
    registered <- allIdle cs cert
    idlingStart (odisvIdling view)
    idle <- allIdle cs cert
    idlingStop (odisvIdling view)
    active <- allIdle cs cert
    idlingStart (odisvIdling view)
    idleAgain <- allIdle cs cert
    setChainSyncIdle handle False
    chainActive <- allIdle cs cert
    setChainSyncIdle handle True
    bothIdle <- allIdle cs cert
    pure [registered, idle, active, idleAgain, chainActive, bothIdle]
  removed <- allIdle cs cert
  pure (missing : during ++ [removed])

certificateFirst :: IOSim s [Bool]
certificateFirst = do
  (cs, cert) <- newHandles
  bracketObjectDiffusionInbound cert 0 $ \view -> do
    idlingStart (odisvIdling view)
    noChain <- allIdle cs cert
    _ <- addChainSync cs 1 PerasSupported True
    wrongConnection <- allIdle cs cert
    atomically $ cschcRemoveHandle cs 1
    _ <- addChainSync cs 0 PerasSupported True
    paired <- allIdle cs cert
    atomically $ cschcRemoveHandle cs 0
    removed <- allIdle cs cert
    pure [noChain, wrongConnection, paired, removed]

unsupported :: IOSim s [Bool]
unsupported = do
  (cs, cert) <- newHandles
  handle <- addChainSync cs 0 PerasUnsupported False
  active <- allIdle cs cert
  setChainSyncIdle handle True
  idle <- allIdle cs cert
  pure [active, idle]

transition :: IOSim s [GsmState]
transition = do
  (cs, cert) <- newHandles
  _ <- addChainSync cs 0 PerasSupported True
  gsmState <- newTVarIO PreSyncing
  let entry =
        GSM.realGsmEntryPoints
          (id, nullTracer)
          GSM.GsmView
            { GSM.antiThunderingHerd = Nothing
            , GSM.getCandidateOverSelection = pure $ \() _ -> GSM.WhetherCandidateIsBetter False
            , GSM.peerIsIdle = gsmPeerIsIdle
            , GSM.durationUntilTooOld = Nothing
            , GSM.equivalent = (==)
            , GSM.getPeerStates = mkGsmPeerStates cs cert
            , GSM.getCurrentSelection = pure ()
            , GSM.minCaughtUpDuration = 0
            , GSM.setCaughtUpPersistentMark = \_ -> pure ()
            , GSM.writeGsmState = atomically . writeTVar gsmState
            , GSM.isHaaSatisfied = pure True
            }
      -- Virtual time lets the GSM run to its next STM wait without wall-clock
      -- sleeps or scheduler-dependent assertions about absence of a transition.
      observe = SI.threadDelay 1 >> atomically (readTVar gsmState)
  withAsync (GSM.enterPreSyncing entry) $ \_ -> do
    missing <- observe
    bracketObjectDiffusionInbound cert 0 $ \view -> do
      registered <- observe
      idlingStart (odisvIdling view)
      caughtUp <- observe
      pure [missing, registered, caughtUp]
