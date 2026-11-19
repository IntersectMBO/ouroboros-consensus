{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Ownership regressions exercise the production queues, kernel admission and
-- actual in-memory writer. Gates belong to writer/source implementations here.
module Test.Consensus.Leios.LeiosFetch.Ownership (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Class.MonadTest (exploreRaces)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, exploreSimTrace, runSimOrThrow, traceResult)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Ouroboros.Consensus.Block (SlotNo (..), WithOrigin (..))
import Ouroboros.Consensus.BlockchainTime (BlockchainTime (..), CurrentSlot (..), slotLengthFromSec)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..))
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Leios.AnnounceState
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..), subscribeLeiosAcquisitions)
import qualified Ouroboros.Consensus.Leios.NotifyLog as Log
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), OCINStaleness (..))
import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..), packEndorserBlock)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork as Owned
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
import qualified Ouroboros.Consensus.NodeKernel as NK
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( BodyWriteReceipt (..)
  , LeiosDbWriter (..)
  , Promise (..)
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (Fingerprint (..), WithFingerprint (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import System.Random (mkStdGen)
import Test.Cardano.Crypto.Leios.Gen (mkCommitteeFromTestSeats)
import Test.Consensus.Leios.LeiosFetch.Replies
  ( assignRequests
  , fixture
  , newContext
  , tryEnqueueRequest
  )
import qualified Test.Consensus.Leios.NodeUnderTest as Node
import Test.Consensus.Leios.TestBlock
  ( LeiosTestBlock (..)
  , LeiosTestLedgerConfig (..)
  , firstLeiosBlock
  , setAnnouncement
  , setSlot
  , singleNodeLeiosTestConfig
  , successorLeiosBlock
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, counterexample, property, testProperty)
import Test.Util.Orphans.IOLike ()

assertM :: Applicative m => String -> Bool -> m ()
assertM message ok = unless ok (error message)

data WorkerFault = WorkerFault deriving Show

instance Exception WorkerFault

isWorkerFault :: SomeException -> Bool
isWorkerFault err = case E.fromException err of
  Just WorkerFault -> True
  Nothing -> case E.fromException err of
    Just (ExceptionInLinkedThread _ cause) -> isWorkerFault cause
    Nothing -> False

-- These have already passed header validation; the fixture varies ownership
-- events, not validation. Distinct headers exercise global acceptance and
-- metadata ownership; only bodies retain cache points.
validAt :: LeiosPoint -> Word -> ValidAnnouncement LeiosTestBlock
validAt point branch =
  MkValidAnnouncement
    { announcingHeader = maybe (error "fixture announcement") id (mkAnnouncingHeader config header)
    , announcementOnset = RelativeTime 0
    , announcementAge = 0
    , announcementStaleness = FreshOCIN
    , announcementEbBounds = maxBound
    }
 where
  header =
    ltbHeader $
      setSlot (pointSlotNo point) $
        setAnnouncement (pointEbHash point) 100 $
          firstLeiosBlock (fromIntegral branch)
  k = SecurityParam (knownNonZeroBounded @10)
  view =
    MkLeiosLedgerView
      { llvCommittee = snd (mkCommitteeFromTestSeats [])
      , llvQuorumStakeThreshold = maxBound
      , llvAnnouncementPeriod = Milliseconds32 1000
      , llvVotePeriod = Milliseconds32 4000
      , llvDiffusionPeriod = Milliseconds32 1000
      , llvMaxEbBodySize = 5000
      , llvMaxEbTxsSize = 5000
      }
  config =
    singleNodeLeiosTestConfig
      ( LeiosTestLedgerConfig
          (HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled)
          100000
          view
      )
      k

newKernel :: IOLike m => LeiosFetchClientContext m Int -> m (LeiosFetchKernel m () LeiosTestBlock)
newKernel ctx =
  newLeiosFetchKernelFromState
    (emptyLeiosOutstanding (mkStdGen 1) (SlotNo 0))
    (lfcStaticEnv ctx)
    (Receipts.receiptClosing (lfcReceiptTasks ctx))
    $ \out ready runtime bodies retired ->
      ctx
        { lfcOutstanding = out
        , lfcReady = ready
        , lfcRuntime = runtime
        , lfcBodyWork = bodies
        , lfcBodyRetired = retired
        , lfcTracer = nullTracer
        , lfcDetailTracer = nullTracer
        }

kernelReady :: IOLike m => LeiosFetchKernel m peer blk -> m Bool
kernelReady kernel = atomically $
  orElse (W.awaitReadiness (lfcReady (lfkContext kernel)) >> pure True) (pure False)

offerReadiness :: IOSim s Bool
offerReadiness = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let (p, _, _) = fixture
      offer = MkPeerOffer SNothing (OfferedBody 100)
      trusted = MkPeerOffer (SJust maxBound) (OfferedBody 100)
      changed = MkPeerOffer (SJust maxBound) (OfferedBody 200)
  withLeiosFetchPeer kernel () IsNotBigLedgerPeer $ \_ peer -> do
    recordLeiosOffer kernel peer p offer
    first <- kernelReady kernel
    threadDelay 1
    recordLeiosOffer kernel peer p offer
    duplicate <- kernelReady kernel
    recordLeiosOffer kernel peer p (MkPeerOffer SNothing OfferedClosure)
    closure <- kernelReady kernel
    original <- atomically (readTVar (lfpOfferTimes peer))
    recordLeiosOffer kernel peer p trusted
    bounds <- kernelReady kernel
    recordLeiosOffer kernel peer p trusted
    sameBounds <- kernelReady kernel
    recordLeiosOffer kernel peer p changed
    size <- kernelReady kernel
    recordLeiosOffer kernel peer p changed
    sameSize <- kernelReady kernel
    times <- atomically (readTVar (lfpOfferTimes peer))
    offers <- atomically (readTVar (lfpOfferings peer))
    -- An equal offer with missing arrival metadata still changes ranking facts.
    atomically (modifyTVar (lfpOfferTimes peer) (Map.delete p))
    recordLeiosOffer kernel peer p changed
    repaired <- kernelReady kernel
    pure $
      [first, duplicate, closure, bounds, sameBounds, size, sameSize, repaired]
        == [True, False, True, True, False, True, False, True]
        && times == original
        && Map.lookup p times == Just (OfferTimes (Just (Time 0)) (Just (Time 1)))
        && Map.lookup p offers == Just (MkPeerOffer (SJust maxBound) (OfferedBodyAndClosure 200))

prunedOfferReadiness :: IOSim s Bool
prunedOfferReadiness = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let (p, _, _) = fixture
      frontier = pointSlotNo p + 1
      newer = p{pointSlotNo = frontier}
      offer = MkPeerOffer SNothing (OfferedBody 100)
  atomically (writeTVar (lfkPrunedSlot kernel) frontier)
  withLeiosFetchPeer kernel () IsNotBigLedgerPeer $ \_ peer -> do
    recordLeiosOffer kernel peer p offer
    old <- kernelReady kernel
    empty <- atomically $
      (&&) <$> (Map.null <$> readTVar (lfpOfferings peer))
        <*> (Map.null <$> readTVar (lfpOfferTimes peer))
    recordLeiosOffer kernel peer newer offer
    current <- kernelReady kernel
    pure (not old && empty && current)

unregisterReadiness :: IOSim s Bool
unregisterReadiness = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let local = lfkContext kernel
      rt = lfcRuntime local
      (p, eb, _) = fixture
      election = ancElectionId (announcingHeader (validAt p 0))
      pool = Jobs.mkJobPool 20000 128 (IntMap.fromList (zip [0 ..] (endorserBlockReferences eb)))
  modifyMVar_ (lfcOutstanding local) $
    pure . acquireEbBody p pool . focusElectionIfUnfocused election (pointEbHash p)
      . recordReferencedPoint p SNothing
  before <- withLeiosFetchPeer kernel () IsNotBigLedgerPeer $ \lifetime peer -> do
    client <- atomically (R.openClient rt lifetime False (lfpRequestsToSend peer) (pure Nothing))
    modifyMVar_ (lfcOutstanding local) $
      assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty $
        Map.singleton lifetime (Map.singleton p (MkPeerOffer (SJust maxBound) OfferedClosure))
    atomically (R.closeClient client)
    _ <- kernelReady kernel
    out <- readMVar (lfcOutstanding local)
    pure (Map.member lifetime (requestedJobsPerPeer out))
  dirty <- kernelReady kernel
  out <- readMVar (lfcOutstanding local)
  pure (before && dirty && Map.null (requestedJobsPerPeer out))

prunerReadiness :: IOSim s Bool
prunerReadiness = Node.withNode $ \node -> do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let (original, _, _) = fixture
      p = original{pointSlotNo = SlotNo 1}
      local = lfkContext kernel
      election = ancElectionId (announcingHeader (validAt p 0))
  modifyMVar_ (lfcOutstanding local) $
    pure . focusElectionIfUnfocused election (pointEbHash p) . recordReferencedPoint p SNothing
  withLeiosFetchPeer kernel () IsNotBigLedgerPeer $ \lifetime peer -> do
    recordLeiosOffer kernel peer p (MkPeerOffer SNothing (OfferedBody 100))
    client <- atomically (R.openClient (lfcRuntime local) lifetime False (lfpRequestsToSend peer) (pure Nothing))
    modifyMVar_ (lfcOutstanding local) $
      assignRequests (lfcRuntime local) (Time 0) (const (Right maxBound)) Nothing Map.empty $
        Map.singleton lifetime (Map.singleton p (MkPeerOffer (SJust maxBound) (OfferedBody 100)))
    before <- readTVarIO (R.runtimeFacts (lfcRuntime local))
    assertM "fixture did not reserve a request" (not (Map.null (L.leases (R.ledger before))))
    atomically (R.closeClient client)
    after <- readTVarIO (R.runtimeFacts (lfcRuntime local))
    assertM "client cleanup retained physical debt" (Map.null (L.leases (R.ledger after)))
    _ <- kernelReady kernel
    threadDelay 14
    Node.addBlocks node (take 14 (iterate successorLeiosBlock (firstLeiosBlock 0)))
    boundary <- atomically $ do
      ledger <- ChainDB.getImmutableLedger (Node.nodeChainDB node)
      case getTipSlot (ledgerState ledger) of
        NotOrigin slot | slot > SlotNo 1 -> pure slot
        _ -> retry
    pruneLeiosFetchToImmTip kernel boundary
    dirty <- kernelReady kernel
    out <- readMVar (lfcOutstanding local)
    (offers, times) <- atomically $ (,) <$> readTVar (lfpOfferings peer) <*> readTVar (lfpOfferTimes peer)
    pure (dirty && Map.notMember p (ebState out) && Map.notMember p offers && Map.notMember p times)

newAnnouncements :: IOLike m => m (AnnounceState m LeiosTestBlock)
newAnnouncements = newAnnounceState defaultLeiosAnnouncementAgeLimits =<< Log.newNotifyLog

accept ::
  IOLike m =>
  LeiosFetchKernel m () LeiosTestBlock ->
  AnnounceState m LeiosTestBlock ->
  ValidAnnouncement LeiosTestBlock ->
  STM m Bool
accept kernel announcements valid =
  onValidAnnouncement announcements valid >>= \result -> case result of
    NewAnnouncement NotEquivocationProof -> enqueueLeiosAnnouncement kernel valid >> pure True
    NewAnnouncement EquivocationProof -> pure True
    NotNewAnnouncement -> pure False

waitMetadata ::
  IOLike m => LeiosFetchKernel m () LeiosTestBlock -> ValidAnnouncement LeiosTestBlock -> m ()
waitMetadata kernel valid = do
  admission <-
    atomically $
      Owned.submitWork (lfkAnnouncements kernel) (announcingRbHash (announcingHeader valid)) valid
  atomically $ Owned.awaitAdmission admission

tests :: TestTree
tests =
  testGroup
    "node-owned announcement and forge work"
    [ testCase "offer readiness ignores repetitions but preserves new metadata" $
        runSimOrThrow offerReadiness @?= True
    , testCase "pruned offers are silent and the frontier remains inclusive" $
        runSimOrThrow prunedOfferReadiness @?= True
    , testCase "kernel unregister still wakes logical release after client cleanup" $
        runSimOrThrow unregisterReadiness @?= True
    , testCase "real pruning publishes readiness with its advanced frontier" $
        runSimOrThrow prunerReadiness @?= True
    , testCase "announcement processing does not evict a retained body" $
        runSimOrThrow announcementRetention @?= True
    , testCase "the node shared pruner retires announcements, cached bodies and completed forge work" $
        runSimOrThrow sharedPruner @?= True
    , testCase "accepted metadata survives caller death before activation" $
        runSimOrThrow peerDeath @?= True
    , testCase "full metadata admission rolls back global acceptance" $
        runSimOrThrow fullAdmission @?= (False, True)
    , testCase "full forged admission rolls back acceptance, metadata and forge claims" $
        runSimOrThrow fullForgedAdmission @?= True
    , testCase "forge callbacks return after admission, share payload work and preserve accepted announcements" $
        runSimOrThrow forgeJoin @?= (1, 0, 2)
    , testCase "incomplete forged receipt preserves its supervised cause and releases owners" $
        runSimOrThrow incompleteForgedReceipt @?= True
    , testCase "work admitted before activation drains during normal close" $
        runSimOrThrow earlyDrain @?= 2
    , testCase "queued forge suppresses both body and TX dispatch" $
        runSimOrThrow queuedSuppression @?= (False, False, False)
    , testCase "exceptional shutdown joins an interruptible blocked producer" $
        runSimOrThrow abortProducer @?= True
    , testCase "queue retirement fences late positive completion" $ runSimOrThrow lateCompletion @?= True
    , testCase "cancelled joiner does not cancel a shared producer" $ runSimOrThrow cancelledJoin @?= 1
    , testCase "worker failure interrupts drain instead of stranding it" $
        runSimOrThrow failedDrain @?= True
    , testCase "failed workers close dependent operations but preserve the supervised cause" $
        runSimOrThrow failureSignals @?= True
    , testProperty
        "normal drain closes new admission but completes admitted joins"
        propNormalDrainJoin
    , testCase "failed owned work releases its dependent without another fatal cause" $
        runSimOrThrow failedDependency @?= True
    , testCase "cached lookup only suppresses its exact body during writer admission" $
        runSimOrThrow cachedOwnership @?= (True, False, True)
    , testCase "failed startup clears forge claims admitted before workers start" $
        runSimOrThrow failedStartup @?= True
    , testCase "retirement before worker activation does not recreate state or submit payloads" $
        runSimOrThrow retiredBeforeActivation @?= (0, True)
    , testCase "initial certified view, prune-only silence and stale snapshots use the real focuser" $
        runSimOrThrow certifiedNotifications @?= True
    , testCase "decision loop forecasts unpruned work at the newer immutable anchor" $
        runSimOrThrow forecastBehindAnchor @?= True
    , testCase "simultaneous producer failures have one supervised teardown" $
        runSimOrThrow simultaneousFailures @?= True
    , testProperty
        "simultaneous producer failures across scheduler interleavings"
        propSimultaneousFailures
    , testProperty "producer and receipt failures across scheduler interleavings" $ propFailuresWith True
    , testCase "worker starts unmasked even with a masked caller" unmaskedWorker
    ]

peerDeath :: IOSim s Bool
peerDeath = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, _, _) = fixture
      valid = validAt point 0
  accepted <- S.newEmptyTMVarIO
  withAsync
    (atomically (accept kernel announcements valid >>= S.putTMVar accepted) >> atomically retry)
    $ \peer -> do
      atomically (S.readTMVar accepted) >>= assertM "first acceptance"
      cancel peer
  repeatNew <- atomically (accept kernel announcements valid)
  assertM "duplicate must remain seen" (not repeatNew)
  withLeiosFetchWorkers kernel $ do
    waitMetadata kernel valid
    out <- readMVar (lfcOutstanding (lfkContext kernel))
    pure (Map.member point (ebState out) && isFocusedPoint point out)

fullAdmission :: IOSim s (Bool, Bool)
fullAdmission = do
  (ctx, _) <- newContext
  original <- newKernel ctx
  one <- Owned.newOwnedWork "test.metadata" 1 (pure False)
  let kernel = original{lfkAnnouncements = one}
      (point, _, _) = fixture
      first = validAt point 0
      second = validAt point{pointSlotNo = pointSlotNo point + 1} 1
  announcements <- newAnnouncements
  _ <- atomically (accept kernel announcements first)
  withAsync (atomically (accept kernel announcements second)) $ \blocked -> do
    threadDelay 0.01
    poll blocked >>= assertM "capacity must block" . maybe True (const False)
    cancel blocked
  let anc = announcingHeader second
  seen <- atomically $ hasAnnouncement announcements (electionSlot (ancElectionId anc)) (announcingRbHash anc)
  admitted <- withLeiosFetchWorkers kernel $ do
    waitMetadata kernel first
    atomically (accept kernel announcements second)
  pure (seen, admitted)

-- Saturate the real forged queue, independently of metadata capacity. Each slot
-- is a different election; otherwise announcement caps or key coalescing would
-- conceal the queue boundary this scenario is intended to exercise.
fullForgedAdmission :: IOSim s Bool
fullForgedAdmission = do
  (base, notes) <- newContext
  release <- S.newEmptyTMVarIO
  submissions <- S.newTVarIO Map.empty
  let original = lfcWriter base
      writer =
        original
          { writeEbBody = \p body fills given -> do
              atomically $ S.modifyTVar submissions (Map.insertWith (+) p (1 :: Int))
              atomically $ S.readTMVar release
              writeEbBody original p body fills given
          }
      (firstPoint, _, txs) = fixture
      at n = validAt firstPoint{pointSlotNo = SlotNo (5 + fromIntegral n)} n
      pointOf valid = firstPoint{pointSlotNo = electionSlot (ancElectionId (announcingHeader valid))}
      firstFour = map at [0 .. 3]
      fifth = at 4
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  kernel <- newKernel base{lfcWriter = writer}
  announcements <- newAnnouncements
  let ctx = lfkContext kernel
      seen valid =
        hasAnnouncement
          announcements
          (pointSlotNo (pointOf valid))
          (announcingRbHash (announcingHeader valid))
      withCallers [] action = action
      withCallers (v : vs) action = withAsync (onForgedLeiosEb kernel announcements v forged) $ \caller ->
        withCallers vs action <* wait caller
  withLeiosFetchWorkers kernel $
    bracket_ (pure ()) (atomically $ void (S.tryPutTMVar release ())) $
      withCallers firstFour $ do
        atomically $ forM_ firstFour (\v -> seen v >>= check)
        absent <- withAsync (onForgedLeiosEb kernel announcements fifth forged) $ \blocked -> do
          -- Let the new caller run to STM retry; an actor-start latch alone
          -- would not demonstrate an attempted admission.
          threadDelay 0.01
          stillWaiting <- maybe True (const False) <$> poll blocked
          accepted <- atomically (seen fifth)
          cancel blocked
          claims <- R.localForges <$> atomically (readTVar (R.runtimeFacts (lfcRuntime ctx)))
          pure
            ( stillWaiting
                && not accepted
                && Set.notMember (pointOf fifth) claims
            )
        atomically $ S.putTMVar release ()
        onForgedLeiosEb kernel announcements fifth forged
        atomically $ S.readTVar notes >>= check . ((== 10) . length)
        accepted <- atomically (seen fifth)
        counts <- S.readTVarIO submissions
        pure
          ( absent
              && accepted
              && counts == Map.fromList [(pointOf v, 1) | v <- firstFour ++ [fifth]]
          )

forgeJoin :: IOSim s (Int, Int, Int)
forgeJoin = do
  (ctx, notes) <- newContext
  bodyCount <- S.newTVarIO 0
  txCount <- S.newTVarIO 0
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let writer = lfcWriter ctx
      counted =
        writer
          { writeEbBody = \p eb fills given -> do
              atomically (S.modifyTVar bodyCount (+ 1))
              atomically (S.putTMVar entered ())
              atomically (S.readTMVar release)
              writeEbBody writer p eb fills given
          , writeTxs = \p payload -> do
              atomically (S.modifyTVar txCount (+ 1))
              writeTxs writer p payload
          }
      (point, _, txs) = fixture
      first = validAt point 0
      second = validAt point 1
      other = validAt point{pointSlotNo = SlotNo 6, pointEbHash = UnsafeHash (SBS.pack (replicate 32 7))} 2
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  kernel <- newKernel ctx{lfcWriter = counted}
  announcements <- newAnnouncements
  withLeiosFetchWorkers kernel $ do
    _ <- atomically (accept kernel announcements first)
    waitMetadata kernel first
    withAsync (onForgedLeiosEb kernel announcements first forged) $ \caller -> do
      atomically (S.readTMVar entered)
      returned <- SI.timeout 0.01 (wait caller)
      assertM "forge callback waited for writer admission" (returned == Just ())
      before <- S.readTVarIO notes
      assertM "callback return implied acquisition before writer admission" (null before)
      withAsync (onForgedLeiosEb kernel announcements second forged) $ \joiner -> do
        joined <- SI.timeout 0.01 (wait joiner)
        assertM "joined callback waited for writer admission" (joined == Just ())
        cancel caller
        -- Storage A is blocked; unrelated metadata B still registers.
        _ <- atomically (accept kernel announcements other)
        waitMetadata kernel other
        atomically $ S.putTMVar release ()
        wait joiner
    atomically $ S.readTVar notes >>= check . ((== 2) . length)
    -- Retained positive completion prevents another body/TX submission.
    onForgedLeiosEb kernel announcements first forged
    waitMetadata kernel second
  a <- S.readTVarIO bodyCount
  b <- S.readTVarIO txCount
  accepted <- atomically $
    mapM (hasAnnouncement announcements (pointSlotNo point) . announcingRbHash . announcingHeader) [first, second]
  pure (a, b, length (filter id accepted))

earlyDrain :: IOSim s Int
earlyDrain = do
  (ctx, notes) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  withAsync (onForgedLeiosEb kernel announcements valid forged) $ \caller -> do
    atomically $ do
      present <-
        hasAnnouncement announcements (pointSlotNo point) (announcingRbHash (announcingHeader valid))
      check present
    cancel caller
  withLeiosFetchWorkers kernel (pure ())
  length <$> S.readTVarIO notes

queuedSuppression :: IOSim s (Bool, Bool, Bool)
queuedSuppression = do
  (ctx, _) <- newContext
  let rt = lfcRuntime ctx
      (point, _, _) = fixture
  queue <- uncheckedNewTVarM mempty
  client <- atomically $ R.openClient rt 0 False queue (pure Nothing)
  request <-
    atomically $
      tryEnqueueRequest client (Time 0) (LeiosBlockRequest (MkLeiosBlockRequest point 100 maxBound))
  key <- maybe (fail "expected queued body") pure request
  atomically $ R.claimLocalForge rt point
  st <- readTVarIO (R.runtimeFacts rt)
  committed <- atomically $ R.commitRequest client key
  pure (R.workEligible (L.Body point) st, R.workEligible (L.Job point 0) st, committed)

abortProducer :: IOSim s Bool
abortProducer = do
  queue <- Owned.newOwnedWork "test.blockedProducer" 1 (pure False)
  entered <- S.newEmptyTMVarIO
  joined <- S.newTVarIO False
  admission <- atomically $ Owned.submitWork queue (0 :: Int) ()
  result <- try @_ @SomeException
    $ Owned.withOwnedWork
      queue
      ( \_ _ ->
          (atomically (S.putTMVar entered ()) >> atomically retry)
            `finally` atomically (S.writeTVar joined True)
      )
    $ do
      atomically (S.readTMVar entered)
      throwIO (userError "startup/action failure")
  assertM "exception propagated" (either (const True) (const False) result)
  rejected <- try @_ @SomeException $ atomically (Owned.awaitAdmission admission)
  done <- S.readTVarIO joined
  pure (done && either (const True) (const False) rejected)

lateCompletion :: IOSim s Bool
lateCompletion = do
  queue <- Owned.newOwnedWork "test.retiredCompletion" 1 (pure False)
  release <- S.newEmptyTMVarIO
  _ <- atomically $ Owned.submitWork queue (0 :: Int) ()
  let process key _ = pure $ do
        when (key == 0) (atomically $ S.readTMVar release)
        pure True
      available key = atomically $ Owned.canSubmitWork queue key >>= check
  Owned.withOwnedWork queue process $ do
    joined <- atomically $ Owned.submitWork queue 0 ()
    atomically $ Owned.awaitAdmission joined
    atomically $ Owned.pruneCompleted queue (>= 1) >> S.putTMVar release ()
    available 1
    -- Completion after retirement must not bank the old key.
    old <- atomically $ Owned.submitWork queue 0 ()
    assertM "late completion recreated retired evidence" (Owned.newlyOwned old)
    available 1
    _ <- atomically $ Owned.submitWork queue 1 ()
    available 2
    _ <- atomically $ Owned.submitWork queue 2 ()
    available 3
    -- Two positive completion keys coexist with a one-payload hard bound.
    (one, two) <- atomically $ (,) <$> Owned.submitWork queue 1 () <*> Owned.submitWork queue 2 ()
    assertM "recent completion lost deduplication" (not (Owned.newlyOwned one || Owned.newlyOwned two))
    atomically $ Owned.pruneCompleted queue (>= 2)
    (pruned, retained) <-
      atomically $ (,) <$> Owned.submitWork queue 1 () <*> Owned.submitWork queue 2 ()
    pure (Owned.newlyOwned pruned && not (Owned.newlyOwned retained))

cancelledJoin :: IOSim s Int
cancelledJoin = do
  queue <- Owned.newOwnedWork "test.shared" 1 (pure False)
  count <- S.newTVarIO 0
  gate <- S.newEmptyTMVarIO
  _ <- atomically $ Owned.submitWork queue (0 :: Int) ()
  Owned.withOwnedWork
    queue
    ( \_ _ -> do
        atomically (S.modifyTVar count (+ 1))
        atomically (S.readTMVar gate)
        pure (pure True)
    )
    $ do
      withAsync (atomically (Owned.submitWork queue 0 ()) >>= atomically . Owned.awaitAdmission) $ \joiner -> do
        atomically (S.readTVar count >>= check . (== 1))
        cancel joiner
      atomically $ S.putTMVar gate ()
  S.readTVarIO count

failedDrain :: IOSim s Bool
failedDrain = do
  queue <- Owned.newOwnedWork "test.failure" 1 (pure False)
  _ <- atomically $ Owned.submitWork queue (0 :: Int) ()
  result <-
    try @_ @SomeException $
      Owned.withOwnedWork queue (\_ _ -> throwIO (userError "producer failure")) (pure ())
  pure $ either (const True) (const False) result

-- Keep the supervising application inside its original fault handler while
-- exercising dependents. This observes the failed pool before abort clears it,
-- without exposing a production scheduling hook or delaying an async exception
-- under an uninterruptible mask.
failureSignals :: forall s. IOSim s Bool
failureSignals = do
  queue <- Owned.newOwnedWork "test.failureSignals" 1 (pure False)
  entered <- S.newEmptyTMVarIO
  failNow <- S.newEmptyTMVarIO
  observed <- S.newEmptyTMVarIO
  releaseOwner <- S.newEmptyTMVarIO
  joined <- S.newTVarIO False
  admission <- atomically $ Owned.submitWork queue (0 :: Int) ()
  let process _ _ =
        ( do
            atomically $ S.putTMVar entered ()
            atomically $ S.readTMVar failNow
            throwIO WorkerFault
        )
          `finally` atomically (S.writeTVar joined True)
      ownerAction :: IOSim s (Either SomeException ())
      ownerAction = try $ Owned.withOwnedWork queue process $
        atomically retry `catch` \(err :: ExceptionInLinkedThread) -> do
          atomically $ S.putTMVar observed (toException err)
          atomically $ S.readTMVar releaseOwner
          throwIO err
      expectClosed :: IOSim s a -> IOSim s ()
      expectClosed action = do
        result <- try @_ @Owned.WorkClosed action
        assertM
          "dependent operation did not report WorkClosed"
          (either (const True) (const False) result)
      submit = atomically $ Owned.submitWork queue 1 ()
      capacity = atomically $ Owned.canSubmitWork queue 1 >>= check
      join = atomically $ Owned.awaitAdmission admission
  withAsync ownerAction $ \owner -> do
    atomically $ S.readTMVar entered
    withAsync (expectClosed submit) $ \submitter ->
      withAsync (expectClosed capacity) $ \waiter ->
        withAsync (expectClosed join) $ \joiner -> do
          -- Logical time advances only after runnable actors reach their waits.
          threadDelay 0.001
          forM_ [submitter, waiter, joiner] $ \actor ->
            poll actor
              >>= assertM "dependent did not wait before failure" . maybe True (const False)
          atomically $ S.putTMVar failNow ()
          atomically (S.readTMVar observed)
            >>= assertM "supervisor lost the original cause" . isWorkerFault
          mapM_ wait [submitter, waiter, joiner]
          expectClosed submit
          expectClosed (atomically $ Owned.canSubmitWork queue 1)
          expectClosed join
          atomically $ Owned.trySubmitWork queue 2 ()
          atomically $ S.putTMVar releaseOwner ()
          outcome <- wait owner
          assertM
            "scope did not propagate the original cause"
            (either isWorkerFault (const False) outcome)
          expectClosed submit
          expectClosed (atomically $ Owned.canSubmitWork queue 1)
          expectClosed join
  threadDelay 0.001
  S.readTVarIO joined

normalDrainJoin :: IOSim s Bool
normalDrainJoin = do
  closing <- S.newTVarIO False
  queue <- Owned.newOwnedWork "test.normalDrain" 1 (S.readTVar closing)
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  admission <- atomically $ Owned.submitWork queue (0 :: Int) ()
  let process _ _ = do
        atomically $ S.putTMVar entered ()
        atomically $ S.readTMVar release
        pure (pure False)
  withAsync (Owned.withOwnedWork queue process (atomically $ S.readTMVar entered)) $ \owner -> do
    -- Observe admission closing while the accepted operation is still running.
    atomically $ do
      closed <- (False <$ Owned.canSubmitWork queue 1) `catch` \Owned.WorkClosed -> pure True
      check closed
    atomically $ S.writeTVar closing True
    withAsync (atomically $ Owned.awaitAdmission admission) $ \joiner -> do
      threadDelay 0.001
      poll joiner
        >>= assertM "normal drain rejected or completed a pending join" . maybe True (const False)
      atomically $ S.putTMVar release ()
      wait joiner
    wait owner
  atomically $ Owned.awaitAdmission admission
  pure True

propNormalDrainJoin :: Property
propNormalDrainJoin = exploreSimTrace id (exploreRaces >> normalDrainJoin) $ \_ trace ->
  case traceResult False trace of
    Left err -> counterexample (show err) False
    Right ok -> property ok

-- A metadata-cache failure is no longer a possible production boundary.
-- Exercise the remaining dependency contract through the public OwnedWork
-- callbacks: a failed parent must wake a dependent admission wait, run its
-- cleanup and preserve the parent's original supervised cause.
failedDependency :: IOSim s Bool
failedDependency = do
  parent <- Owned.newOwnedWork "test.parent" 1 (pure False)
  dependent <- Owned.newOwnedWork "test.dependent" 1 (pure False)
  parentReady <- S.newEmptyTMVarIO
  childReady <- S.newEmptyTMVarIO
  gate <- S.newEmptyTMVarIO
  held <- S.newTVarIO True
  writes <- S.newTVarIO (0 :: Int)
  admission <- atomically $ Owned.submitWork parent (0 :: Int) ()
  _ <- atomically $ Owned.submitWork dependent (0 :: Int) ()
  let failParent _ _ = do
        atomically $ S.putTMVar parentReady ()
        atomically $ S.readTMVar gate
        throwIO WorkerFault
      child _ _ =
        ( do
            atomically $ S.putTMVar childReady ()
            atomically $ Owned.awaitAdmission admission
            atomically $ S.modifyTVar writes (+ 1)
            pure (pure False)
        )
          `finally` atomically (S.writeTVar held False)
          `catch` \Owned.WorkClosed -> pure (pure False)
  result <- try @_ @SomeException $
    Owned.withOwnedWork parent failParent $
      Owned.withOwnedWork dependent child $
        ( do
            atomically $ S.readTMVar parentReady >> S.readTMVar childReady >> S.putTMVar gate ()
            atomically retry
        )
          `catch` \(err :: ExceptionInLinkedThread) -> do
            assertM "parent fault was replaced" (isWorkerFault (toException err))
            atomically $ do
              Owned.canSubmitWork dependent 0 >>= check
              S.readTVar held >>= check . not
            throwIO err
  stillHeld <- S.readTVarIO held
  count <- S.readTVarIO writes
  pure (not stillHeld && count == 0 && either isWorkerFault (const False) result)

-- Violate confirmation only after the real in-memory full write. The receipt
-- supervisor must keep that local invariant fault while cancelling the waiting
-- forge worker and clearing its exact local-payload claim.
incompleteForgedReceipt :: IOSim s Bool
incompleteForgedReceipt = do
  (base, notes) <- newContext
  locations <- S.newTVarIO (0 :: Int)
  let (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
      writer = lfcWriter base
      cache = lfcTxCache base
      ctx = base
        { lfcWriter = writer
            { writeEbBody = \p eb fills given -> do
                written <- writeEbBody writer p eb fills given
                pure $ Promise $ await written >> pure (BodyPresent (Set.singleton 0))
            }
        , lfcTxCache = cache
            { setTxLocations = \p locs -> do
                atomically $ S.modifyTVar locations (+ 1)
                setTxLocations cache p locs
            }
        }
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  onForgedLeiosEb kernel announcements valid forged
  result <- try @_ @SomeException $ withLeiosFetchWorkers kernel (atomically retry)
  original <- case result of
    Left err -> case E.fromException err of
      Just (ExnLeiosForgedWriteIncomplete p expected held) ->
        pure (p == point && expected == 2 && held == 1)
      Nothing -> pure False
    Right () -> pure False
  out <- readMVar (lfcOutstanding (lfkContext kernel))
  facts <- readTVarIO (R.runtimeFacts (lfcRuntime (lfkContext kernel)))
  acquired <- S.readTVarIO notes
  published <- S.readTVarIO locations
  pure
    ( original && null acquired && published == 0 && Map.null (closureCoverage out)
        && Map.null (R.pending facts) && Set.null (R.localForges facts)
    )

unmaskedWorker :: Assertion
unmaskedWorker = do
  queue <- Owned.newOwnedWork "test.masking" 1 (pure False)
  state <- S.newEmptyTMVarIO
  _ <- atomically $ Owned.submitWork queue (0 :: Int) ()
  E.mask_ $
    Owned.withOwnedWork
      queue
      ( \_ _ -> do
          maskState <- E.getMaskingState
          atomically (S.putTMVar state maskState)
          pure (pure False)
      )
      (pure ())
  atomically (S.readTMVar state) >>= (@?= E.Unmasked)

cachedOwnership :: IOSim s (Bool, Bool, Bool)
cachedOwnership = do
  (ctx, notes) <- newContext
  let (point, eb, _) = fixture
      valid = validAt point 0
      cache = lfcTxCache ctx
      writer = lfcWriter ctx
  _ <- insertBody cache point (packEndorserBlock eb) () (\w _ _ _ _ -> w)
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let blockedWriter =
        writer
          { writeEbBody = \p body fills txs -> do
              atomically $ S.putTMVar entered ()
              atomically $ S.readTMVar release
              writeEbBody writer p body fills txs
          }
  kernel <- newKernel ctx{lfcWriter = blockedWriter}
  announcements <- newAnnouncements
  _ <- atomically (accept kernel announcements valid)
  before <- readTVarIO (R.runtimeFacts (lfcRuntime (lfkContext kernel)))
  withLeiosFetchWorkers kernel $ do
    atomically $ S.readTMVar entered
    during <- readTVarIO (R.runtimeFacts (lfcRuntime (lfkContext kernel)))
    atomically $ S.putTMVar release ()
    atomically $ S.readTVar notes >>= check . (not . null)
    pure
      ( R.workEligible (L.Body point) before
      , R.workEligible (L.Body point) during
      , R.workEligible (L.Job point 0) during
      )

failedStartup :: IOSim s Bool
failedStartup = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  withAsync (onForgedLeiosEb kernel announcements valid forged) $ \caller -> do
    atomically $
      hasAnnouncement announcements (pointSlotNo point) (announcingRbHash (announcingHeader valid))
        >>= check
    cancel caller
  result <-
    try @_ @SomeException $ withLeiosFetchWorkers kernel $ throwIO (userError "startup failure")
  state <- readTVarIO (R.runtimeFacts (lfcRuntime (lfkContext kernel)))
  pure (either (const True) (const False) result && Set.null (R.localForges state))

retiredBeforeActivation :: IOSim s (Int, Bool)
retiredBeforeActivation = do
  (ctx, notes) <- newContext
  calls <- S.newTVarIO 0
  let original = lfcWriter ctx
      writer =
        original
          { writeEbBody = \p eb fills given -> do
              atomically $ S.modifyTVar calls (+ 1)
              writeEbBody original p eb fills given
          }
      (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  kernel <- newKernel ctx{lfcWriter = writer}
  announcements <- newAnnouncements
  withAsync (onForgedLeiosEb kernel announcements valid forged) $ \caller -> do
    atomically $
      hasAnnouncement announcements (pointSlotNo point) (announcingRbHash (announcingHeader valid))
        >>= check
    cancel caller
  _ <- evictOlderThan (lfcTxCache ctx) (SlotNo 6)
  modifyMVar_ (lfcOutstanding (lfkContext kernel)) (pure . pruneOutstandingToImmTip (SlotNo 6))
  atomically $ writeTVar (lfkPrunedSlot kernel) (SlotNo 6)
  withLeiosFetchWorkers kernel $ do
    waitMetadata kernel valid
    -- A new callback at an already retired point cannot recreate payload work.
    onForgedLeiosEb kernel announcements valid forged
  out <- readMVar (lfcOutstanding (lfkContext kernel))
  acquired <- S.readTVarIO notes
  count <- S.readTVarIO calls
  pure (count, Map.null (ebState out) && null acquired)

-- Override only the ChainDB getter and use a separate kernel: the fixture
-- node's ordinary focuser cannot make these assertions pass. Version-zero
-- contents test initial acquisition, not a fabricated certificate-validation
-- history. Subsequent publications exercise the same production loop.
certifiedNotifications :: IOSim s Bool
certifiedNotifications = Node.withNode $ \node -> do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let (first, _, _) = fixture
      next = first{pointSlotNo = pointSlotNo first + 1}
      stale = first{pointSlotNo = pointSlotNo first - 1}
      election point = ancElectionId (announcingHeader (validAt point 0))
      initial = WithFingerprint (Map.singleton (election first) (pointEbHash first)) (Fingerprint 0)
      local = lfkContext kernel
  snapshots <- S.newTVarIO initial
  let database = (Node.nodeChainDB node){ChainDB.getLeiosCertifiedEbs = S.readTVar snapshots}
      awaitFocus = atomically $ W.awaitReadiness (lfcReady local)
  withAsync (leiosFetchCertificateFocuser database kernel) $ \focuser -> do
    link focuser
    awaitFocus
    firstOut <- readMVar (lfcOutstanding local)
    assertM "initial version-zero snapshot was ignored" (isFocusedPoint first firstOut)
    atomically $ S.writeTVar snapshots (WithFingerprint Map.empty (Fingerprint 0))
    quiet <- SI.timeout 0.01 awaitFocus
    assertM "pruning alone woke the focuser" (quiet == Nothing)
    modifyMVar_ (lfcOutstanding local) (pure . pruneOutstandingToImmTip (pointSlotNo next))
    atomically $ do
      writeTVar (lfkPrunedSlot kernel) (pointSlotNo next)
      S.writeTVar
        snapshots
        ( WithFingerprint
            (Map.fromList [(election stale, pointEbHash stale), (election next, pointEbHash next)])
            (Fingerprint 1)
        )
    awaitFocus
    following <- readMVar (lfcOutstanding local)
    pure
      ( isFocusedPoint next following
          && Map.member next (ebState following)
          && not (isFocusedPoint stale following)
          && Map.notMember stale (ebState following)
          && Map.notMember first (ebState following)
      )

-- A separate real kernel retains old work while the real ChainDB advances.
-- Its pruner has not caught up. A fixture clock isolates forecast anchoring from
-- diffusion-window expiry; no production scheduling hook is required.
forecastBehindAnchor :: IOSim s Bool
forecastBehindAnchor = Node.withNode $ \node -> do
  threadDelay 14
  Node.addBlocks node (take 14 (iterate successorLeiosBlock (firstLeiosBlock 0)))
  anchor <- atomically $ do
    ledger <- ChainDB.getImmutableLedger (Node.nodeChainDB node)
    case getTipSlot (ledgerState ledger) of
      NotOrigin slot | slot > SlotNo 1 -> pure slot
      _ -> retry
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  let (original, _, _) = fixture
      point = original{pointSlotNo = anchor - 1}
      election = ancElectionId (announcingHeader (validAt point 0))
      clock = (NK.getBlockchainTime (Node.nodeKernel node)){getCurrentSlot = pure (CurrentSlot anchor)}
  modifyMVar_ (lfcOutstanding (lfkContext kernel)) $
    pure
      . focusElectionIfUnfocused election (pointEbHash point)
      . recordReferencedPoint point SNothing
  withLeiosFetchWorkers kernel $ withLeiosFetchPeer kernel () IsNotBigLedgerPeer $ \lifetime peer -> do
    _ <-
      atomically $
        R.openClient (lfcRuntime (lfkContext kernel)) lifetime False (lfpRequestsToSend peer) (pure Nothing)
    recordLeiosOffer kernel peer point (MkPeerOffer SNothing (OfferedBody 100))
    withAsync
      (leiosFetchDecisionLoop (Node.nodeTopLevelConfig node) (Node.nodeChainDB node) clock kernel)
      $ \worker -> do
        link worker
        atomically $ readTVar (lfpRequestsToSend peer) >>= check . (not . null)
        pure True

-- Several producers can observe the same writer failure. Their original owner
-- must complete cleanup without a second linked exception arriving afterwards.
simultaneousFailures :: IOSim s Bool
simultaneousFailures = simultaneousFailuresWith False

simultaneousFailuresWith :: forall s. Bool -> IOSim s Bool
simultaneousFailuresWith receiptAlso = do
  tasks <- Receipts.newReceiptTasks
  left <- Owned.newOwnedWork "test.left" 1 (pure False)
  right <- Owned.newOwnedWork "test.right" 1 (pure False)
  gate <- S.newEmptyTMVarIO
  entered <- S.newTVarIO (0 :: Int)
  joined <- S.newTVarIO (0 :: Int)
  _ <- atomically $ Owned.submitWork left (0 :: Int) ()
  _ <- atomically $ Owned.submitWork right (0 :: Int) ()
  let count = if receiptAlso then 3 else 2
      failAfterGate :: IOSim s ()
      failAfterGate =
        ( do
            exploreRaces
            atomically $ S.modifyTVar entered (+ 1)
            atomically $ S.readTMVar gate
            throwIO (userError "shared writer failure")
        )
          `finally` atomically (S.modifyTVar joined (+ 1))
      producer _ _ = failAfterGate >> pure (pure False)
  result <- try @_ @SomeException $ Receipts.withReceiptTasks tasks $ do
    when receiptAlso $ Receipts.withReceiptSubmission tasks $ Receipts.forkReceipt tasks failAfterGate
    Owned.withOwnedWork left producer $ Owned.withOwnedWork right producer $ do
      atomically $ S.readTVar entered >>= check . (== count)
      atomically $ S.putTMVar gate ()
      atomically retry
  threadDelay 0.001
  stopped <- S.readTVarIO joined
  pure (either (const True) (const False) result && stopped == count)

propSimultaneousFailures :: Property
propSimultaneousFailures = propFailuresWith False

propFailuresWith :: Bool -> Property
propFailuresWith receiptAlso = exploreSimTrace id (exploreRaces >> simultaneousFailuresWith receiptAlso) $ \_ trace ->
  case traceResult False trace of
    Left err -> counterexample (show err) False
    Right ok -> property ok

-- The old model evicted a seed body after 128 new announcements. Drain the
-- real metadata worker so this checks processing rather than queue admission.
announcementRetention :: IOSim s Bool
announcementRetention = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, eb, _) = fixture
      other = point{pointEbHash = UnsafeHash (SBS.replicate 32 77)}
      valid n = validAt other{pointSlotNo = pointSlotNo point + fromIntegral n} n
      cache = lfcTxCache (lfkContext kernel)
  void $ insertBody cache point (packEndorserBlock eb) () (\acc _ _ _ _ -> acc)
  withLeiosFetchWorkers kernel $ do
    forM_ [1 .. 129] $ \n -> atomically (accept kernel announcements (valid n)) >>= assertM "first announcement rejected"
    waitMetadata kernel (valid 129)
  found <- lookupBody cache (pointEbHash point)
  pure (maybe False (const True) found)

-- Use the worker installed by initNodeKernel and a real positive forged-write
-- receipt before advancing the actual ChainDB's immutable tip.
sharedPruner :: IOSim s Bool
sharedPruner = Node.withNode $ \node -> do
  let kernel = NK.getLeiosFetchKernel (Node.nodeKernel node)
      announcements = NK.getLeiosAnnounceState (Node.nodeKernel node)
      ctx = lfkContext kernel
      (seed, _, txs) = fixture
      point = seed{pointSlotNo = 1}
      valid = validAt point 0
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
      key = announcingRbHash (announcingHeader valid)
  next <- subscribeLeiosAcquisitions (Node.nodeLeiosAcquisitions node)
  onForgedLeiosEb kernel announcements valid forged
  let awaitClosure = do
        acquired <- next
        case acquired of
          AcquiredEbTxs p | p == point -> pure ()
          _ -> awaitClosure
  atomically awaitClosure
  atomically $ Owned.canSubmitWork (lfkForged kernel) point >>= check
  previous <- atomically $ Owned.submitWork (lfkForged kernel) point (forged, pure ())
  assertM "positive forge completion was not retained" (not (Owned.newlyOwned previous))
  before <- lookupBody (lfcTxCache ctx) (pointEbHash point)
  assertM "the completed body was not cached" (maybe False (const True) before)
  threadDelay 14
  Node.addBlocks node (take 14 (iterate successorLeiosBlock (firstLeiosBlock 0)))
  atomically $ readTVar (lfkPrunedSlot kernel) >>= check . (> pointSlotNo point)
  seen <- atomically $ hasAnnouncement announcements (pointSlotNo point) key
  after <- lookupBody (lfcTxCache ctx) (pointEbHash point)
  out <- readMVar (lfcOutstanding ctx)
  following <- atomically $ Owned.submitWork (lfkForged kernel) point (forged, pure ())
  atomically $ Owned.awaitAdmission following
  pure (not seen && maybe True (const False) after && Map.notMember point (ebState out) && Owned.newlyOwned following)
