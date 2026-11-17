{-# LANGUAGE BangPatterns #-}
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
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Ouroboros.Consensus.Block (SlotNo (..), WithOrigin (..))
import Ouroboros.Consensus.BlockchainTime
  ( BackoffDelay (..)
  , BlockchainTime (..)
  , CurrentSlot (..)
  , HardForkBlockchainTimeArgs (..)
  , SystemTime (..)
  , TraceBlockchainTimeEvent (..)
  , hardForkBlockchainTime
  , slotLengthFromSec
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..))
import Ouroboros.Consensus.Config (SecurityParam (..), configLedger)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..), subscribeLeiosAcquisitions)
import Ouroboros.Consensus.Leios.AnnounceState
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.NewValidClaims
  ( NewLeiosValidClaims (..)
  , mkNewLeiosValidClaims
  )
import qualified Ouroboros.Consensus.Leios.NotifyLog as Log
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement
  , LeiosLedgerView (..)
  , OCINStaleness (..)
  )
import Ouroboros.Consensus.Leios.TxCache.API
  ( LandedTx (..)
  , LeiosTxCache (..)
  , TxArrivalPrior (..)
  , TxState (..)
  , bucketTxArrival
  , evictOlderThan
  , insertBody
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
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
kernelReady kernel =
  atomically $
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
    empty <-
      atomically $
        (&&)
          <$> (Map.null <$> readTVar (lfpOfferings peer))
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
    pure
      . acquireEbBody p pool
      . focusElectionIfUnfocused election (pointEbHash p)
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
    client <-
      atomically (R.openClient (lfcRuntime local) lifetime False (lfpRequestsToSend peer) (pure Nothing))
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
    (offers, times) <-
      atomically $ (,) <$> readTVar (lfpOfferings peer) <*> readTVar (lfpOfferTimes peer)
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
    , testCase "the node shared pruner retires announcements, body references and completed forge work" $
        runSimOrThrow sharedPruner @?= True
    , testCase "accepted metadata survives caller death before activation" $
        runSimOrThrow peerDeath @?= True
    , testCase "full metadata admission rolls back global acceptance" $
        runSimOrThrow fullAdmission @?= (False, True)
    , testCase "full forged admission rolls back acceptance, metadata and forge claims" $
        runSimOrThrow fullForgedAdmission @?= True
    , testCase
        "forge callbacks return after admission, share payload work and preserve accepted announcements"
        $ runSimOrThrow forgeJoin @?= (1, 0, 2)
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
    , testCase "failed startup clears forge claims admitted before workers start" $
        runSimOrThrow failedStartup @?= True
    , testCase "retirement before worker activation does not recreate state or submit payloads" $
        runSimOrThrow retiredBeforeActivation @?= (0, True)
    , testGroup
        "verified claim delivery"
        [ testCase "a queued initial claim is focused once and pruning alone does not resume consumption" $
            runSimOrThrow certifiedNotifications @?= True
        , testCase "a queued retired claim cannot restore work or start body preparation" $
            runSimOrThrow delayedCertifiedNotification @?= True
        , testCase "a queued claim exactly at the fetch pruning fence remains admissible" $
            runSimOrThrow certifiedFenceEquality @?= True
        , testCase "ordered claims in one batch override an ordinary announcement" $
            runSimOrThrow (certifiedClaimOrder True) @?= True
        , testCase "ordered claims across batches override an ordinary announcement" $
            runSimOrThrow (certifiedClaimOrder False) @?= True
        , testCase "a closed body owner does not block claim delivery or submit body work" $
            runSimOrThrow certifiedWhileClosing @?= True
        , testCase "failure after a real dequeue reaches the owning registry scope" $
            runSimOrThrow certifiedDequeueFailure @?= True
        , testCase "shutdown joins a focuser waiting for its first claim" $
            runSimOrThrow (certifiedShutdown False) @?= True
        , testCase "shutdown joins a focuser waiting for the outstanding lock and leaves pending input" $
            runSimOrThrow (certifiedShutdown True) @?= True
        ]
    , testCase "decision loop forecasts unpruned work at the newer immutable anchor" $
        runSimOrThrow forecastBehindAnchor @?= True
    , testCase "the real HardFork clock retains its slot/onset pair through a tolerated rewind" $
        runSimOrThrow hardForkOnsetRewind
          @?= ( [(SlotNo 2, RelativeTime 2), (SlotNo 2, RelativeTime 2), (SlotNo 3, RelativeTime 3)]
              , (RelativeTime 2.25, RelativeTime 1.5)
              )
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
  seen <-
    atomically $ hasAnnouncement announcements (electionSlot (ancElectionId anc)) (announcingRbHash anc)
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
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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
  accepted <-
    atomically $
      mapM
        (hasAnnouncement announcements (pointSlotNo point) . announcingRbHash . announcingHeader)
        [first, second]
  pure (a, b, length (filter id accepted))

earlyDrain :: IOSim s Int
earlyDrain = do
  (ctx, notes) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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
      ownerAction =
        try $
          Owned.withOwnedWork queue process $
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
  result <-
    try @_ @SomeException $
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
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
      writer = lfcWriter base
      cache = lfcTxCache base
      ctx =
        base
          { lfcWriter =
              writer
                { writeEbBody = \p eb fills given -> do
                    written <- writeEbBody writer p eb fills given
                    pure $ Promise $ await written >> pure (BodyPresent (Set.singleton 0))
                }
          , lfcTxCache =
              cache
                { insertLandedTxs = \p entries -> do
                    atomically $ S.modifyTVar locations (+ 1)
                    insertLandedTxs cache p entries
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
    ( original
        && null acquired
        && published == 0
        && Map.null (closureCoverage out)
        && Map.null (R.pending facts)
        && Set.null (R.localForges facts)
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

failedStartup :: IOSim s Bool
failedStartup = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  announcements <- newAnnouncements
  let (point, _, txs) = fixture
      valid = validAt point 0
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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

-- Fixture inputs are already-verified claims, not a certificate validator.
-- Separate ChainDB tests exercise publication, validation and deduplication.
claimEvent :: LeiosPoint -> Word -> (LeiosClaim, LeiosAnnouncement)
claimEvent point branch =
  (LeiosClaim (pointSlotNo point) (announcingRbHash anc), ancAnnouncement anc)
 where
  anc = announcingHeader (validAt point branch)

-- Count entry into the public consume action, not trace observation or a read
-- by another consumer. Attempt n+1 proves that batch n was fully processed.
withClaimFocuser ::
  NewLeiosValidClaims (IOSim s) ->
  LeiosFetchKernel (IOSim s) () LeiosTestBlock ->
  (StrictTVar (IOSim s) Int -> IOSim s a) ->
  IOSim s a
withClaimFocuser stream kernel action = do
  attempts <- newTVarIO 0
  let consume = do
        atomically $ modifyTVar attempts (+ 1)
        atomically $ consumeNewLeiosValidClaims stream
  withAsync (leiosFetchCertificateFocuser consume kernel) $ \worker -> do
    link worker
    action attempts

awaitClaimPass :: StrictTVar (IOSim s) Int -> Int -> IOSim s ()
awaitClaimPass attempts n = atomically $ readTVar attempts >>= check . (> n)

certifiedNotifications :: IOSim s Bool
certifiedNotifications = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  stream <- mkNewLeiosValidClaims
  let (point, _, _) = fixture
      (claim, ann) = claimEvent point 0
  atomically $ recordNewLeiosValidClaim stream claim ann
  withClaimFocuser stream kernel $ \attempts -> do
    awaitClaimPass attempts 1
    before <- readMVar (lfcOutstanding (lfkContext kernel))
    assertM "queued initial event was ignored" (isFocusedPoint point before)
    pruneLeiosFetchToImmTip kernel (pointSlotNo point + 1)
    resumed <- SI.timeout 0.01 (awaitClaimPass attempts 2)
    after <- readMVar (lfcOutstanding (lfkContext kernel))
    pure (resumed == Nothing && Map.null (ebState after) && Map.null (elFocus after))

delayedCertifiedNotification :: IOSim s Bool
delayedCertifiedNotification = do
  (base, notes) <- newContext
  submissions <- newTVarIO (0 :: Int)
  let cache = lfcTxCache base
      writer = lfcWriter base
      (point, body, txs) = fixture
      source = point{pointSlotNo = pointSlotNo point + 10}
      (claim, ann) = claimEvent point 0
      counted =
        writer
          { writeEbBody = \p eb fills given ->
              atomically (modifyTVar submissions (+ 1)) >> writeEbBody writer p eb fills given
          }
  -- Even a live newer point holding the same transactions cannot revive the
  -- retired target or supply an unreceived body. Confirm the warm source first.
  receipt <-
    await
      =<< writeEbBody writer source body Map.empty (Map.fromList (zip [0 ..] (map leiosTxBytes txs)))
  assertM "warm source did not commit" (receipt == BodyPresent (Set.fromList [0, 1]))
  _ <- insertBody cache source (packEndorserBlock body)
  let landed =
        [ MkLandedTx off h size (TxInserted ())
        | (off, (h, size)) <- zip [0 ..] (endorserBlockReferences body)
        ]
      !() = foldl' (\() !_ -> ()) () landed
  _ <- insertLandedTxs cache source landed
  kernel <- newKernel base{lfcWriter = counted}
  stream <- mkNewLeiosValidClaims
  atomically $ recordNewLeiosValidClaim stream claim ann
  pruneLeiosFetchToImmTip kernel (pointSlotNo point + 1)
  out <- withLeiosFetchWorkers kernel $ withClaimFocuser stream kernel $ \attempts -> do
    awaitClaimPass attempts 1
    readMVar (lfcOutstanding (lfkContext kernel))
  -- Both the focuser pass and normal worker drain have completed.
  count <- readTVarIO submissions
  acquired <- S.readTVarIO notes
  pure (Map.null (ebState out) && Map.null (elFocus out) && count == 0 && null acquired)

certifiedFenceEquality :: IOSim s Bool
certifiedFenceEquality = do
  (ctx, notes) <- newContext
  kernel <- newKernel ctx
  stream <- mkNewLeiosValidClaims
  let (point, _, _) = fixture
      (claim, ann) = claimEvent point 0
  pruneLeiosFetchToImmTip kernel (pointSlotNo point)
  atomically $ recordNewLeiosValidClaim stream claim ann
  withLeiosFetchWorkers kernel $ withClaimFocuser stream kernel $ \attempts -> do
    awaitClaimPass attempts 1
    out <- readMVar (lfcOutstanding (lfkContext kernel))
    acquired <- S.readTVarIO notes
    pure
      ( outstandingPrunedSlot out == pointSlotNo point
          && isFocusedPoint point out
          && Map.member point (ebState out)
          && null acquired
      )

certifiedClaimOrder :: Bool -> IOSim s Bool
certifiedClaimOrder together = do
  (ctx, notes) <- newContext
  kernel <- newKernel ctx
  stream <- mkNewLeiosValidClaims
  announcements <- newAnnouncements
  let (first, _, _) = fixture
      second = first{pointEbHash = UnsafeHash (SBS.replicate 32 7)}
      ordinary = first{pointEbHash = UnsafeHash (SBS.replicate 32 8)}
      valid = validAt ordinary 2
      publish (claim, ann) = recordNewLeiosValidClaim stream claim ann
      local = lfkContext kernel
  withLeiosFetchWorkers kernel $ do
    _ <- atomically $ accept kernel announcements valid
    waitMetadata kernel valid
    initial <- readMVar (lfcOutstanding local)
    assertM "ordinary announcement was not installed" (isFocusedPoint ordinary initial)
    atomically $ do
      publish (claimEvent first 0)
      when together $ publish (claimEvent second 1)
    withClaimFocuser stream kernel $ \attempts -> do
      awaitClaimPass attempts 1
      unless together $ do
        firstOut <- readMVar (lfcOutstanding local)
        assertM "first verified claim did not override the ordinary focus" (isFocusedPoint first firstOut)
        atomically $ publish (claimEvent second 1)
        awaitClaimPass attempts 2
      out <- readMVar (lfcOutstanding local)
      acquired <- S.readTVarIO notes
      pure
        ( isFocusedPoint second out
            && not (isFocusedPoint first out)
            && Map.member first (ebState out)
            && Map.member second (ebState out)
            && null acquired
        )

certifiedWhileClosing :: IOSim s Bool
certifiedWhileClosing = do
  (base, notes) <- newContext
  submissions <- newTVarIO (0 :: Int)
  let writer = lfcWriter base
      counted =
        writer
          { writeEbBody = \p eb fills given ->
              atomically (modifyTVar submissions (+ 1)) >> writeEbBody writer p eb fills given
          }
      (point, _, _) = fixture
      (claim, ann) = claimEvent point 0
  kernel <- newKernel base{lfcWriter = counted}
  -- Exercise the real interval after normal worker closure and before the
  -- registry would stop claim delivery, without setting production flags.
  withLeiosFetchWorkers kernel (pure ())
  let local = lfkContext kernel
  closing <- atomically $ R.runtimeClosing (lfcRuntime local)
  assertM "worker scope did not close" closing
  stream <- mkNewLeiosValidClaims
  atomically $ recordNewLeiosValidClaim stream claim ann
  withClaimFocuser stream kernel $ \attempts -> do
    awaitClaimPass attempts 1
    out <- readMVar (lfcOutstanding local)
    count <- readTVarIO submissions
    acquired <- S.readTVarIO notes
    pure (isFocusedPoint point out && count == 0 && null acquired)

certifiedDequeueFailure :: IOSim s Bool
certifiedDequeueFailure = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  stream <- mkNewLeiosValidClaims
  dequeued <- newTVarIO False
  let (point, _, _) = fixture
      (claim, ann) = claimEvent point 0
      consume = do
        _ <- atomically $ do
          batch <- consumeNewLeiosValidClaims stream
          writeTVar dequeued True
          pure batch
        throwIO (userError "verified-claim failure after dequeue")
  atomically $ recordNewLeiosValidClaim stream claim ann
  result <- try @_ @SomeException $ withRegistry $ \registry -> do
    _ <-
      forkLinkedThread registry "test.certificateFocuser" $ leiosFetchCertificateFocuser consume kernel
    atomically retry
  reached <- readTVarIO dequeued
  out <- readMVar (lfcOutstanding (lfkContext kernel))
  pure
    ( reached
        && Map.null (ebState out)
        && either (isInfixOf "verified-claim failure after dequeue" . displayException) (const False) result
    )

certifiedShutdown :: Bool -> IOSim s Bool
certifiedShutdown holdLock = do
  (ctx, _) <- newContext
  kernel <- newKernel ctx
  stream <- mkNewLeiosValidClaims
  entered <- newTVarIO False
  dequeued <- newTVarIO False
  stopped <- newTVarIO False
  let (point, _, _) = fixture
      (claim, ann) = claimEvent point 0
      pending = claimEvent point{pointSlotNo = pointSlotNo point + 1} 1
      local = lfkContext kernel
      consume = do
        atomically $ writeTVar entered True
        atomically $ do
          batch <- consumeNewLeiosValidClaims stream
          writeTVar dequeued True
          pure batch
      owningLock action =
        if holdLock
          then bracket (takeMVar (lfcOutstanding local)) (putMVar (lfcOutstanding local)) (const action)
          else action
  when holdLock $ atomically $ recordNewLeiosValidClaim stream claim ann
  owningLock $ withRegistry $ \registry -> do
    _ <-
      forkLinkedThread registry "test.certificateFocuser" $
        leiosFetchCertificateFocuser consume kernel `finally` atomically (writeTVar stopped True)
    atomically $ readTVar (if holdLock then dequeued else entered) >>= check
    -- IOSim runs runnable children before advancing this timer. With the lock
    -- held, a real batch has been dequeued; without it, the empty read was entered.
    threadDelay 0.001
    readTVarIO stopped >>= assertM "consumer stopped before registry shutdown" . not
    when holdLock $ atomically $ uncurry (recordNewLeiosValidClaim stream) pending
  joined <- readTVarIO stopped
  out <- readMVar (lfcOutstanding local)
  -- Inspect remaining input only after the sole production consumer is joined.
  remaining <- atomically $ (Just <$> consumeNewLeiosValidClaims stream) `orElse` pure Nothing
  pure
    ( joined
        && Map.null (ebState out)
        && if holdLock then fmap NE.toList remaining == Just [pending] else remaining == Nothing
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
      -- The node fixture has one-second slots from system start. Freeze a
      -- coherent slot/onset pair so this test still isolates forecast anchoring.
      slotOnset = RelativeTime (fromIntegral (unSlotNo anchor))
      clock =
        (NK.getBlockchainTime (Node.nodeKernel node)){getCurrentSlot = pure (CurrentSlot anchor slotOnset)}
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

-- The node supplies the real ledger/history. A separate public clock receives
-- its own time readings, so a rewind does not disturb the fixture node's clock.
-- The existing production trace witnesses the rewind branch before we inspect
-- its published pair; forward publication is acknowledged through getCurrentSlot.
hardForkOnsetRewind :: IOSim s ([(SlotNo, RelativeTime)], (RelativeTime, RelativeTime))
hardForkOnsetRewind = Node.withNode $ \node -> withRegistry $ \registry -> do
  now <- uncheckedNewTVarM (RelativeTime 2.25)
  rewind <- uncheckedNewTVarM Nothing
  clock <-
    hardForkBlockchainTime
      HardForkBlockchainTimeArgs
        { hfbtBackoffDelay = pure (BackoffDelay 1)
        , hfbtGetLedgerState = ledgerState <$> ChainDB.getCurrentLedger (Node.nodeChainDB node)
        , hfbtLedgerConfig = configLedger (Node.nodeTopLevelConfig node)
        , hfbtRegistry = registry
        , hfbtSystemTime = SystemTime{systemTimeCurrent = readTVarIO now, systemTimeWait = pure ()}
        , hfbtTracer = mkTracer $ \event -> case event of
            TraceSystemClockMovedBack previous current -> atomically $ writeTVar rewind (Just (previous, current))
            _ -> pure ()
        , hfbtMaxClockRewind = 2
        }
  let readKnown = do
        sample <- getCurrentSlot clock
        case sample of
          CurrentSlot slot onset -> pure (slot, onset)
          CurrentSlotUnknown -> retry
  before <- atomically readKnown
  atomically $ writeTVar now (RelativeTime 1.5)
  witnessed <- atomically $ readTVar rewind >>= maybe retry pure
  -- The trace precedes publication. IOSim runs that publication to its next
  -- blocking delay before this short simulated timer can expire.
  threadDelay 0.001
  retained <- atomically readKnown
  atomically $ writeTVar now (RelativeTime 3.25)
  advanced <- atomically $ do
    pair@(slot, _) <- readKnown
    check (slot > SlotNo 2)
    pure pair
  pure ([before, retained, advanced], witnessed)

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
  void $ insertBody cache point (packEndorserBlock eb)
  withLeiosFetchWorkers kernel $ do
    forM_ [1 .. 129] $ \n -> atomically (accept kernel announcements (valid n)) >>= assertM "first announcement rejected"
    waitMetadata kernel (valid 129)
  observed <- classifyTxs cache point [(fst (head (endorserBlockReferences eb)), 1)]
  pure (observed == bucketTxArrival TxWasAbsent 1)

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
      forged = forgeLeiosEb minBound (NE.fromList (leiosTxBytes <$> txs))
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
  before <- classifyTxs (lfcTxCache ctx) point [(hashLeiosTx (head txs), 1)]
  assertM "the confirmed point was not retained" (before == bucketTxArrival TxWasHeld 1)
  threadDelay 14
  Node.addBlocks node (take 14 (iterate successorLeiosBlock (firstLeiosBlock 0)))
  atomically $ readTVar (lfkPrunedSlot kernel) >>= check . (> pointSlotNo point)
  seen <- atomically $ hasAnnouncement announcements (pointSlotNo point) key
  after <- classifyTxs (lfcTxCache ctx) point [(hashLeiosTx (head txs), 1)]
  out <- readMVar (lfcOutstanding ctx)
  following <- atomically $ Owned.submitWork (lfkForged kernel) point (forged, pure ())
  atomically $ Owned.awaitAdmission following
  pure
    ( not seen
        && after == bucketTxArrival TxEvicted 1
        && Map.notMember point (ebState out)
        && Owned.newlyOwned following
    )
