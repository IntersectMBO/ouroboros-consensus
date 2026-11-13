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
import Control.Monad (unless, when)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.IOSim (IOSim, exploreSimTrace, runSimOrThrow, traceResult)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Short as SBS
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
import qualified Ouroboros.Consensus.Leios.NotifyLog as Log
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), OCINStaleness (..))
import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..), packEndorserBlock)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork as Owned
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
import qualified Ouroboros.Consensus.NodeKernel as NK
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbWriter (..))
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import System.Random (mkStdGen)
import Test.Cardano.Crypto.Leios.Gen (mkCommitteeFromTestSeats)
import Test.Consensus.Leios.LeiosFetch.Replies (fixture, newContext)
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

-- These have already passed header validation; the fixture varies ownership
-- events, not validation. Distinct header hashes retain distinct references.
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
    $ \out ready runtime ->
      ctx
        { lfcOutstanding = out
        , lfcReady = ready
        , lfcRuntime = runtime
        , lfcTracer = nullTracer
        , lfcDetailTracer = nullTracer
        }

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
    NewAnnouncement -> onNewLeiosAnnouncement kernel valid >> pure True
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
    [ testCase "accepted metadata survives caller death before activation" $
        runSimOrThrow peerDeath @?= True
    , testCase "full metadata admission rolls back global acceptance" $
        runSimOrThrow fullAdmission @?= (False, True)
    , testCase "forge after peer acceptance joins payload work and preserves header refs" $
        runSimOrThrow forgeJoin @?= (1, 1, 2)
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
    , testCase "cached lookup only suppresses its exact body during writer admission" $
        runSimOrThrow cachedOwnership @?= (True, False, True)
    , testCase "failed startup clears forge claims admitted before workers start" $
        runSimOrThrow failedStartup @?= True
    , testCase "retirement before worker activation does not recreate state or submit payloads" $
        runSimOrThrow retiredBeforeActivation @?= (0, True)
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
      second = validAt point 1
  announcements <- newAnnouncements
  _ <- atomically (accept kernel announcements first)
  withAsync (atomically (accept kernel announcements second)) $ \blocked -> do
    threadDelay 0.01
    poll blocked >>= assertM "capacity must block" . maybe True (const False)
    cancel blocked
  let anc = announcingHeader second
  seen <- atomically $ hasAnnouncement announcements (pointSlotNo point) (announcingRbHash anc)
  admitted <- withLeiosFetchWorkers kernel $ do
    waitMetadata kernel first
    atomically (accept kernel announcements second)
  pure (seen, admitted)

forgeJoin :: IOSim s (Int, Int, Int)
forgeJoin = do
  (ctx, notes) <- newContext
  bodyCount <- S.newTVarIO 0
  txCount <- S.newTVarIO 0
  refs <- S.newTVarIO Set.empty
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let writer = lfcWriter ctx
      cache = lfcTxCache ctx
      counted =
        writer
          { writeEbBody = \p eb fills -> atomically (S.modifyTVar bodyCount (+ 1)) >> writeEbBody writer p eb fills
          , writeTxs = \p payload -> do
              atomically (S.putTMVar entered ())
              atomically (S.readTMVar release)
              atomically (S.modifyTVar txCount (+ 1))
              writeTxs writer p payload
          }
      recorded =
        cache
          { insertAnnouncement = \slot header eb -> do
              atomically (S.modifyTVar refs (Set.insert header))
              insertAnnouncement cache slot header eb
          }
      (point, _, txs) = fixture
      first = validAt point 0
      second = validAt point 1
      other = validAt point{pointSlotNo = SlotNo 6, pointEbHash = UnsafeHash (SBS.pack (replicate 32 7))} 2
      forged = forgeLeiosEb EvenProtVer (NE.fromList (leiosTxBytes <$> txs))
  kernel <- newKernel ctx{lfcWriter = counted, lfcTxCache = recorded}
  announcements <- newAnnouncements
  withLeiosFetchWorkers kernel $ do
    _ <- atomically (accept kernel announcements first)
    waitMetadata kernel first
    withAsync (onForgedLeiosEb kernel announcements first forged) $ \caller -> do
      atomically (S.readTMVar entered)
      withAsync (onForgedLeiosEb kernel announcements second forged) $ \joiner -> do
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
  recordedRefs <- S.readTVarIO refs
  pure
    ( a
    , b
    , Set.size
        ( Set.intersection
            recordedRefs
            (Set.fromList [announcingRbHash (announcingHeader first), announcingRbHash (announcingHeader second)])
        )
    )

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
      R.enqueueRequest client (Time 0) (LeiosBlockRequest (MkLeiosBlockRequest point 100 maxBound))
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
  Owned.withOwnedWork queue (\_ _ -> pure (atomically (S.readTMVar release) >> pure True)) $ do
    joined <- atomically $ Owned.submitWork queue 0 ()
    atomically $ Owned.awaitAdmission joined
    atomically $ Owned.pruneCompleted queue (const False) >> S.putTMVar release ()
    atomically $ do
      fresh <- Owned.submitWork queue 0 ()
      check (Owned.newlyOwned fresh)
      pure True

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
  _ <- insertBody cache (pointEbHash point) (packEndorserBlock eb) () (\w _ _ _ _ -> w)
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let blockedWriter =
        writer
          { writeEbBody = \p body fills -> do
              atomically $ S.putTMVar entered ()
              atomically $ S.readTMVar release
              writeEbBody writer p body fills
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
          { writeEbBody = \p eb fills -> do
              atomically $ S.modifyTVar calls (+ 1)
              writeEbBody original p eb fills
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
  withLeiosFetchWorkers kernel (pure ())
  out <- readMVar (lfcOutstanding (lfkContext kernel))
  acquired <- S.readTVarIO notes
  count <- S.readTVarIO calls
  pure (count, Map.null (ebState out) && null acquired)

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
