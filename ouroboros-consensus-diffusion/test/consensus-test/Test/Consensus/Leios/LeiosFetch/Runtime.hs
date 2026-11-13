module Test.Consensus.Leios.LeiosFetch.Runtime (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked as Checked
import Control.Monad (forM, forM_, void)
import qualified Data.ByteString as BS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace.Policy as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Util.IOLike
import System.Random (mkStdGen)
import Test.Consensus.Leios.LeiosFetch.Replies
  ( expireRuntimeHeads
  , fixture
  , newContext
  , tryEnqueueRequest
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ordinary runtime lifecycle"
    [ testGroup "effective readiness"
        [ testCase "only a live physical release sets readiness" testReleaseReadiness
        , testCase
            "pending cleanup distinguishes changed, accepted and obsolete owners"
            testPendingReadiness
        , testCase "forge ownership notifies only on set membership changes" testForgeReadiness
        , testCase
            "commit is quiet while send, decode exit and terminal receipt still wake"
            testTimingReadiness
        , testCase
            "obsolete callbacks neither alter nor wake a replacement invocation"
            testObsoleteReadiness
        ]
    , testCase
        "explicit admission distinguishes credit, eligibility, protocol and lifetime refusals"
        testAdmissionReasons
    , testCase "explicit body and shared count refusals consume no credit or ordinal"
        testAdmissionCounts
    , testCase "send classification agrees with the prior global rule across peer FIFOs" $
        forM_ [0 .. 2] $ \phase -> forM_ [False, True] (testSendClassification phase)
    , testCase
        "each fast reply retains and removes only its own late-send descriptor"
        testTargetedPruning
    , testCase
        "late network callbacks preserve replacement and unrelated local owners"
        testTargetedClaims
    , testCase
        "discard leaves other peer queues and replacement invocations intact"
        testTargetedDiscard
    , testCase
        "admission freezes connection RTT while FIFO activation follows handler completion"
        testRttAdmission
    , testCase "known forge ownership keeps physical leases without charging redundant TX stalls" $ withTxHeads $ \rt _client out point keys -> do
        atomically $ R.claimLocalForge rt point
        initial <- atomically (readTVar (R.runtimeFacts rt))
        let (expired, charges) = R.expireHeadsObserved (Time 10) (`workStillNeeded` out) initial
        charges @?= []
        map (\key -> Map.member key (L.leases (R.ledger expired))) keys @?= replicate (length keys) True
    , testCase "charge and blocked traces replay captured ownership exactly once" $ withTxHeads $ \rt client out _ keys -> do
        initial <- atomically (readTVar (R.runtimeFacts rt))
        let (failed, charges) = R.expireHeadsObserved (Time 10) (\work -> case work of L.Job{} -> True; _ -> False) initial
        case A.penaltyChanges charges of
          [A.Routine key (FetchPenaltyCharged _ at previous effective following)] -> do
            key @?= head keys
            at @?= Time 10
            previous @?= P.penaltySnapshot P.emptyPenalty
            effective @?= P.zeroScore
            following @?= P.penaltySnapshot (P.chargePenalty at P.emptyPenalty)
          _ -> assertFailure "missing or repeated charge"
        length (snd (R.expireHeadsObserved (Time 20) (const True) failed)) @?= 0
        atomically (writeTVar (R.runtimeFacts rt) failed)
        blocked <- atomically (A.blockedChanges rt)
        case blocked of
          [A.Routine _ (FetchUpstreamChanged True)] -> pure ()
          _ -> assertFailure "missing blocked transition"
        atomically (A.blockedChanges rt) >>= (\xs -> length xs @?= 0)
        atomically (R.completeWire client (head keys) (Time 11))
        resumed <- atomically (A.blockedChanges rt)
        case resumed of
          [A.Routine _ (FetchUpstreamChanged False)] -> pure ()
          _ -> assertFailure "missing resumption"
        -- The callback is still pending; a terminal already clears wire blockage.
        void (pure out)
    , testCase "zero-byte jobs are bounded by the command count despite free byte credit" $ withClient $ \rt client -> do
        let (point, _, _) = fixture
            job = Jobs.MkLeiosJob (IntSet.singleton 0) 0 (Jobs.jobCommitmentOfTxs [])
        admitted <- forM [0 .. 199] $ \jid ->
          atomically
            ( tryEnqueueRequest
                client
                (Time 0)
                (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest point (NEIntMap.singleton jid job)))
            )
        length [() | Just _ <- admitted] @?= 150
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 0 0 150
        Map.size (L.leases (R.ledger state)) @?= 150
    , testCase "local pending owner survives peer removal and blocks another writer" $ withClient $ \rt client -> do
        key <- admit client 0
        state <- atomically (readTVar (R.runtimeFacts rt))
        let point = case R.attemptWork (R.attempts state Map.! key) of L.Body p -> p; _ -> error "body fixture"
        token <- atomically (R.claimLocalBody rt point) >>= maybe (fail "local claim") pure
        atomically (R.claimUseful rt key) >>= (@?= False)
        atomically $ do
          R.closePeer rt 0
          R.closePeer rt 0
          R.acceptLocalBody rt point token
          R.clearLocalBody rt token
        atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 1)
        atomically (R.settleLocalBody rt point token)
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    , testCase "progressive admission is authoritative for both body and TX copies" $
        forM_ [True, False] testProgressiveAdmission
    , testCase "queued body and TX followers can be rescued without their own timer" $
        forM_ [True, False] $ \body -> withProgressiveWork body $ \rt clients out req -> do
          let first = clients !! 0
              blockerPoint = let (p, _, _) = fixture in p{pointSlotNo = SlotNo 0}
              blockerReq = LeiosBlockRequest (MkLeiosBlockRequest blockerPoint 1000 maxBound)
          blocker <-
            atomically (tryEnqueueRequest first (Time 0) blockerReq) >>= maybe (fail "blocker") pure
          atomically $ R.commitRequest first blocker >>= check >> R.observeSend first (Time 0)
          follower <-
            atomically (tryEnqueueRequest first (Time 0) req) >>= maybe (fail "follower") pure
          atomically (tryEnqueueRequest (clients !! 1) (Time 0) req) >>= (@?= Nothing)
          atomically (modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out))
          state <- atomically (readTVar (R.runtimeFacts rt))
          fmap (T.clockHead . R.attemptClock) (Map.lookup follower (R.attempts state)) @?= Just Nothing
          score state @?= 0
          rescue <- atomically (tryEnqueueRequest (clients !! 1) (Time 10) req)
          assertBool "blocked follower is eligible" (rescue /= Nothing)
    , testCase "terminal receipt and local work do not count as a stalled copy" $
        forM_ [True, False] testTerminalGate
    , testCase "TX head charges once, followers wait, and later cross-EB expiry charges again" $ withTxHeads $ \rt client out point keys -> do
        (a, b) <- case keys of
          [a, b] -> pure (a, b)
          _ -> assertFailure "two fixture jobs" >> fail "unreachable"
        before <- atomically (readTVar (R.runtimeFacts rt))
        let failed = expireHeads (Time 10) out before
        score failed @?= 1
        R.blocked 0 failed @?= True
        score (expireHeads (Time 20) out failed) @?= 1
        atomically $ do
          writeTVar (R.runtimeFacts rt) failed
          R.completeWire client a (Time 10)
          R.finishHandler client a (L.Job point 0) (Time 10)
        recovered <- atomically (readTVar (R.runtimeFacts rt))
        score recovered @?= 1
        let again = expireHeads (Time 20) out recovered
        assertBool
          "the previous stall ages before the next charge"
          (abs (score again - (1 + 2 ** (-10 / 600))) < 1e-12)
        -- The second immutable head belongs to the other EB.
        fmap R.attemptWork (Map.lookup b (R.attempts again))
          @?= Just (L.Job point{pointSlotNo = SlotNo 6} 0)
    , testCase "obsolete and unfocused TX heads keep physical debt without a failure penalty" $ withTxHeads $ \rt _ out point _ -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        let pendingOffsets (MkEbState onset state) = MkEbState onset $ case state of
              BodyAcquired _ -> BodyAcquired Jobs.emptyJobPool
              NoBody -> NoBody
              imminent@ReceivingFromForge{} -> imminent
            variants =
              [ ("completed", out{ebState = Map.delete point (ebState out)})
              , ("pruned", out{ebState = Map.empty})
              , ("unfocused", out{elFocus = Map.empty})
              , ("all jobs completed", out{ebState = Map.adjust pendingOffsets point (ebState out)})
              ]
        forM_ variants $ \(label, current) -> do
          let expired = expireHeads (Time 10) current before
          assertEqual label 0 (score expired)
          assertBool (label ++ " keeps FIFO debt") (R.blocked 0 expired)
    , testCase "exact charge observations identify only the qualifying immutable head" $ withTxHeads $ \rt _ out point keys -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        let (expired, charges) = expireHeadsObserved (Time 10) out before
        map R.penaltyAttempt charges @?= take 1 keys
        map R.penaltyAt charges @?= [Time 10]
        map (P.scoreValue . P.savedScore . R.penaltyBefore) charges @?= [0]
        map (P.scoreValue . P.savedScore . R.penaltyAfter) charges @?= [1]
        map R.penaltyWork charges @?= [L.Job point 0]
        snd (expireHeadsObserved (Time 20) out expired) @?= []
        -- Delayed observation does not replace the captured evidence with a
        -- later session score or a blocked follower's identity.
        score (expireHeads (Time 30) out expired) @?= 1
        map (P.scoreValue . P.savedScore . R.penaltyAfter) charges @?= [1]
        let pending = before{R.pending = Map.singleton (L.Job point 0) (R.NetworkOwner (L.AttemptKey 1 99 0), True)}
        snd (expireHeadsObserved (Time 10) out pending) @?= []
    , testCase "forgiving a live session's score does not unblock its physical head" $ withTxHeads $ \rt client out point _ -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        let expired = expireHeads (Time 10) out before
            session = R.sessions expired Map.! 0
        assertBool "evidence lasts until the boundary" (R.closureScore (Time 609) session > P.zeroScore)
        R.closureScore (Time 610) session @?= P.zeroScore
        R.blocked 0 expired @?= True
        atomically (writeTVar (R.runtimeFacts rt) expired)
        let fresh = LeiosBlockRequest (MkLeiosBlockRequest point{pointSlotNo = SlotNo 99} 1000 maxBound)
        atomically (tryEnqueueRequest client (Time 610) fresh) >>= (@?= Nothing)
    , testCase "another complete pending copy suppresses a failure charge" $ withTxHeads $ \rt _ out point _ -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        let pending = before{R.pending = Map.singleton (L.Job point 0) (R.NetworkOwner (L.AttemptKey 1 99 0), True)}
            expired = expireHeads (Time 10) out pending
        score expired @?= 0
        R.blocked 0 expired @?= True
    , testCase "TX input publication and original receipt protect scoring from local delay" $
        withTxHeads $ \rt client out point keys -> do
          atomically (R.publishInput client True)
          protected <- atomically (readTVar (R.runtimeFacts rt))
          score (expireHeads (Time 10) out protected) @?= 0
          atomically $ do
            R.publishInput client False
            R.completeWire client (head keys) (Time 0.1)
          received <- atomically (readTVar (R.runtimeFacts rt))
          score (expireHeads (Time 10) out received) @?= 0
          atomically (R.finishHandler client (head keys) (L.Job point 0) (Time 20))
          ready <- atomically (readTVar (R.runtimeFacts rt))
          score (expireHeads (Time 20) out ready) @?= 0
    , testCase "removal/reconnect clears evidence and old callbacks cannot penalize replacement" $ withTxHeads $ \rt old out _ keys -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        atomically $ do
          writeTVar (R.runtimeFacts rt) (expireHeads (Time 10) out before)
          R.closePeer rt 0
          void (openClient rt 0 False (pure Nothing))
          R.completeWire old (head keys) (Time 30)
          R.observeSend old (Time 30)
          R.closeClient old
        fresh <- atomically (readTVar (R.runtimeFacts rt))
        score fresh @?= 0
        Map.size (R.sessions fresh) @?= 1
    , testCase "body expiry never earns a closure-service penalty" $ withClient $ \rt client -> do
        key <- admit client 0
        atomically $ R.commitRequest client key >>= check >> R.observeSend client (Time 0)
        before <- atomically (readTVar (R.runtimeFacts rt))
        let expired = expireHeads (Time 10) noWork before
        score expired @?= 0
        R.blocked 0 expired @?= True
    , testCase "fast reply reconciles send return without retaining a lease" $ withClient $ \rt client -> do
        key <- admit client 0
        atomically $ do
          R.commitRequest client key >>= check
          R.installReceiver client key
          R.completeWire client key (Time 0.05)
          R.claimUseful rt key >>= check
          R.acceptReply rt key (bodyWork 0)
          R.finishHandler client key (bodyWork 0) (Time 0.07)
        before <- atomically (readTVar (R.runtimeFacts rt))
        Map.size (R.attempts before) @?= 1
        Map.size (L.leases (R.ledger before)) @?= 0
        atomically (R.observeSend client (Time 0.1))
        after <- atomically (readTVar (R.runtimeFacts rt))
        Map.size (R.attempts after) @?= 0
        Map.size (R.pending after) @?= 1
        atomically (R.settleReceipt rt key (bodyWork 0))
        atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 0)
    , testCase "blocked local handler gives successor no running timer" $ withClient $ \rt client -> do
        a <- admit client 0
        b <- admit client 1
        atomically $ do
          R.commitRequest client a >>= check
          R.observeSend client (Time 0)
          R.commitRequest client b >>= check
          R.observeSend client (Time 0.01)
          R.completeWire client a (Time 0.2)
        busy <- atomically (readTVar (R.runtimeFacts rt))
        R.nextDeadline busy @?= Nothing
        assertBool "follower not expired" (not (R.blocked 0 (expireHeads (Time 10) noWork busy)))
        atomically (R.finishHandler client a (bodyWork 0) (Time 0.7))
        ready <- atomically (readTVar (R.runtimeFacts rt))
        R.nextDeadline ready @?= Just (Time 1.2)
    , testCase "bounded input publication protects expiry without renewing it" $ withClient $ \rt client -> do
        key <- admit client 0
        atomically $ do
          R.commitRequest client key >>= check
          R.observeSend client (Time 0)
          R.publishInput client True
        protected <- atomically (readTVar (R.runtimeFacts rt))
        R.nextDeadline protected @?= Nothing
        assertBool "publication protected" (not (R.blocked 0 (expireHeads (Time 2) noWork protected)))
        atomically (R.publishInput client False)
        resumed <- atomically (readTVar (R.runtimeFacts rt))
        assertBool "original deadline expires" (R.blocked 0 (expireHeads (Time 2) noWork resumed))
    , testCase "old callbacks cannot affect replacement invocation" $ withClient $ \rt old -> do
        key <- admit old 0
        atomically (R.closePeer rt 0)
        fresh <- atomically (openClient rt 0 False (pure Nothing))
        freshKey <- admit fresh 0
        atomically $ do
          R.finishHandler old key (bodyWork 0) (Time 100)
          R.completeWire old key (Time 100)
          R.observeSend old (Time 100)
          R.closeClient old
        st <- atomically (readTVar (R.runtimeFacts rt))
        Map.member freshKey (L.leases (R.ledger st)) @?= True
        Map.size (R.attempts st) @?= 1
    , testCase "sibling removal cannot erase a submitting handler's claim" $ withClient $ \rt client -> do
        key <- admit client 0
        atomically $ do
          R.commitRequest client key >>= check
          R.installReceiver client key
          R.completeWire client key (Time 0.1)
          R.claimUseful rt key >>= check
          R.closePeer rt 0
          R.acceptReply rt key (bodyWork 0)
          R.finishHandler client key (bodyWork 0) (Time 0.2)
        atomically (Map.elems . R.pending <$> readTVar (R.runtimeFacts rt))
          >>= (@?= [(R.NetworkOwner key, True)])
        atomically (R.settleReceipt rt key (bodyWork 0))
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    , testCase "submitting claims survive repeated removal until their owner cleans up" $ withClient $ \rt client -> do
        a <- admit client 0
        b <- admit client 1
        atomically $ do
          R.claimUseful rt a >>= check
          R.claimUseful rt b >>= check
          R.acceptReply rt a (bodyWork 0)
          R.closePeer rt 0
          R.closePeer rt 0
        atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 2)
        atomically (R.clearUnsubmitted rt b (bodyWork 1))
        st <- atomically (readTVar (R.runtimeFacts rt))
        Map.elems (R.pending st) @?= [(R.NetworkOwner a, True)]
        Map.null (L.leases (R.ledger st)) @?= True
        atomically (R.settleReceipt rt a (bodyWork 0))
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    ]

takeRuntimeReady :: R.Runtime IO Int -> IO Bool
takeRuntimeReady rt = atomically $
  orElse (W.awaitReadiness (R.runtimeReady rt) >> pure True) (pure False)

testReleaseReadiness :: Assertion
testReleaseReadiness = withClient $ \rt client -> do
  key <- admit client 0
  _ <- takeRuntimeReady rt
  atomically (R.releaseAttempt rt (L.AttemptKey 99 99 99))
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.releaseAttempt rt key)
  -- Repeated cleanup must neither create a fresh signal nor clear the real one.
  atomically $ forM_ [1 .. 20 :: Int] $ \_ -> R.releaseAttempt rt key
  takeRuntimeReady rt >>= (@?= True)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.releaseAttempt rt key)
  takeRuntimeReady rt >>= (@?= False)
  atomically (L.usage 0 . R.ledger <$> readTVar (R.runtimeFacts rt)) >>= (@?= L.Usage 0 0 0)

testPendingReadiness :: Assertion
testPendingReadiness = withClient $ \rt client -> do
  key <- admit client 0
  let work = bodyWork 0
      point = L.workPoint work
  _ <- takeRuntimeReady rt
  atomically (R.clearUnsubmitted rt key work >> R.settleReceipt rt key work)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.claimUseful rt key) >>= (@?= True)
  atomically (R.clearUnsubmitted rt key work)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.clearUnsubmitted rt key work)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.claimUseful rt key) >>= (@?= True)
  atomically (R.acceptReply rt key work)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.clearUnsubmitted rt key work >> R.acceptReply rt key work)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.settleReceipt rt key work)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.settleReceipt rt key work)
  takeRuntimeReady rt >>= (@?= False)
  token <- atomically (R.claimLocalBody rt point) >>= maybe (fail "local claim") pure
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.acceptLocalBody rt point token >> R.acceptLocalBody rt point token)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.settleLocalBody rt point token)
  takeRuntimeReady rt >>= (@?= True)
  replacement <- atomically (R.claimLocalBody rt point) >>= maybe (fail "replacement") pure
  _ <- takeRuntimeReady rt
  atomically $ do
    R.acceptLocalBody rt point token
    R.settleLocalBody rt point token
    R.settleReceipt rt key work
    R.clearUnsubmitted rt key work
  takeRuntimeReady rt >>= (@?= False)
  atomically (Map.lookup work . R.pending <$> readTVar (R.runtimeFacts rt))
    >>= (@?= Just (R.LocalOwner replacement, False))
  atomically (R.settleLocalBody rt point replacement)
  takeRuntimeReady rt >>= (@?= True)

testForgeReadiness :: Assertion
testForgeReadiness = withClient $ \rt _ -> do
  let point = L.workPoint (bodyWork 0)
  _ <- takeRuntimeReady rt
  atomically (R.releaseLocalForge rt point)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.claimLocalForge rt point)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.claimLocalForge rt point)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.releaseLocalForge rt point)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.releaseLocalForge rt point)
  takeRuntimeReady rt >>= (@?= False)

testTimingReadiness :: Assertion
testTimingReadiness = withClient $ \rt client -> do
  key <- admit client 0
  _ <- takeRuntimeReady rt
  atomically (R.commitRequest client key) >>= (@?= True)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.nextDeadline <$> readTVar (R.runtimeFacts rt)) >>= (@?= Nothing)
  atomically (R.observeSend client (Time 0))
  takeRuntimeReady rt >>= (@?= True)
  original <- atomically (R.nextDeadline <$> readTVar (R.runtimeFacts rt))
  assertBool "send return must activate the original deadline" (original /= Nothing)
  atomically (R.publishInput client True >> R.publishInput client True)
  takeRuntimeReady rt >>= (@?= False)
  atomically (R.nextDeadline <$> readTVar (R.runtimeFacts rt)) >>= (@?= Nothing)
  atomically (R.publishInput client False)
  takeRuntimeReady rt >>= (@?= True)
  atomically (R.nextDeadline <$> readTVar (R.runtimeFacts rt)) >>= (@?= original)
  atomically (R.publishInput client False)
  takeRuntimeReady rt >>= (@?= False)
  atomically $ modifyTVar (R.runtimeFacts rt) (expireRuntimeHeads (Time 100) (const True))
  atomically (R.blocked 0 <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
  atomically (R.completeWire client key (Time 0.1))
  takeRuntimeReady rt >>= (@?= True)
  current <- atomically (readTVar (R.runtimeFacts rt))
  R.blocked 0 current @?= False
  T.clockReceipt (R.attemptClock (R.attempts current Map.! key)) @?= Just (Time 0.1)

testObsoleteReadiness :: Assertion
testObsoleteReadiness = withClient $ \rt old -> do
  key <- admit old 0
  atomically (R.closeClient old)
  fresh <- atomically (openClient rt 0 False (pure Nothing))
  next <- admit fresh 0
  atomically (R.claimUseful rt next) >>= (@?= True)
  _ <- takeRuntimeReady rt
  atomically $ do
    R.releaseAttempt rt key
    R.acceptReply rt key (bodyWork 0)
    R.clearUnsubmitted rt key (bodyWork 0)
    R.settleReceipt rt key (bodyWork 0)
    R.observeSend old (Time 100)
    R.completeWire old key (Time 100)
    R.publishInput old False
    R.finishHandler old key (bodyWork 0) (Time 100)
    R.closeClient old
  takeRuntimeReady rt >>= (@?= False)
  current <- atomically (readTVar (R.runtimeFacts rt))
  Map.member next (L.leases (R.ledger current)) @?= True
  Map.lookup (bodyWork 0) (R.pending current) @?= Just (R.NetworkOwner next, False)

testAdmissionReasons :: Assertion
testAdmissionReasons = withClient $ \rt client -> do
  let job jid bytes =
        LeiosBlockTxsRequest $
          MkLeiosBlockTxsRequest (L.workPoint (bodyWork 0)) $
            NEIntMap.singleton jid $
              Jobs.MkLeiosJob (IntSet.singleton jid) bytes (Jobs.jobCommitmentOfTxs [])
      request = job 0 262144
      need = either (fail . show) pure
      reject req expected = do
        before <- atomically (readTVar (R.runtimeFacts rt))
        commands <- atomically $ case Map.lookup 0 (R.sessions before) of
          Just session -> readTVar (R.queued session)
          Nothing -> error "missing fixture session"
        result <- atomically (R.enqueueRequest client (Time 0) req)
        result @?= Left expected
        after <- atomically (readTVar (R.runtimeFacts rt))
        R.ledger after @?= R.ledger before
        Map.keys (R.attempts after) @?= Map.keys (R.attempts before)
        fmap R.nextOrdinal (Map.lookup 0 (R.sessions after))
          @?= fmap R.nextOrdinal (Map.lookup 0 (R.sessions before))
        case Map.lookup 0 (R.sessions after) of
          Just session -> atomically (readTVar (R.queued session)) >>= (@?= commands)
          Nothing -> assertFailure "live fixture session disappeared"
  primary <- atomically (R.enqueueRequest client (Time 0) request) >>= need
  reject request R.WorkIneligible
  reject (job 1 1) (R.ReservationRejected L.TransactionCreditLimit)
  reject (job 1 262145) (R.ReservationRejected L.RequestExceedsCredit)
  reject (job 1 500001) R.ProtocolInvalid
  _ <- atomically (R.enqueueRequest client (Time 0) (job 1 0)) >>= need
  _ <- atomically $
    R.enqueueRequest client (Time 0) $
      LeiosBlockRequest (MkLeiosBlockRequest (L.workPoint (bodyWork 0)) 1000 maxBound)
  state <- atomically (readTVar (R.runtimeFacts rt))
  L.usage 0 (R.ledger state) @?= L.Usage 262144 1 3
  atomically $ do
    R.commitRequest client primary >>= check
    R.observeSend client (Time 0)
    modifyTVar (R.runtimeFacts rt) (expireRuntimeHeads (Time 100) (const True))
  reject (job 2 0) R.SourceBlocked
  atomically (R.closeClient client)
  atomically (R.enqueueRequest client (Time 101) (job 2 0)) >>= (@?= Left R.SourceClosed)
  ready <- atomically W.newReadiness
  closed <- uncheckedNewTVarM False
  closing <- R.newRuntime defaultLeiosFetchStaticEnv ready (readTVar closed)
  queue <- Checked.newTVarIO Seq.empty
  original <- atomically (R.openClient closing (0 :: Int) False queue (pure Nothing))
  atomically (writeTVar closed True)
  atomically (R.enqueueRequest original (Time 0) (job 0 0)) >>= (@?= Left R.SourceClosed)

testAdmissionCounts :: Assertion
testAdmissionCounts = withClient $ \rt client -> do
  let body slot =
        LeiosBlockRequest (MkLeiosBlockRequest (L.workPoint (bodyWork slot)) 1000 maxBound)
      need = either (fail . show) pure
  keys <- forM [0 .. 3] $ \slot ->
    atomically (R.enqueueRequest client (Time 0) (body slot)) >>= need
  atomically (R.enqueueRequest client (Time 0) (body 4))
    >>= (@?= Left (R.ReservationRejected L.BodyCountLimit))
  forM_ [0 .. 145] $ \jid -> do
    let req = LeiosBlockTxsRequest $
          MkLeiosBlockTxsRequest (L.workPoint (bodyWork 0)) $
            NEIntMap.singleton jid $
              Jobs.MkLeiosJob (IntSet.singleton jid) 0 (Jobs.jobCommitmentOfTxs [])
    _ <- atomically (R.enqueueRequest client (Time 0) req) >>= need
    pure ()
  before <- atomically (readTVar (R.runtimeFacts rt))
  L.usage 0 (R.ledger before) @?= L.Usage 0 4 150
  atomically (R.enqueueRequest client (Time 0) (body 4))
    >>= (@?= Left (R.ReservationRejected L.RequestCountLimit))
  atomically (R.discardAttempt rt (head keys))
  replacement <- atomically (R.enqueueRequest client (Time 0) (body 4)) >>= need
  case replacement of L.AttemptKey _ _ ordinal -> ordinal @?= 150
  after <- atomically (readTVar (R.runtimeFacts rt))
  L.usage 0 (R.ledger after) @?= L.Usage 0 4 150

-- Drive real lifecycle transitions, but compute the reference classification by
-- the old global rule rather than by inspecting the new FIFO-based predicate.
-- The other peer is always active, and queued same-peer work must not count.
testSendClassification :: Int -> Bool -> Assertion
testSendClassification phase pipeline = withClient $ \rt client -> do
  other <- atomically (openClient rt 1 False (pure Nothing))
  a <- admit client 0
  b <- admit client 1
  follower <- admit client 2
  c <- admit other 3
  atomically $ do
    R.commitRequest client a >>= check
    R.observeSend client (Time 0)
    R.commitRequest client b >>= check
    R.commitRequest other c >>= check
    R.observeSend other (Time 0)
    if pipeline then R.commitRequest client follower >>= check else pure ()
    if phase >= 1 then R.completeWire client a (Time 0.1) else pure ()
    if phase >= 2 then R.finishHandler client a (bodyWork 0) (Time 0.2) else pure ()
  before <- atomically (readTVar (R.runtimeFacts rt))
  let L.AttemptKey peer generation _ = b
      reference =
        or
          [ key /= b && p == peer && n == generation
              && R.committed attempt && not (R.settled attempt)
              && T.clockReceipt (R.attemptClock attempt) == Nothing
          | (key@(L.AttemptKey p n _), attempt) <- Map.toList (R.attempts before)
          ]
      observedAt = Time 0.3
  assertEqual "fixture covers cold and continuous cases" (phase == 0 || pipeline) reference
  atomically (R.observeSend client observedAt)
  after <- atomically (readTVar (R.runtimeFacts rt))
  let clock = R.attemptClock (R.attempts after Map.! b)
  T.clockSent clock @?= Just (observedAt, reference)

-- Handler completion and send return can arrive in either order, independently
-- for different peers. Receipt settlement can precede either one.
testTargetedPruning :: Assertion
testTargetedPruning = withClient $ \rt client -> do
  other <- atomically (openClient rt 1 False (pure Nothing))
  a <- admit client 0
  b <- admit other 1
  atomically $ forM_ [(client, a, bodyWork 0), (other, b, bodyWork 1)] $ \(owner, key, work) -> do
    R.commitRequest owner key >>= check
    R.installReceiver owner key
    R.completeWire owner key (Time 0.1)
    R.claimUseful rt key >>= check
    R.acceptReply rt key work
    R.settleReceipt rt key work
    R.finishHandler owner key work (Time 0.2)
  let keys = Map.keys . R.attempts <$> readTVar (R.runtimeFacts rt)
  atomically keys >>= (@?= [a, b])
  atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
  atomically (Map.null . L.leases . R.ledger <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
  atomically (R.observeSend client (Time 0.3))
  atomically keys >>= (@?= [b])
  atomically (R.observeSend other (Time 0.4))
  atomically keys >>= (@?= [])
  -- In the ordinary order the handler itself removes its descriptor.
  c <- admit client 2
  atomically $ do
    R.commitRequest client c >>= check
    R.observeSend client (Time 0.5)
    R.completeWire client c (Time 0.6)
    R.finishHandler client c (bodyWork 2) (Time 0.7)
  atomically keys >>= (@?= [])

testTargetedClaims :: Assertion
testTargetedClaims = withClient $ \rt old -> do
  a <- admit old 0
  atomically (R.claimUseful rt a) >>= (@?= True)
  atomically $ do
    R.closeClient old
    R.acceptReply rt a (bodyWork 0)
    R.clearUnsubmitted rt a (bodyWork 0)
  atomically (Map.lookup (bodyWork 0) . R.pending <$> readTVar (R.runtimeFacts rt))
    >>= (@?= Just (R.NetworkOwner a, True))
  atomically (R.settleReceipt rt a (bodyWork 0))
  fresh <- atomically (openClient rt 0 False (pure Nothing))
  b <- admit fresh 0
  atomically (R.claimUseful rt b) >>= (@?= True)
  let point = L.workPoint (bodyWork 1)
  local <- atomically (R.claimLocalBody rt point) >>= maybe (fail "local claim") pure
  atomically $ do
    R.acceptReply rt a (bodyWork 0)
    R.clearUnsubmitted rt a (bodyWork 0)
    R.settleReceipt rt a (bodyWork 0)
    R.finishHandler old a (bodyWork 0) (Time 1)
    R.acceptLocalBody rt point local
  st <- atomically (readTVar (R.runtimeFacts rt))
  R.pending st @?= Map.fromList
    [(bodyWork 0, (R.NetworkOwner b, False)), (bodyWork 1, (R.LocalOwner local, True))]
  Map.member b (L.leases (R.ledger st)) @?= True
  atomically $ do
    R.settleLocalBody rt point local
    R.clearUnsubmitted rt b (bodyWork 0)
  atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)

testTargetedDiscard :: Assertion
testTargetedDiscard = withClient $ \rt old -> do
  other <- atomically (openClient rt 1 False (pure Nothing))
  a <- admit old 0
  b <- admit other 1
  keep <- admit old 2
  atomically (R.discardAttempt rt a)
  atomically (R.takeRequest old) >>= (\request -> fmap fst request @?= Just keep)
  atomically (R.takeRequest other) >>= (\request -> fmap fst request @?= Just b)
  atomically (R.closeClient old)
  fresh <- atomically (openClient rt 0 False (pure Nothing))
  c <- admit fresh 0
  atomically (R.discardAttempt rt a)
  atomically (R.takeRequest fresh) >>= (\request -> fmap fst request @?= Just c)
  st <- atomically (readTVar (R.runtimeFacts rt))
  Map.member c (L.leases (R.ledger st)) @?= True
  Map.member b (L.leases (R.ledger st)) @?= True

-- Admission captures a budget, not an activation timestamp. Later samples and
-- replacement sessions cannot change it, while local handling gates the FIFO.
testRttAdmission :: Assertion
testRttAdmission = withClient $ \rt original -> do
  sample <- uncheckedNewTVarM (Just (T.RttObservation 0.1 (Time 0)))
  client <- atomically $ R.closeClient original >> openClient rt 0 False (readTVar sample)
  first <- admit client 1
  atomically $ R.commitRequest client first >>= check >> R.observeSend client (Time 0)
  atomically $ writeTVar sample (Just (T.RttObservation 0.2 (Time 1)))
  let point = MkLeiosPoint 2 (endorserBlockHash (mkEndorserBlock []))
      req = LeiosBlockRequest (MkLeiosBlockRequest point 1000 maxBound)
  second <-
    atomically (tryEnqueueRequest client (Time 1) req) >>= maybe (fail "follower admission") pure
  atomically $ R.commitRequest client second >>= check >> R.observeSend client (Time 1)
  before <- atomically $ readTVar (R.runtimeFacts rt)
  let clock key state = R.attemptClock (R.attempts state Map.! key)
  T.snapshotRtt (T.clockSnapshot (clock first before)) @?= 0.15
  T.snapshotRtt (T.clockSnapshot (clock second before)) @?= 0.3
  T.snapshotRttSource (T.clockSnapshot (clock first before)) @?= T.KeepAliveSample
  T.clockHead (clock second before) @?= Nothing
  atomically $ R.completeWire client first (Time 2)
  handling <- atomically $ readTVar (R.runtimeFacts rt)
  T.clockHead (clock second handling) @?= Nothing
  atomically $ R.finishHandler client first (bodyWork 1) (Time 3)
  after <- atomically $ readTVar (R.runtimeFacts rt)
  T.clockHead (clock second after) @?= Just (Just (Time 2), Just (Time 3))
  T.clockSnapshot (clock second after) @?= T.clockSnapshot (clock second before)
  replacement <- atomically $ R.closeClient client >> openClient rt 0 False (pure Nothing)
  atomically (tryEnqueueRequest client (Time 4) req) >>= (@?= Nothing)
  fresh <-
    atomically (tryEnqueueRequest replacement (Time 4) req)
      >>= maybe (fail "new invocation admission") pure
  current <- atomically $ readTVar (R.runtimeFacts rt)
  T.snapshotRtt (T.clockSnapshot (clock fresh current)) @?= 0.5
  T.snapshotRttSource (T.clockSnapshot (clock fresh current)) @?= T.MissingRtt
  assertBool "replacement has a distinct physical identity" (fresh /= second)

withClient :: (R.Runtime IO Int -> R.Client IO Int -> IO ()) -> IO ()
withClient action = do
  ready <- atomically W.newReadiness
  rt <- R.newRuntime (defaultLeiosFetchStaticEnv) ready (pure False)
  client <- atomically (openClient rt 0 False (pure Nothing))
  action rt client

bodyWork :: Word -> L.Work LeiosPoint
bodyWork slot =
  L.Body (MkLeiosPoint (SlotNo (fromIntegral slot)) (endorserBlockHash (mkEndorserBlock [])))

admit :: R.Client IO Int -> Word -> IO (L.AttemptKey Int)
admit client slot = do
  let point = MkLeiosPoint (SlotNo (fromIntegral slot)) (endorserBlockHash (mkEndorserBlock []))
      req = LeiosBlockRequest (MkLeiosBlockRequest point 1000 maxBound)
  atomically (tryEnqueueRequest client (Time 0) req)
    >>= maybe (assertFailure "admission rejected" >> fail "unreachable") pure

-- A pair of immutable jobs on different EBs sharing one physical FIFO.
withTxHeads ::
  ( R.Runtime IO Int ->
    R.Client IO Int ->
    LeiosOutstanding Int ->
    LeiosPoint ->
    [L.AttemptKey Int] ->
    IO ()
  ) ->
  IO ()
withTxHeads action = withClient $ \rt client -> do
  (context, _) <- newContext
  original <- readMVar (lfcOutstanding context)
  let (point, _, txs) = fixture
      other = point{pointSlotNo = SlotNo 6}
      refs =
        IntMap.fromList
          (zip [0 ..] [(hashLeiosTx tx, fromIntegral (BS.length (leiosTxBytes tx))) | tx <- txs])
      pool = Jobs.mkJobPool 65536 128 refs
      out =
        original
          { ebState = Map.fromList [(p, MkEbState SNothing (BodyAcquired pool)) | p <- [point, other]]
          , elFocus =
              Map.union
                ( Map.fromList
                    [ (ElectionId (pointSlotNo other) issuer, pointEbHash other)
                    | (ElectionId _ issuer, _) <- Map.toList (elFocus original)
                    ]
                )
                (elFocus original)
          }
  job <-
    maybe
      (assertFailure "missing fixture job" >> fail "unreachable")
      pure
      (Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool)
  keys <- forM [point, other] $ \p -> do
    key <-
      atomically
        ( tryEnqueueRequest
            client
            (Time 0)
            (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest p (NEIntMap.singleton 0 job)))
        )
        >>= maybe (assertFailure "job admission" >> fail "unreachable") pure
    atomically $ R.commitRequest client key >>= check >> R.observeSend client (Time 0)
    pure key
  action rt client out point keys

score :: R.RuntimeState IO Int -> Double
score = maybe 0 (P.scoreValue . R.closureScore (Time 0)) . Map.lookup 0 . R.sessions

noWork :: LeiosOutstanding Int
noWork = emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)

-- Both kinds use the same reservation STM path; no caller can opt into hedging.
withProgressiveWork ::
  Bool ->
  (R.Runtime IO Int -> [R.Client IO Int] -> LeiosOutstanding Int -> LeiosFetchRequest -> IO ()) ->
  IO ()
withProgressiveWork body action = withClient $ \rt first -> do
  (context, _) <- newContext
  original <- readMVar (lfcOutstanding context)
  let (point, eb, _) = fixture
      refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
      pool = Jobs.mkJobPool 65536 128 refs
      out = if body then original else acquireEbBody point pool original
  job <-
    maybe
      (assertFailure "missing fixture job" >> fail "unreachable")
      pure
      (Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool)
  let req =
        if body
          then LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
          else LeiosBlockTxsRequest (MkLeiosBlockTxsRequest point (NEIntMap.singleton 0 job))
  others <- forM [1 .. 3] $ \peer -> atomically (openClient rt peer False (pure Nothing))
  action rt (first : others) out req

testProgressiveAdmission :: Bool -> Assertion
testProgressiveAdmission body = withProgressiveWork body $ \rt clients out req -> do
  let admitCopy n = atomically (tryEnqueueRequest (clients !! n) (Time (fromIntegral n * 10)) req)
      requireCopy n = admitCopy n >>= maybe (assertFailure "expected eligible copy" >> fail "unreachable") pure
      expireCopy n key = atomically $ do
        R.commitRequest (clients !! n) key >>= check
        R.observeSend (clients !! n) (Time (fromIntegral n * 10))
        modifyTVar (R.runtimeFacts rt) (expireHeads (Time (fromIntegral (n + 1) * 10)) out)
  a <- requireCopy 0
  forM_ [1 .. 3] $ \n -> admitCopy n >>= (@?= Nothing)
  expireCopy 0 a
  b <- requireCopy 1
  forM_ [2 .. 3] $ \n -> admitCopy n >>= (@?= Nothing)
  expireCopy 1 b
  c <- requireCopy 2
  admitCopy 3 >>= (@?= Nothing)
  expireCopy 2 c
  d <- requireCopy 3
  state <- atomically (readTVar (R.runtimeFacts rt))
  length (R.workOwners (R.workOf req) state) @?= 4
  -- A fresh queued copy suppresses more admissions; retiring that registration
  -- restores eligibility without forgetting any of the original three debts.
  R.workEligible (R.workOf req) state @?= False
  atomically (R.closePeer rt 3)
  after <- atomically (readTVar (R.runtimeFacts rt))
  R.workEligible (R.workOf req) after @?= True
  Map.member d (L.leases (R.ledger after)) @?= False
  atomically (R.closePeer rt 0 >> R.closePeer rt 1 >> R.closePeer rt 2)
  remaining <- atomically (readTVar (R.runtimeFacts rt))
  R.workEligible (R.workOf req) remaining @?= True
  fresh <- atomically (openClient rt 0 False (pure Nothing))
  key <- atomically (tryEnqueueRequest fresh (Time 50) req)
  assertBool "all prior copies retired permits a new primary" (key /= Nothing)

testTerminalGate :: Bool -> Assertion
testTerminalGate body = withProgressiveWork body $ \rt clients out req -> do
  let first = clients !! 0
      second = clients !! 1
      third = clients !! 2
  a <- atomically (tryEnqueueRequest first (Time 0) req) >>= maybe (fail "primary") pure
  atomically $ do
    R.commitRequest first a >>= check
    R.observeSend first (Time 0)
    modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out)
  b <- atomically (tryEnqueueRequest second (Time 10) req) >>= maybe (fail "hedge") pure
  atomically $ do
    R.commitRequest second b >>= check
    R.observeSend second (Time 10)
    modifyTVar (R.runtimeFacts rt) (expireHeads (Time 20) out)
    R.completeWire first a (Time 20)
  atomically (tryEnqueueRequest third (Time 20) req) >>= (@?= Nothing)
  atomically $ do
    R.claimUseful rt a >>= check
    R.acceptReply rt a (R.workOf req)
    R.finishHandler first a (R.workOf req) (Time 20)
    R.closeClient first
  atomically (tryEnqueueRequest third (Time 20) req) >>= (@?= Nothing)
  -- An unconfirmed accepted receipt can leave work missing; settling pending
  -- ownership permits another attempt while the stalled second copy remains.
  atomically (R.settleReceipt rt a (R.workOf req))
  replacement <- atomically (tryEnqueueRequest third (Time 20) req)
  assertBool "unconfirmed receipt permits further recovery" (replacement /= Nothing)

-- Each fixture shares the same queue with its runtime, just like a kernel peer.
openClient ::
  R.Runtime IO Int -> Int -> Bool -> STM IO (Maybe T.RttObservation) -> STM IO (R.Client IO Int)
openClient rt peer big sample = do
  queue <- Checked.newTVar Seq.empty
  R.openClient rt peer big queue sample

expireHeads :: Time -> LeiosOutstanding Int -> R.RuntimeState IO Int -> R.RuntimeState IO Int
expireHeads now out = expireRuntimeHeads now (`workStillNeeded` out)

expireHeadsObserved ::
  Time ->
  LeiosOutstanding Int ->
  R.RuntimeState IO Int ->
  (R.RuntimeState IO Int, [R.PenaltyCharge Int])
expireHeadsObserved now out = R.expireHeadsObserved now (`workStillNeeded` out)
