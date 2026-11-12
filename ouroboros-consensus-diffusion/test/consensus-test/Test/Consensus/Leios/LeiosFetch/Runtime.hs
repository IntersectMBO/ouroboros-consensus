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
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Util.IOLike
import System.Random (mkStdGen)
import Test.Consensus.Leios.LeiosFetch.Replies (fixture, newContext)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ordinary runtime lifecycle"
    [ testCase "zero-byte jobs are bounded by the command count despite free byte credit" $ withClient $ \rt client -> do
        let (point, _, _) = fixture
            job = Jobs.MkLeiosJob (IntSet.singleton 0) 0 (Jobs.jobCommitmentOfTxHashes [])
        admitted <- forM [0 .. 199] $ \jid ->
          atomically
            ( R.enqueueRequest
                client
                (Time 0)
                (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest point (NEIntMap.singleton jid job)))
            )
        length [() | Just _ <- admitted] @?= 150
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 0 0
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
          R.acceptLocalBody rt token
          R.clearLocalBody rt token
        atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 1)
        atomically (R.settleLocalBody rt token)
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    , testCase "progressive admission is authoritative for both body and TX copies" $
        forM_ [True, False] testProgressiveAdmission
    , testCase "queued body and TX followers can be rescued without their own timer" $
        forM_ [True, False] $ \body -> withProgressiveWork body $ \rt clients out req -> do
          let first = clients !! 0
              blockerPoint = let (p, _, _) = fixture in p{pointSlotNo = SlotNo 0}
              blockerReq = LeiosBlockRequest (MkLeiosBlockRequest blockerPoint 1000 maxBound)
          blocker <- atomically (R.enqueueRequest first (Time 0) blockerReq) >>= maybe (fail "blocker") pure
          atomically $ R.commitRequest first blocker >>= check >> R.observeSend first (Time 0)
          follower <- atomically (R.enqueueRequest first (Time 0) req) >>= maybe (fail "follower") pure
          atomically (R.enqueueRequest (clients !! 1) (Time 0) req) >>= (@?= Nothing)
          atomically (modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out))
          state <- atomically (readTVar (R.runtimeFacts rt))
          fmap (T.clockHead . R.attemptClock) (Map.lookup follower (R.attempts state)) @?= Just Nothing
          score state @?= 0
          rescue <- atomically (R.enqueueRequest (clients !! 1) (Time 10) req)
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
          R.finishHandler client a (Time 10)
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
        atomically (R.enqueueRequest client (Time 610) fresh) >>= (@?= Nothing)
    , testCase "another complete pending copy suppresses a failure charge" $ withTxHeads $ \rt _ out point _ -> do
        before <- atomically (readTVar (R.runtimeFacts rt))
        let pending = before{R.pending = Map.singleton (L.Job point 0) (R.NetworkOwner (L.AttemptKey 1 99 0), True)}
            expired = expireHeads (Time 10) out pending
        score expired @?= 0
        R.blocked 0 expired @?= True
    , testCase "TX input publication and original receipt protect scoring from local delay" $ withTxHeads $ \rt client out _ keys -> do
        atomically (R.publishInput client True)
        protected <- atomically (readTVar (R.runtimeFacts rt))
        score (expireHeads (Time 10) out protected) @?= 0
        atomically $ do
          R.publishInput client False
          R.completeWire client (head keys) (Time 0.1)
        received <- atomically (readTVar (R.runtimeFacts rt))
        score (expireHeads (Time 10) out received) @?= 0
        atomically (R.finishHandler client (head keys) (Time 20))
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
          R.acceptReply rt key
          R.finishHandler client key (Time 0.07)
        before <- atomically (readTVar (R.runtimeFacts rt))
        Map.size (R.attempts before) @?= 1
        Map.size (L.leases (R.ledger before)) @?= 0
        atomically (R.observeSend client (Time 0.1))
        after <- atomically (readTVar (R.runtimeFacts rt))
        Map.size (R.attempts after) @?= 0
        Map.size (R.pending after) @?= 1
        atomically (R.settleReceipt rt key)
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
        atomically (R.finishHandler client a (Time 0.7))
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
          R.finishHandler old key (Time 100)
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
          R.acceptReply rt key
          R.finishHandler client key (Time 0.2)
        atomically (Map.elems . R.pending <$> readTVar (R.runtimeFacts rt))
          >>= (@?= [(R.NetworkOwner key, True)])
        atomically (R.settleReceipt rt key)
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    , testCase "submitting claims survive repeated removal until their owner cleans up" $ withClient $ \rt client -> do
        a <- admit client 0
        b <- admit client 1
        atomically $ do
          R.claimUseful rt a >>= check
          R.claimUseful rt b >>= check
          R.acceptReply rt a
          R.closePeer rt 0
          R.closePeer rt 0
        atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 2)
        atomically (R.clearUnsubmitted rt b)
        st <- atomically (readTVar (R.runtimeFacts rt))
        Map.elems (R.pending st) @?= [(R.NetworkOwner a, True)]
        Map.null (L.leases (R.ledger st)) @?= True
        atomically (R.settleReceipt rt a)
        atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    ]

withClient :: (R.Runtime IO Int -> R.Client IO Int -> IO ()) -> IO ()
withClient action = do
  ready <- atomically W.newReadiness
  rt <- R.newRuntime (defaultLeiosFetchStaticEnv maxBound) ready (pure False)
  client <- atomically (openClient rt 0 False (pure Nothing))
  action rt client

admit :: R.Client IO Int -> Word -> IO (L.AttemptKey Int)
admit client slot = do
  let point = MkLeiosPoint (SlotNo (fromIntegral slot)) (endorserBlockHash (mkEndorserBlock []))
      req = LeiosBlockRequest (MkLeiosBlockRequest point 1000 maxBound)
  atomically (R.enqueueRequest client (Time 0) req)
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
        ( R.enqueueRequest
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
  let admitCopy n = atomically (R.enqueueRequest (clients !! n) (Time (fromIntegral n * 10)) req)
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
  key <- atomically (R.enqueueRequest fresh (Time 50) req)
  assertBool "all prior copies retired permits a new primary" (key /= Nothing)

testTerminalGate :: Bool -> Assertion
testTerminalGate body = withProgressiveWork body $ \rt clients out req -> do
  let first = clients !! 0
      second = clients !! 1
      third = clients !! 2
  a <- atomically (R.enqueueRequest first (Time 0) req) >>= maybe (fail "primary") pure
  atomically $ do
    R.commitRequest first a >>= check
    R.observeSend first (Time 0)
    modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out)
  b <- atomically (R.enqueueRequest second (Time 10) req) >>= maybe (fail "hedge") pure
  atomically $ do
    R.commitRequest second b >>= check
    R.observeSend second (Time 10)
    modifyTVar (R.runtimeFacts rt) (expireHeads (Time 20) out)
    R.completeWire first a (Time 20)
  atomically (R.enqueueRequest third (Time 20) req) >>= (@?= Nothing)
  atomically $ do
    R.claimUseful rt a >>= check
    R.acceptReply rt a
    R.finishHandler first a (Time 20)
    R.closeClient first
  atomically (R.enqueueRequest third (Time 20) req) >>= (@?= Nothing)
  -- An unconfirmed accepted receipt can leave work missing; settling pending
  -- ownership permits another attempt while the stalled second copy remains.
  atomically (R.settleReceipt rt a)
  replacement <- atomically (R.enqueueRequest third (Time 20) req)
  assertBool "unconfirmed receipt permits further recovery" (replacement /= Nothing)

-- Each fixture shares the same queue with its runtime, just like a kernel peer.
openClient ::
  R.Runtime IO Int -> Int -> Bool -> STM IO (Maybe T.RttObservation) -> STM IO (R.Client IO Int)
openClient rt peer big sample = do
  queue <- Checked.newTVar Seq.empty
  R.openClient rt peer big queue sample

expireHeads :: Time -> LeiosOutstanding Int -> R.RuntimeState IO Int -> R.RuntimeState IO Int
expireHeads now out = R.expireHeads now (`workStillNeeded` out)

expireHeadsObserved ::
  Time ->
  LeiosOutstanding Int ->
  R.RuntimeState IO Int ->
  (R.RuntimeState IO Int, [R.PenaltyCharge Int])
expireHeadsObserved now out = R.expireHeadsObserved now (`workStillNeeded` out)
