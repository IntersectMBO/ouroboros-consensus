{-# LANGUAGE DataKinds #-}
module Test.Consensus.Leios.LeiosFetch.Assignment (tests) where

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (Origin))
import Control.Monad (foldM, forM, forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Forecast (OutsideForecastRange (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision hiding (orderedClosureSources)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision as Policy
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel (OfferTimes (..), noteOfferTime)
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
  ( assignRequests
  , expireRuntimeHeads
  , fixture
  , newContext
  , tryEnqueueRequest
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ranked progressive assignment"
    [ testGroup "priority barriers"
        [ testCase "small refunds accumulate for the earlier job" testCreditAccumulation
        , testCase "a selected barrier also stops smaller jobs in its own EB" testWithinEbBarrier
        , testCase
            "TX barriers permit bodies but protect even zero-byte later work"
            testBarrierScopes
        , testCase
            "another source taking the barrier schedules exactly one reconsideration"
            testBarrierRescue
        , testCase "unchanged no-progress passes do not notify themselves" testNoProgressWake
        , testCase "pruning or loss of focus removes the next pass barrier" testBarrierRelevance
        , testCase "replacement invocations do not inherit a source stop" testBarrierReconnect
        , testCase
            "shared command exhaustion stops both request kinds for this pass"
            testCommandStop
        , testCase "body exhaustion still permits TX work" testBodyStop
        , testCase "zero-byte work can use a source with no free TX bytes" testZeroCredit
        , testCase
            "invalid and permanently unfit jobs do not become priority barriers"
            testUnfitBarrier
        , testCase "empty terminal pools skip source ranking without acquisition" testEmptyPool
        ]
    , testCase "pressure reports the actual temporary TX refusal" $
        withClosureConfig oneJobCredit $ \rt out p -> do
          (assigned, events) <-
            assignRequestsObserved rt (Time 0) (const (Right maxBound)) Nothing Map.empty
              (closureOffers p [0]) out
          [kinds | A.Detail _ (FetchPressureChanged kinds _) <- events]
            @?= [[FetchTransactionCredit]]
          facts <- atomically (readTVar (R.runtimeFacts rt))
          L.usage 0 (R.ledger facts) @?= L.Usage 52041 0 1
          let picked =
                [sum (IntMap.elems (Jobs.jobMultiplicities pool))
                | MkEbState _ (BodyAcquired pool) <- Map.elems (ebState assigned)]
          picked @?= [1]
    , testCase "permanently unfit work consumes neither picks nor temporary pressure" $
        withClosureConfig oneJobCredit $ \rt out p -> do
          let refs = [(hashLeiosTx (MkLeiosTx (BS.replicate 100000 0)), 100000)]
              sampledEb = mkEndorserBlock refs
              target = p{pointEbHash = endorserBlockHash sampledEb}
              pool = Jobs.mkJobPool 20000 20000 (IntMap.fromList (zip [0 ..] refs))
              initial =
                addPoint target (MkEbState SNothing (BodyAcquired pool)) out{ebState = Map.empty}
          (assigned, events) <-
            assignRequestsObserved rt (Time 0) (const (Right maxBound)) Nothing Map.empty
              (closureOffers target [0]) initial
          [kinds | A.Detail _ (FetchPressureChanged kinds _) <- events] @?= []
          facts <- atomically (readTVar (R.runtimeFacts rt))
          Map.null (L.leases (R.ledger facts)) @?= True
          Map.null (requestedJobsPerPeer assigned) @?= True
          case Map.lookup target (ebState assigned) of
            Just (MkEbState _ (BodyAcquired current)) ->
              Jobs.jobMultiplicities current @?= IntMap.singleton 0 0
            _ -> assertFailure "unfit job was lost"
    , testCase "body and shared-count pressure come from the reservation decision" $
        forM_ [False, True] $ \bodies -> withClosure $ \rt out p -> do
          let client = R.Client rt 0 0
              require = either (fail . show) pure
          forM_ ([0 .. if bodies then 3 else 149] :: [Int]) $ \i -> do
            let reservedPoint = p{pointSlotNo = SlotNo (100 + fromIntegral i)}
                request
                  | bodies = LeiosBlockRequest (MkLeiosBlockRequest reservedPoint 1000 maxBound)
                  | otherwise =
                      LeiosBlockTxsRequest $
                        MkLeiosBlockTxsRequest reservedPoint $
                          NEIntMap.singleton 0 $
                            Jobs.MkLeiosJob mempty 0 (Jobs.jobCommitmentOfTxs [])
            _ <- atomically (R.enqueueRequest client (Time 0) request) >>= require
            pure ()
          let current =
                if bodies then out{ebState = Map.singleton p (MkEbState SNothing NoBody)} else out
              offers =
                if bodies then Map.singleton 0 (Map.singleton p bodyOffer) else closureOffers p [0]
          (_, events) <-
            assignRequestsObserved rt (Time 0) (const (Right maxBound))
              Nothing Map.empty offers current
          [kinds | A.Detail _ (FetchPressureChanged kinds _) <- events]
            @?= [[if bodies then FetchBodyCount else FetchCommandCount]]
    , testCase "admission retains the selected EB offer, invocation and score" $
        withClosure $ \rt out p -> do
          let times = Map.singleton 1 (Map.singleton p (Time 3))
          (_, reports) <-
            assignRequestsObserved
              rt
              (Time 10)
              (const (Right maxBound))
              Nothing
              times
              (closureOffers p [0, 1])
              out
          let admissions =
                [(key, selected) | A.Detail key (FetchAdmitted _ _ _ _ selected) <- reports]
          length admissions @?= 5
          -- Destroy the session: delayed trace formatting still has original facts.
          atomically (R.closePeer rt 1)
          forM_ admissions $ \(L.AttemptKey peer generation _, selected) -> do
            peer @?= 1
            generation @?= 1
            selected @?= SJust (FetchSelection (Time 10) P.zeroScore (SJust (Time 3)))
    , testCase "untrusted oversized jobs reject without crashing or consuming another EB's credit" $ withClosureConfig oneJobCredit $ \rt out p -> do
        let badPoint = p{pointSlotNo = SlotNo 4}
            badPool = Jobs.mkJobPool 65536 20000 (IntMap.singleton 0 (hashLeiosTx (MkLeiosTx BS.empty), 100000))
            both = addPoint badPoint (MkEbState SNothing (BodyAcquired badPool)) out
            offers = Map.unionWith Map.union (closureOffers p [0]) (closureOffers badPoint [0])
        updated <- assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers both
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 52041 0 1
        [workPoint | L.Lease (L.Job workPoint _) _ <- Map.elems (L.leases (R.ledger state))] @?= [p]
        case Map.lookup badPoint (ebState updated) of
          Just (MkEbState _ (BodyAcquired pool)) -> Jobs.jobMultiplicities pool @?= IntMap.singleton 0 0
          _ -> assertFailure "lost unserviceable job"
    , testCase "local forge acquisition is never requested from upstreams" $ withState $ \rt out -> do
        let local = markReceivingFromForge point out
        void
          ( assignRequests
              rt
              (Time 0)
              (const (Right maxBound))
              Nothing
              Map.empty
              (Map.singleton 0 (Map.singleton point (MkPeerOffer (SJust maxBound) (OfferedBodyAndClosure 1000))))
              local
          )
        atomically (Map.null . R.attempts <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)
    , testCase "body and closure timestamps are independent and never renewed" $ do
        let bodyStamp = noteOfferTime (Time 1) (OfferedBody 10) Nothing
            closure = noteOfferTime (Time 4) OfferedClosure (Just bodyStamp)
        bodyStamp @?= OfferTimes (Just (Time 1)) Nothing
        closure @?= OfferTimes (Just (Time 1)) (Just (Time 4))
        noteOfferTime (Time 8) (OfferedBodyAndClosure 10) (Just closure) @?= closure
        noteOfferTime (Time 0) OfferedClosure (Just closure) @?= OfferTimes (Just (Time 1)) (Just (Time 0))
    , testCase "earliest closure offer wins without moving existing queued work" $ withClosure $ \rt out p -> do
        let times = Map.fromList [(0, Map.singleton p (Time 2)), (1, Map.singleton p (Time 1))]
            offers = closureOffers p [0, 1]
        assigned <- assignRequests rt (Time 0) (const (Right maxBound)) Nothing times offers out
        first <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger first) @?= L.Usage 260205 0 5
        L.usage 0 (R.ledger first) @?= L.Usage 0 0 0
        queues <- atomically $ traverse (readTVar . R.queued) (R.sessions first)
        let immutable state =
              Map.map
                ( \attempt -> case R.attemptRequest attempt of
                    LeiosBlockRequest req -> Left (lbrPoint req, lbrOfferedSize req, lbrEbBounds req)
                    LeiosBlockTxsRequest (MkLeiosBlockTxsRequest target jobs) -> Right (target, jobs)
                )
                (R.attempts state)
            usage state = [(peer, L.usage peer (R.ledger state)) | peer <- Map.keys (R.sessions state)]
            better = Map.insert 0 (Map.singleton p (Time 0)) times
        void (assignRequests rt (Time 1) (const (Right maxBound)) Nothing better offers assigned)
        after <- atomically (readTVar (R.runtimeFacts rt))
        L.leases (R.ledger after) @?= L.leases (R.ledger first)
        immutable after @?= immutable first
        usage after @?= usage first
        atomically (traverse (readTVar . R.queued) (R.sessions after)) >>= (@?= queues)
    , testCase "failure evidence outranks an earlier offer for new assignments" $ withClosure $ \rt out p -> do
        atomically $ modifyTVar (R.runtimeFacts rt) $ \st ->
          st{R.sessions = Map.adjust (\session -> session{R.closurePenalty = penalty 1}) 0 (R.sessions st)}
        let times = Map.fromList [(0, Map.singleton p (Time 0)), (1, Map.singleton p (Time 2))]
        void $
          assignRequests rt (Time 3) (const (Right maxBound))
            Nothing times (closureOffers p [0, 1]) out
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger state) @?= L.Usage 260205 0 5
    , testCase "aging restores the earliest eligible offer on the same idle session" $ withClosure $ \rt out p -> do
        atomically $ modifyTVar (R.runtimeFacts rt) $ \st ->
          st{R.sessions = Map.adjust (\session -> session{R.closurePenalty = penalty 1}) 0 (R.sessions st)}
        before <- atomically (readTVar (R.runtimeFacts rt))
        let times = Map.fromList [(0, Map.singleton p (Time 0)), (1, Map.singleton p (Time 1))]
            offers = closureOffers p [0, 1]
            rank at =
              map
                fst
                (Policy.orderedClosureSources p (Map.map (R.closureScore at) (R.sessions before)) times offers)
        rank (Time 300) @?= [1, 0]
        rank (Time 600) @?= [0, 1]
        _ <- assignRequests rt (Time 600) (const (Right maxBound)) Nothing times offers out
        after <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger after) @?= L.Usage 260205 0 5
        Map.map R.invocation (R.sessions after) @?= Map.map R.invocation (R.sessions before)
    , testCase "a full preferred source falls back and retained offers use released capacity" $ withClosureConfig oneJobCredit $ \rt out p -> do
        job <- fixtureJob out p
        let times = Map.fromList [(0, Map.singleton p (Time 0)), (1, Map.singleton p (Time 1))]
            offers = closureOffers p [0, 1]
            client = R.Client rt 0 0
            oldPoint = p{pointSlotNo = SlotNo 1}
        old <-
          atomically
            ( tryEnqueueRequest
                client
                (Time 0)
                (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest oldPoint (NEIntMap.singleton 0 job)))
            )
            >>= maybe (assertFailure "old credit" >> fail "unreachable") pure
        assigned <- assignRequests rt (Time 1) (const (Right maxBound)) Nothing times offers out
        first <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger first) @?= L.Usage 52041 0 1
        Map.size (L.leases (R.ledger first)) @?= 2
        atomically (R.discardAttempt rt old)
        void (assignRequests rt (Time 2) (const (Right maxBound)) Nothing times offers assigned)
        after <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger after) @?= L.Usage 52041 0 1
        Map.size (L.leases (R.ledger after)) @?= 2
    , testCase "a blocked preferred source cannot stall assignment to another source" $ withClosure $ \rt out p -> do
        let client = R.Client rt 0 0
        key <-
          atomically
            ( tryEnqueueRequest client (Time 0) $
                LeiosBlockRequest (MkLeiosBlockRequest p 1000 maxBound)
            )
            >>= maybe (assertFailure "body head" >> fail "unreachable") pure
        atomically $ do
          R.commitRequest client key >>= check
          R.observeSend client (Time 0)
          modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out)
        void
          ( assignRequests
              rt (Time 10) (const (Right maxBound)) Nothing Map.empty (closureOffers p [0, 1]) out)
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger state) @?= L.Usage 260205 0 5
    , testCase "global EB order and opposing source arrivals preserve scarce cross-EB capacity" $ withClosureConfig oneJobCredit $ \rt out p -> do
        let q = p{pointSlotNo = SlotNo 6}
            small = trimToOneJob out
            both = addPoint q (ebState small Map.! p) small
            times =
              Map.fromList
                [(0, Map.fromList [(p, Time 2), (q, Time 1)]), (1, Map.fromList [(p, Time 1), (q, Time 2)])]
            offers = Map.unionWith Map.union (closureOffers p [0, 1]) (closureOffers q [0, 1])
        void (assignRequests rt (Time 3) (const (Right maxBound)) Nothing times offers both)
        state <- atomically (readTVar (R.runtimeFacts rt))
        let workByPeer =
              Map.fromList [(peer, R.attemptWork a) | (L.AttemptKey peer _ _, a) <- Map.toList (R.attempts state)]
        workByPeer @?= Map.fromList [(0, L.Job q 0), (1, L.Job p 0)]
    , testCase "EB tier order precedes body work without changing body source order" $ withClosure $ \rt out p -> do
        let q = p{pointSlotNo = SlotNo 6}
            both = addPoint q (MkEbState SNothing NoBody) (trimToOneJob out)
            offers =
              Map.unionWith
                Map.union
                (closureOffers p [0, 1])
                (Map.fromList [(peer, Map.singleton q bodyOffer) | peer <- [0 .. 3]])
        void (assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers both)
        state <- atomically (readTVar (R.runtimeFacts rt))
        case Map.lookup 0 (R.sessions state) of
          Just session -> do
            commands <- atomically (readTVar (R.queued session))
            [ R.attemptWork a
              | L.CommandKey generation ordinal <- foldr (:) [] commands
              , Just a <- [Map.lookup (L.AttemptKey 0 generation ordinal) (R.attempts state)]
              ]
              @?= [L.Job p 0, L.Body q]
          Nothing -> assertFailure "missing session"
        let bodyPeers = [peer | (L.AttemptKey peer _ _, a) <- Map.toList (R.attempts state), R.attemptWork a == L.Body q]
        bodyPeers @?= [0]
    , testCase "priority tiers and unknown horizon retain deterministic EB ordering" $ do
        let points = Map.fromList [(point{pointSlotNo = SlotNo slot}, ()) | slot <- [1, 2, 9, 10]]
            slots = map (pointSlotNo . fst)
        slots (orderedOffers (Just (SlotNo 10, 2)) points) @?= map SlotNo [9, 10, 2, 1]
        slots (orderedOffers Nothing points) @?= map SlotNo [1, 2, 9, 10]
    , testCase "closure order uses arrival before address and failure before arrival" $ do
        let scores = Map.fromList [(0 :: Int, 0), (1, 0)]
            times = Map.fromList [(0, Map.singleton point (Time 2)), (1, Map.singleton point (Time 1))]
            offers = closureOffers point [0, 1]
        orderedClosureSources point scores times offers @?= [1, 0]
        orderedClosureSources point (Map.insert 1 1 scores) times offers @?= [0, 1]
    , testCase "opposing EB arrivals cannot be represented by one peer ordering" $ do
        let other = point{pointSlotNo = SlotNo 99}
            scores = Map.fromList [(0 :: Int, 0), (1, 0)]
            times =
              Map.fromList
                [ (0, Map.fromList [(point, Time 2), (other, Time 1)])
                , (1, Map.fromList [(point, Time 1), (other, Time 2)])
                ]
            offers = Map.unionWith Map.union (closureOffers point [0, 1]) (closureOffers other [0, 1])
        orderedClosureSources point scores times offers @?= [1, 0]
        orderedClosureSources other scores times offers @?= [0, 1]
    , testCase "known equal times tie by registration and missing metadata remains usable" $ do
        let scores = Map.fromList [(0 :: Int, 0), (1, 0), (2, 0), (4, 0)]
            times =
              Map.fromList
                [ (1, Map.singleton point (Time 2))
                , (2, Map.singleton point (Time 2))
                , (3, Map.singleton point (Time 0))
                ]
            offers = Map.insert 4 (Map.singleton point bodyOffer) (closureOffers point [0, 1, 2, 3])
        orderedClosureSources point scores times offers @?= [1, 2, 0]
    , testCase "bodies hedge progressively through four peers only as each source stalls" $ withState $ \rt out -> do
        let offers = Map.fromList [(peer, Map.singleton point bodyOffer) | peer <- [0 .. 3]]
            plan at =
              assignRequests rt (Time at) (const (Right maxBound)) Nothing Map.empty offers out
        void (plan 0)
        count rt >>= (@?= 1)
        void (plan 1)
        count rt >>= (@?= 1)
        forM_ [0 .. 2] $ \source -> do
          facts <- atomically (readTVar (R.runtimeFacts rt))
          key <- case [k | k@(L.AttemptKey peer _ _) <- Map.keys (L.leases (R.ledger facts)), peer == source] of
            [k] -> pure k
            _ -> assertFailure "one source body" >> fail "unreachable"
          let client = R.Client rt source (fromIntegral source)
              at = fromIntegral source * 10
          atomically $ do
            R.commitRequest client key >>= check
            R.observeSend client (Time at)
            modifyTVar (R.runtimeFacts rt) (expireHeads (Time (at + 10)) out)
          void (plan (at + 10))
          count rt >>= (@?= source + 2)
          void (plan (at + 10))
          count rt >>= (@?= source + 2)
    , testCase "body count across EBs is independent of TX byte credit" $ withState $ \rt out -> do
        let points = [point{pointSlotNo = SlotNo n} | n <- [1 .. 5]]
            focused = case Map.toList (elFocus out) of
              (ElectionId _ issuer, hash) : _ ->
                out
                  { ebState = Map.fromList [(p, MkEbState SNothing NoBody) | p <- points]
                  , elFocus = Map.fromList [(ElectionId (pointSlotNo p) issuer, hash) | p <- points]
                  }
              _ -> out
            offers = Map.singleton 0 (Map.fromList [(p, bodyOffer) | p <- points])
        void (assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers focused)
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 0 4 4
    , testCase "pending winner suppresses new copies and interrupted winner permits retry" $ withState $ \rt out -> do
        let offers peers = Map.fromList [(p, Map.singleton point bodyOffer) | p <- peers]
        void $
          assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (offers [0]) out
        state <- atomically (readTVar (R.runtimeFacts rt))
        key <- case Map.keys (R.attempts state) of
          [one] -> pure one
          _ -> assertFailure "one request expected" >> fail "unreachable"
        atomically (R.claimUseful rt key) >>= (@?= True)
        void $
          assignRequests rt (Time 0) (const (Right maxBound))
            Nothing Map.empty (offers [1, 2, 3]) out
        count rt >>= (@?= 1)
        atomically $ R.clearUnsubmitted rt key (L.Body point) >> R.discardAttempt rt key
        void $
          assignRequests rt (Time 0) (const (Right maxBound))
            Nothing Map.empty (offers [0, 1, 2, 3]) out
        count rt >>= (@?= 1)
    , testCase "closure-only offers and missing trusted bounds cannot request a body" $ withState $ \rt out -> do
        let noBounds = MkPeerOffer SNothing (OfferedBody 10)
            offers =
              Map.fromList
                [ (0, Map.singleton point noBounds)
                , (1, Map.singleton point (MkPeerOffer (SJust maxBound) OfferedClosure))
                ]
        void
          ( assignRequests
              rt
              (Time 0)
              (const (Left (error "unused forecast failure")))
              Nothing
              Map.empty
              offers
              out
          )
        count rt >>= (@?= 0)
    , testCase "a body offer survives a forecast miss and becomes requestable without another offer" $ withState $ \rt out -> do
        let offers = Map.singleton 0 (Map.singleton point (MkPeerOffer SNothing (OfferedBody 10)))
            beyond slot = Left (OutsideForecastRange Origin slot slot)
        waiting <- assignRequests rt (Time 0) beyond Nothing Map.empty offers out
        count rt >>= (@?= 0)
        -- Keep the original offer and outstanding state from the failed pass.
        -- Only the forecast advances; no second announcement/offer is supplied.
        void (assignRequests rt (Time 1) (const (Right maxBound)) Nothing Map.empty offers waiting)
        state <- atomically (readTVar (R.runtimeFacts rt))
        case map R.attemptRequest (Map.elems (R.attempts state)) of
          [LeiosBlockRequest (MkLeiosBlockRequest requested bytes _)] -> do
            requested @?= point
            bytes @?= 10
          _ -> assertFailure "the retained body offer was not requested"
    , testCase "five full jobs fit a 260205-byte closure, with no eager TX copies" $ withClosure $ \rt out p -> do
        updated <-
          assignRequests rt (Time 0) (const (Right maxBound))
            Nothing Map.empty (closureOffers p [0 .. 3]) out
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 260205 0 5
        Map.size (L.leases (R.ledger state)) @?= 5
        Map.size (requestedJobsPerPeer updated) @?= 1
        forM_ (Map.elems (R.attempts state)) $ \attempt -> case R.attemptRequest attempt of
          LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) -> NEIntMap.size jobs @?= 1
          _ -> assertFailure "unexpected body"
    , testCase "blocked heads progressively rescue followers without a fixed copy ceiling" $ withClosure $ \rt out p -> do
        assigned <-
          assignRequests rt (Time 0) (const (Right maxBound))
            Nothing Map.empty (closureOffers p [0 .. 3]) out
        original <- atomically (readTVar (R.runtimeFacts rt))
        key <- firstKey original
        let client = R.Client rt 0 0
        atomically $ do
          R.commitRequest client key >>= check
          R.observeSend client (Time 0)
          modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) assigned)
        hedged <-
          assignRequests
            rt
            (Time 10)
            (const (Right maxBound))
            Nothing
            Map.empty
            (closureOffers p [0 .. 3])
            assigned
        raced <- atomically (readTVar (R.runtimeFacts rt))
        Map.size (L.leases (R.ledger raced)) @?= 10
        forM_ [0 .. 4] $ \i -> L.copies (L.Job p i) (R.ledger raced) @?= 2
        forM_ (Map.toList (R.attempts raced)) $ \(k, attempt) ->
          when (k /= key) (T.clockHead (R.attemptClock attempt) @?= Nothing)
        second <- case [k | k@(L.AttemptKey source _ _) <- Map.keys (R.attempts raced), source == 1] of
          k : _ -> pure k
          [] -> assertFailure "missing hedge" >> fail "unreachable"
        atomically $ do
          R.commitRequest (R.Client rt 1 1) second >>= check
          R.observeSend (R.Client rt 1 1) (Time 10)
          modifyTVar (R.runtimeFacts rt) (expireHeads (Time 20) hedged)
        expanded <-
          assignRequests
            rt
            (Time 20)
            (const (Right maxBound))
            Nothing
            Map.empty
            (closureOffers p [0 .. 3])
            hedged
        count rt >>= (@?= 15)
        void
          ( assignRequests
              rt
              (Time 20)
              (const (Right maxBound))
              Nothing
              Map.empty
              (closureOffers p [0 .. 3])
              expanded
          )
        count rt >>= (@?= 15)
        -- A real terminal unblocks the source even if local handling still waits.
        atomically (R.completeWire client key (Time 20))
        atomically (R.blocked 0 <$> readTVar (R.runtimeFacts rt)) >>= (@?= False)
        beforeRemoval <- atomically (readTVar (R.runtimeFacts rt))
        atomically (R.closePeer rt 0)
        void
          ( assignRequests
              rt
              (Time 21)
              (const (Right maxBound))
              Nothing
              Map.empty
              (closureOffers p [0 .. 3])
              (removePeerFromOutstanding 0 expanded)
          )
        recovered <- atomically (readTVar (R.runtimeFacts rt))
        fmap R.attemptClock (Map.lookup second (R.attempts recovered))
          @?= fmap R.attemptClock (Map.lookup second (R.attempts beforeRemoval))
        Map.size (L.leases (R.ledger recovered)) @?= 10
    , testCase "unwanted body head remains guarded and exposes its TX followers" $ withClosure $ \rt out p -> do
        let client = R.Client rt 0 0
        bodyKey <-
          atomically
            ( tryEnqueueRequest
                client
                (Time 0)
                (LeiosBlockRequest (MkLeiosBlockRequest p 1000 maxBound))
            )
            >>= maybe (assertFailure "body admission" >> fail "unreachable") pure
        atomically $ R.commitRequest client bodyKey >>= check >> R.observeSend client (Time 0)
        assigned <-
          assignRequests rt (Time 0) (const (Right maxBound))
            Nothing Map.empty (closureOffers p [0]) out
        atomically (modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) assigned))
        void
          ( assignRequests
              rt
              (Time 10)
              (const (Right maxBound))
              Nothing
              Map.empty
              (closureOffers p [0 .. 3])
              assigned
          )
        count rt >>= (@?= 11)
        facts <- atomically (readTVar (R.runtimeFacts rt))
        forM_ [0 .. 4] $ \i -> L.copies (L.Job p i) (R.ledger facts) @?= 2
    , testCase "body possession and a separate full-closure offer are both required" $ withClosure $ \rt out p -> do
        void
          ( assignRequests
              rt
              (Time 0)
              (const (Right maxBound))
              Nothing
              Map.empty
              (Map.singleton 0 (Map.singleton p (MkPeerOffer (SJust maxBound) (OfferedBody 10))))
              out
          )
        count rt >>= (@?= 0)
        let noBody = out{ebState = Map.singleton p (MkEbState SNothing NoBody)}
        void
          ( assignRequests
              rt
              (Time 0)
              (const (Right maxBound))
              Nothing
              Map.empty
              (closureOffers p [0 .. 3])
              noBody
          )
        count rt >>= (@?= 0)
    ]
 where
  (point, body, _) = fixture
  bodyOffer = MkPeerOffer (SJust maxBound) (OfferedBody (encodeEndorserBlockSize body))
  count rt = atomically (Map.size . L.leases . R.ledger <$> readTVar (R.runtimeFacts rt))

-- A high-priority 60 kB job and a later 5 kB job share a 65,536 B source.
-- Five older 10 kB reservations initially leave enough room only for the latter.
withPriorityCredit ::
  (R.Runtime IO Int -> R.Client IO Int -> LeiosOutstanding Int ->
   LeiosPoint -> LeiosPoint -> [L.AttemptKey Int] -> IO ()) ->
  IO ()
withPriorityCredit action = withStateConfig oneJobCredit $ \rt base -> do
  let (early, first) = addSizedPoint (SlotNo 5) [60000] base{ebState = Map.empty}
      (later, out) = addSizedPoint (SlotNo 6) [5000] first
      client = R.Client rt 0 0
  loans <- reserveOlder client (replicate 5 10000)
  action rt client out early later loans

addSizedPoint :: SlotNo -> [Word32] -> LeiosOutstanding Int -> (LeiosPoint, LeiosOutstanding Int)
addSizedPoint slot sizes out =
  let refs =
        [ (hashLeiosTx (MkLeiosTx (BS.replicate (fromIntegral bytes) (fromIntegral i))), bytes)
        | (i, bytes) <- zip [0 :: Int ..] sizes
        ]
      eb = mkEndorserBlock refs
      p = MkLeiosPoint slot (endorserBlockHash eb)
      pool = Jobs.mkJobPool 20000 20000 (IntMap.fromList (zip [0 ..] refs))
   in (p, addPoint p (MkEbState SNothing (BodyAcquired pool)) out)

-- These older queued requests are outside the current planning fixture's
-- frontier. Their credit remains until explicit unsent discard; no wire reply
-- is fabricated by these assignment tests.
reserveOlder :: R.Client IO Int -> [Word32] -> IO [L.AttemptKey Int]
reserveOlder client sizes = forM (zip [0 ..] sizes) $ \(jid, bytes) -> do
  let p = MkLeiosPoint (SlotNo 1) (endorserBlockHash (mkEndorserBlock []))
      req = LeiosBlockTxsRequest $
        MkLeiosBlockTxsRequest p $
          NEIntMap.singleton jid $
            Jobs.MkLeiosJob (IntSet.singleton jid) bytes (Jobs.jobCommitmentOfTxs [])
  atomically (R.enqueueRequest client (Time 0) req) >>= either (fail . show) pure

priorityPlan ::
  R.Runtime IO Int ->
  Map.Map Int (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding Int ->
  IO (LeiosOutstanding Int)
priorityPlan rt offers =
  assignRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers

ownersOf :: R.Runtime IO Int -> L.Work LeiosPoint -> IO [L.AttemptKey Int]
ownersOf rt work = atomically (R.workOwners work <$> readTVar (R.runtimeFacts rt))

takeReady :: R.Runtime IO Int -> IO Bool
takeReady rt = atomically $
  orElse (W.awaitReadiness (R.runtimeReady rt) >> pure True) (pure False)

testCreditAccumulation :: Assertion
testCreditAccumulation = withPriorityCredit $ \rt _ out early later loans -> do
  let offers = Map.unionWith Map.union (closureOffers early [0]) (closureOffers later [0])
      plan = priorityPlan rt offers
      blocked current = do
        next <- plan current
        ownersOf rt (L.Job early 0) >>= (@?= [])
        ownersOf rt (L.Job later 0) >>= (@?= [])
        pure next
  initial <- blocked out
  accumulating <- foldM
    (\current key -> atomically (R.discardAttempt rt key) >> blocked current)
    initial
    (init loans)
  atomically (R.discardAttempt rt (last loans))
  _ <- plan accumulating
  ownersOf rt (L.Job early 0) >>= (\xs -> length xs @?= 1)
  ownersOf rt (L.Job later 0) >>= (\xs -> length xs @?= 1)
  -- Both now fit, and the first admitted command must be the earlier large job.
  facts <- atomically (readTVar (R.runtimeFacts rt))
  let fresh = [(key, lease) | (key, lease) <- Map.toAscList (L.leases (R.ledger facts))]
  map (L.leaseWork . snd) fresh @?= [L.Job early 0, L.Job later 0]
  L.usage 0 (R.ledger facts) @?= L.Usage 65000 0 2

testWithinEbBarrier :: Assertion
testWithinEbBarrier = withPriorityCredit $ \rt _ out early _ _ -> do
  let (p, mixed) = addSizedPoint (pointSlotNo early) [60000, 10000] out{ebState = Map.empty}
      pool = case ebState mixed Map.! p of
        MkEbState _ (BodyAcquired value) -> value
        _ -> error "body fixture"
      seeds =
        [ generator
        | seed <- [0 .. 99]
        , let generator = mkStdGen seed
        , Just (Jobs.MkLeiosJobId 0, _, _, _) <-
            [Jobs.pickLeastRequestedJobExcept generator IntSet.empty pool]
        ]
  next <- priorityPlan rt (closureOffers p [0]) mixed{leiosFetchPrng = head seeds}
  ownersOf rt (L.Job p 0) >>= (@?= [])
  ownersOf rt (L.Job p 1) >>= (@?= [])
  case ebState next Map.! p of
    MkEbState _ (BodyAcquired current) ->
      Jobs.jobMultiplicities current @?= IntMap.fromList [(0, 0), (1, 0)]
    _ -> assertFailure "mixed pool disappeared"

testBarrierScopes :: Assertion
testBarrierScopes = withPriorityCredit $ \rt _ out early _ _ -> do
  let (zero, withZero) =
        addSizedPoint (SlotNo 6) [0] out{ebState = Map.singleton early (ebState out Map.! early)}
      missingBody = early{pointSlotNo = SlotNo 7}
      current = addPoint missingBody (MkEbState SNothing NoBody) withZero
      offers = Map.unionWith Map.union
        (Map.unionWith Map.union (closureOffers early [0]) (closureOffers zero [0]))
        (Map.singleton 0 (Map.singleton missingBody priorityBodyOffer))
  _ <- priorityPlan rt offers current
  ownersOf rt (L.Job early 0) >>= (@?= [])
  ownersOf rt (L.Job zero 0) >>= (@?= [])
  ownersOf rt (L.Body missingBody) >>= (\xs -> length xs @?= 1)

testBarrierRescue :: Assertion
testBarrierRescue = withPriorityCredit $ \rt _ out early later _ -> do
  let offers = Map.unionWith Map.union (closureOffers early [0, 1]) (closureOffers later [0])
      plan = priorityPlan rt offers
  _ <- takeReady rt
  first <- plan out
  ownersOf rt (L.Job early 0) >>= (\xs -> [peer | L.AttemptKey peer _ _ <- xs] @?= [1])
  ownersOf rt (L.Job later 0) >>= (@?= [])
  takeReady rt >>= (@?= True)
  takeReady rt >>= (@?= False)
  second <- plan first
  ownersOf rt (L.Job later 0) >>= (\xs -> [peer | L.AttemptKey peer _ _ <- xs] @?= [0])
  takeReady rt >>= (@?= False)
  _ <- plan second
  takeReady rt >>= (@?= False)

testNoProgressWake :: Assertion
testNoProgressWake = withPriorityCredit $ \rt _ out early later _ -> do
  let offers = Map.unionWith Map.union (closureOffers early [0]) (closureOffers later [0])
  _ <- takeReady rt
  first <- priorityPlan rt offers out
  takeReady rt >>= (@?= False)
  _ <- priorityPlan rt offers first
  takeReady rt >>= (@?= False)
  ownersOf rt (L.Job later 0) >>= (@?= [])

testBarrierRelevance :: Assertion
testBarrierRelevance = forM_ [False, True] $ \prune ->
  withPriorityCredit $ \rt _ out early later _ -> do
    let offers = Map.unionWith Map.union (closureOffers early [0]) (closureOffers later [0])
    first <- priorityPlan rt offers out
    let current
          | prune = pruneOutstandingToImmTip (pointSlotNo later) first
          | otherwise = first{elFocus = Map.filter (/= pointEbHash early) (elFocus first)}
    _ <- priorityPlan rt offers current
    ownersOf rt (L.Job later 0) >>= (\xs -> length xs @?= 1)

testBarrierReconnect :: Assertion
testBarrierReconnect = withPriorityCredit $ \rt old out early later _ -> do
  let offers = Map.unionWith Map.union (closureOffers early [0]) (closureOffers later [0])
  first <- priorityPlan rt offers out
  atomically (R.closeClient old)
  commands <- uncheckedNewTVarM Seq.empty
  fresh <- atomically (R.openClient rt 0 False commands (pure Nothing))
  atomically (R.closeClient old)
  _ <- priorityPlan rt offers (removePeerFromOutstanding 0 first)
  facts <- atomically (readTVar (R.runtimeFacts rt))
  [generation | L.AttemptKey 0 generation _ <- Map.keys (L.leases (R.ledger facts))]
    @?= replicate 2 (R.clientInvocation fresh)
  ownersOf rt (L.Job early 0) >>= (\xs -> length xs @?= 1)

testCommandStop :: Assertion
testCommandStop = withStateConfig oneJobCredit $ \rt base -> do
  let client = R.Client rt 0 0
      (p, first) = addSizedPoint (SlotNo 5) [20000, 20000, 20000] base{ebState = Map.empty}
      missingBody = p{pointSlotNo = SlotNo 6}
      out = addPoint missingBody (MkEbState SNothing NoBody) first
      offers = Map.unionWith Map.union (closureOffers p [0])
        (Map.singleton 0 (Map.singleton missingBody priorityBodyOffer))
      pool = case ebState out Map.! p of
        MkEbState _ (BodyAcquired value) -> value
        _ -> error "body fixture"
      once = case Jobs.pickLeastRequestedJobExcept (leiosFetchPrng out) IntSet.empty pool of
        Just (_, _, _, generator) -> generator
        Nothing -> error "nonempty fixture"
  -- Three candidates matter: selecting the only remaining candidate may
  -- consume no PRNG state, so a two-job fixture cannot detect an extra scan.
  _ <- reserveOlder client (replicate 150 0)
  _ <- takeReady rt
  next <- priorityPlan rt offers out
  leiosFetchPrng next @?= once
  ownersOf rt (L.Body missingBody) >>= (@?= [])
  Map.null (requestedJobsPerPeer next) @?= True
  takeReady rt >>= (@?= False)

testBodyStop :: Assertion
testBodyStop = withStateConfig oneJobCredit $ \rt base -> do
  let client = R.Client rt 0 0
      (p, first) = addSizedPoint (SlotNo 5) [20000] base{ebState = Map.empty}
      targets = [p{pointSlotNo = SlotNo n} | n <- [3, 4]]
      out = foldr (\target -> addPoint target (MkEbState SNothing NoBody)) first targets
      offers = Map.unionWith Map.union (closureOffers p [0])
        (Map.singleton 0 (Map.fromList [(target, priorityBodyOffer) | target <- targets]))
  forM_ [100 .. 103] $ \slot -> do
    let req = LeiosBlockRequest (MkLeiosBlockRequest p{pointSlotNo = SlotNo slot} 1000 maxBound)
    _ <- atomically (R.enqueueRequest client (Time 0) req) >>= either (fail . show) pure
    pure ()
  _ <- priorityPlan rt offers out
  facts <- atomically (readTVar (R.runtimeFacts rt))
  L.usage 0 (R.ledger facts) @?= L.Usage 20000 4 5
  forM_ targets $ \target -> ownersOf rt (L.Body target) >>= (@?= [])

testZeroCredit :: Assertion
testZeroCredit = forM_ [False, True] $ \hasBarrier -> withStateConfig oneJobCredit $ \rt base -> do
  let client = R.Client rt 0 0
      (zero, first) = addSizedPoint (SlotNo 6) [0] base{ebState = Map.empty}
      (large, second) = addSizedPoint (SlotNo 5) [20000] first
      current = if hasBarrier then second else first
      offers = Map.unionWith Map.union (closureOffers zero [0]) (closureOffers large [0])
  _ <- reserveOlder client [65536]
  _ <- priorityPlan rt offers current
  ownersOf rt (L.Job zero 0) >>= (\xs -> length xs @?= if hasBarrier then 0 else 1)

testUnfitBarrier :: Assertion
testUnfitBarrier = forM_ [100000, 500001] $ \bytes -> withStateConfig oneJobCredit $ \rt base -> do
  let (bad, first) = addSizedPoint (SlotNo 5) [bytes] base{ebState = Map.empty}
      (good, out) = addSizedPoint (SlotNo 6) [10000] first
      offers = Map.unionWith Map.union (closureOffers bad [0]) (closureOffers good [0])
  _ <- priorityPlan rt offers out
  ownersOf rt (L.Job bad 0) >>= (@?= [])
  ownersOf rt (L.Job good 0) >>= (\xs -> length xs @?= 1)

testEmptyPool :: Assertion
testEmptyPool = withClosure $ \rt out p -> do
  let empty = out{ebState = Map.singleton p (MkEbState SNothing (BodyAcquired Jobs.emptyJobPool))}
  (next, reports) <-
    assignRequestsObserved rt (Time 0) (const (Right maxBound)) Nothing
      (error "ranked arrival times for an empty pool")
      (error "ranked sources for an empty pool")
      empty
  leiosFetchPrng next @?= leiosFetchPrng empty
  length reports @?= 0
  Map.null (requestedJobsPerPeer next) @?= True
  facts <- atomically (readTVar (R.runtimeFacts rt))
  Map.null (L.leases (R.ledger facts)) @?= True

priorityBodyOffer :: PeerOffer
priorityBodyOffer = MkPeerOffer (SJust maxBound) (OfferedBody 1000)

withState :: (R.Runtime IO Int -> LeiosOutstanding Int -> IO ()) -> IO ()
withState = withStateConfig defaultConfig

withStateConfig ::
  LeiosFetchStaticEnv -> (R.Runtime IO Int -> LeiosOutstanding Int -> IO ()) -> IO ()
withStateConfig cfg action = do
  (context, _) <- newContext
  rt <- R.newRuntime cfg (lfcReady context) (pure False)
  forM_ [0 .. 3] $ \peer -> do
    queue <- uncheckedNewTVarM Seq.empty
    void (atomically (R.openClient rt peer False queue (pure Nothing)))
  readMVar (lfcOutstanding context) >>= action rt

withClosure :: (R.Runtime IO Int -> LeiosOutstanding Int -> LeiosPoint -> IO ()) -> IO ()
withClosure = withClosureConfig defaultConfig

withClosureConfig ::
  LeiosFetchStaticEnv -> (R.Runtime IO Int -> LeiosOutstanding Int -> LeiosPoint -> IO ()) -> IO ()
withClosureConfig cfg action = withStateConfig cfg $ \rt original -> do
  let refs = [(hashLeiosTx (MkLeiosTx (BS.replicate 52041 n)), 52041) | n <- [0 .. 4]]
      body = mkEndorserBlock refs
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)
      pool = newLeiosJobPool (defaultLeiosFetchStaticEnv) (IntMap.fromList (zip [0 ..] refs))
      out =
        original
          { ebState = Map.singleton point (MkEbState SNothing (BodyAcquired pool))
          , elFocus = Map.map (const (pointEbHash point)) (elFocus original)
          }
  action rt out point

closureOffers :: LeiosPoint -> [Int] -> Map.Map Int (Map.Map LeiosPoint PeerOffer)
closureOffers point peers =
  Map.fromList
    [(peer, Map.singleton point (MkPeerOffer (SJust maxBound) OfferedClosure)) | peer <- peers]

firstKey :: R.RuntimeState IO Int -> IO (L.AttemptKey Int)
firstKey state = case Map.keys (R.attempts state) of
  key : _ -> pure key
  [] -> assertFailure "no request" >> fail "unreachable"

oneJobCredit :: LeiosFetchStaticEnv
oneJobCredit =
  defaultConfig{maxRequestedBytesSizePerPeer = 65536, maxRequestedBytesSizePerBigLedgerPeer = 65536}

fixtureJob :: LeiosOutstanding Int -> LeiosPoint -> IO Jobs.LeiosJob
fixtureJob out p = case Map.lookup p (ebState out) of
  Just (MkEbState _ (BodyAcquired pool)) ->
    maybe
      (assertFailure "missing job" >> fail "unreachable")
      pure
      (Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool)
  _ -> assertFailure "missing pool" >> fail "unreachable"

trimToOneJob :: LeiosOutstanding Int -> LeiosOutstanding Int
trimToOneJob out = out{ebState = fmap trim (ebState out)}
 where
  trim (MkEbState onset (BodyAcquired pool)) = MkEbState onset (BodyAcquired (foldr (Jobs.completeJob . Jobs.MkLeiosJobId) pool [1 .. 4]))
  trim state = state

addPoint :: LeiosPoint -> EbState -> LeiosOutstanding Int -> LeiosOutstanding Int
addPoint p state out =
  out
    { ebState = Map.insert p state (ebState out)
    , elFocus =
        Map.union
          ( Map.fromList
              [ (ElectionId (pointSlotNo p) issuer, pointEbHash p)
              | (ElectionId _ issuer, _) <- Map.toList (elFocus out)
              ]
          )
          (elFocus out)
    }

penalty :: Int -> P.Penalty
penalty n = iterate (P.chargePenalty (Time 0)) P.emptyPenalty !! n

orderedClosureSources ::
  Ord peer =>
  LeiosPoint ->
  Map.Map peer Int ->
  ClosureOfferTimes peer ->
  Map.Map peer (Map.Map LeiosPoint PeerOffer) ->
  [peer]
orderedClosureSources point counts times offers =
  map
    fst
    ( Policy.orderedClosureSources
        point
        (Map.map (P.effectiveScore (Time 0) . penalty) counts)
        times
        offers
    )

defaultConfig :: LeiosFetchStaticEnv
defaultConfig = defaultLeiosFetchStaticEnv

orderedOffers :: Maybe (SlotNo, Word64) -> Map.Map LeiosPoint v -> [(LeiosPoint, v)]
orderedOffers horizon offers =
  let (high, low) = fetchPriorityTiers horizon [(pointSlotNo p, (p, v)) | (p, v) <- Map.toList offers]
   in high ++ low

expireHeads ::
  Ord peer => Time -> LeiosOutstanding peer -> R.RuntimeState m peer -> R.RuntimeState m peer
expireHeads now out = expireRuntimeHeads now (`workStillNeeded` out)
