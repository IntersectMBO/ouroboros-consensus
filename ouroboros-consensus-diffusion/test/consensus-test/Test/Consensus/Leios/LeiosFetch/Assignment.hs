module Test.Consensus.Leios.LeiosFetch.Assignment (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
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
import Ouroboros.Consensus.Util.IOLike
import Test.Consensus.Leios.LeiosFetch.Replies (fixture, newContext)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ranked progressive assignment"
    [ testCase "admission retains the selected EB offer, invocation and score" $ withClosure $ \rt out p -> do
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
        let admissions = [(key, selected) | A.Detail key (FetchAdmitted _ _ _ _ selected) <- reports]
        length admissions @?= 5
        -- Destroy the session: delayed trace formatting still has original facts.
        atomically (R.closePeer rt 1)
        forM_ admissions $ \(L.AttemptKey peer generation _, selected) -> do
          peer @?= 1
          generation @?= 1
          selected @?= Just (FetchSelection (Time 10) P.zeroScore (Just (Time 3)))
    , testCase "untrusted oversized jobs reject without crashing or consuming another EB's credit" $ withClosureConfig oneJobCredit $ \rt out p -> do
        let badPoint = p{pointSlotNo = SlotNo 4}
            badPool = Jobs.mkJobPool 65536 20000 (IntMap.singleton 0 (hashLeiosTx (MkLeiosTx BS.empty), 100000))
            both = addPoint badPoint (MkEbState SNothing (BodyAcquired badPool)) out
            offers = Map.unionWith Map.union (closureOffers p [0]) (closureOffers badPoint [0])
        updated <- planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers both
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 52041 0
        [workPoint | L.Lease (L.Job workPoint _) _ <- Map.elems (L.leases (R.ledger state))] @?= [p]
        case Map.lookup badPoint (ebState updated) of
          Just (MkEbState _ (BodyAcquired pool)) -> Jobs.jobMultiplicities pool @?= IntMap.singleton 0 0
          _ -> assertFailure "lost unserviceable job"
    , testCase "local forge acquisition is never requested from upstreams" $ withState $ \rt out -> do
        let local = markReceivingFromForge point out
        void
          ( planRequests
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
        assigned <- planRequests rt (Time 0) (const (Right maxBound)) Nothing times offers out
        first <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger first) @?= L.Usage 260205 0
        L.usage 0 (R.ledger first) @?= L.Usage 0 0
        let better = Map.insert 0 (Map.singleton p (Time 0)) times
        void (planRequests rt (Time 1) (const (Right maxBound)) Nothing better offers assigned)
        after <- atomically (readTVar (R.runtimeFacts rt))
        Map.keys (L.leases (R.ledger after)) @?= Map.keys (L.leases (R.ledger first))
    , testCase "failure evidence outranks an earlier offer for new assignments" $ withClosure $ \rt out p -> do
        atomically $ modifyTVar (R.runtimeFacts rt) $ \st ->
          st{R.sessions = Map.adjust (\session -> session{R.closurePenalty = penalty 1}) 0 (R.sessions st)}
        let times = Map.fromList [(0, Map.singleton p (Time 0)), (1, Map.singleton p (Time 2))]
        void (planRequests rt (Time 3) (const (Right maxBound)) Nothing times (closureOffers p [0, 1]) out)
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger state) @?= L.Usage 260205 0
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
        _ <- planRequests rt (Time 600) (const (Right maxBound)) Nothing times offers out
        after <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger after) @?= L.Usage 260205 0
        Map.map R.invocation (R.sessions after) @?= Map.map R.invocation (R.sessions before)
    , testCase "a full preferred source falls back and retained offers use released capacity" $ withClosureConfig oneJobCredit $ \rt out p -> do
        job <- fixtureJob out p
        let times = Map.fromList [(0, Map.singleton p (Time 0)), (1, Map.singleton p (Time 1))]
            offers = closureOffers p [0, 1]
            client = R.Client rt 0 0
            oldPoint = p{pointSlotNo = SlotNo 1}
        old <-
          atomically
            ( R.enqueueRequest
                client
                (Time 0)
                (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest oldPoint (NEIntMap.singleton 0 job)))
            )
            >>= maybe (assertFailure "old credit" >> fail "unreachable") pure
        assigned <- planRequests rt (Time 1) (const (Right maxBound)) Nothing times offers out
        first <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger first) @?= L.Usage 52041 0
        Map.size (L.leases (R.ledger first)) @?= 2
        atomically (R.discardAttempt rt old)
        void (planRequests rt (Time 2) (const (Right maxBound)) Nothing times offers assigned)
        after <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger after) @?= L.Usage 52041 0
        Map.size (L.leases (R.ledger after)) @?= 2
    , testCase "a blocked preferred source cannot stall assignment to another source" $ withClosure $ \rt out p -> do
        let client = R.Client rt 0 0
        key <-
          atomically
            (R.enqueueRequest client (Time 0) (LeiosBlockRequest (MkLeiosBlockRequest p 1000 maxBound)))
            >>= maybe (assertFailure "body head" >> fail "unreachable") pure
        atomically $ do
          R.commitRequest client key >>= check
          R.observeSend client (Time 0)
          modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) out)
        void
          (planRequests rt (Time 10) (const (Right maxBound)) Nothing Map.empty (closureOffers p [0, 1]) out)
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 1 (R.ledger state) @?= L.Usage 260205 0
    , testCase "global EB order and opposing source arrivals preserve scarce cross-EB capacity" $ withClosureConfig oneJobCredit $ \rt out p -> do
        let q = p{pointSlotNo = SlotNo 6}
            small = trimToOneJob out
            both = addPoint q (ebState small Map.! p) small
            times =
              Map.fromList
                [(0, Map.fromList [(p, Time 2), (q, Time 1)]), (1, Map.fromList [(p, Time 1), (q, Time 2)])]
            offers = Map.unionWith Map.union (closureOffers p [0, 1]) (closureOffers q [0, 1])
        void (planRequests rt (Time 3) (const (Right maxBound)) Nothing times offers both)
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
        void (planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers both)
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
            plan at = planRequests rt (Time at) (const (Right maxBound)) Nothing Map.empty offers out
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
        void (planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty offers focused)
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 0 4
    , testCase "pending winner suppresses new copies and interrupted winner permits retry" $ withState $ \rt out -> do
        let offers peers = Map.fromList [(p, Map.singleton point bodyOffer) | p <- peers]
        void (planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (offers [0]) out)
        state <- atomically (readTVar (R.runtimeFacts rt))
        key <- case Map.keys (R.attempts state) of
          [one] -> pure one
          _ -> assertFailure "one request expected" >> fail "unreachable"
        atomically (R.claimUseful rt key) >>= (@?= True)
        void (planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (offers [1, 2, 3]) out)
        count rt >>= (@?= 1)
        atomically $ R.clearUnsubmitted rt key >> R.discardAttempt rt key
        void (planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (offers [0, 1, 2, 3]) out)
        count rt >>= (@?= 1)
    , testCase "closure-only offers and missing trusted bounds cannot request a body" $ withState $ \rt out -> do
        let noBounds = MkPeerOffer SNothing (OfferedBody 10)
            offers =
              Map.fromList
                [ (0, Map.singleton point noBounds)
                , (1, Map.singleton point (MkPeerOffer (SJust maxBound) OfferedClosure))
                ]
        void
          ( planRequests
              rt
              (Time 0)
              (const (Left (error "unused forecast failure")))
              Nothing
              Map.empty
              offers
              out
          )
        count rt >>= (@?= 0)
    , testCase "five full jobs fit a 260205-byte closure, with no eager TX copies" $ withClosure $ \rt out p -> do
        updated <-
          planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (closureOffers p [0 .. 3]) out
        state <- atomically (readTVar (R.runtimeFacts rt))
        L.usage 0 (R.ledger state) @?= L.Usage 260205 0
        Map.size (L.leases (R.ledger state)) @?= 5
        Map.size (requestedJobsPerPeer updated) @?= 1
        forM_ (Map.elems (R.attempts state)) $ \attempt -> case R.attemptRequest attempt of
          LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) -> NEIntMap.size jobs @?= 1
          _ -> assertFailure "unexpected body"
    , testCase "blocked heads progressively rescue followers without a fixed copy ceiling" $ withClosure $ \rt out p -> do
        assigned <-
          planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (closureOffers p [0 .. 3]) out
        original <- atomically (readTVar (R.runtimeFacts rt))
        key <- firstKey original
        let client = R.Client rt 0 0
        atomically $ do
          R.commitRequest client key >>= check
          R.observeSend client (Time 0)
          modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) assigned)
        hedged <-
          planRequests
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
          planRequests
            rt
            (Time 20)
            (const (Right maxBound))
            Nothing
            Map.empty
            (closureOffers p [0 .. 3])
            hedged
        count rt >>= (@?= 15)
        void
          ( planRequests
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
          ( planRequests
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
            ( R.enqueueRequest
                client
                (Time 0)
                (LeiosBlockRequest (MkLeiosBlockRequest p 1000 maxBound))
            )
            >>= maybe (assertFailure "body admission" >> fail "unreachable") pure
        atomically $ R.commitRequest client bodyKey >>= check >> R.observeSend client (Time 0)
        assigned <-
          planRequests rt (Time 0) (const (Right maxBound)) Nothing Map.empty (closureOffers p [0]) out
        atomically (modifyTVar (R.runtimeFacts rt) (expireHeads (Time 10) assigned))
        void
          ( planRequests
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
          ( planRequests
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
          ( planRequests
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
      pool = newLeiosJobPool (defaultLeiosFetchStaticEnv 262144) (IntMap.fromList (zip [0 ..] refs))
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
defaultConfig = defaultLeiosFetchStaticEnv maxBound

planRequests ::
  Ord peer =>
  R.Runtime IO peer ->
  Time ->
  (SlotNo -> Either OutsideForecastRange LeiosEbByteSizeBounds) ->
  Maybe (SlotNo, Word64) ->
  ClosureOfferTimes peer ->
  Map.Map peer (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding peer ->
  IO (LeiosOutstanding peer)
planRequests = assignRequests

orderedOffers :: Maybe (SlotNo, Word64) -> Map.Map LeiosPoint v -> [(LeiosPoint, v)]
orderedOffers horizon offers =
  let (high, low) = fetchPriorityTiers horizon [(pointSlotNo p, (p, v)) | (p, v) <- Map.toList offers]
   in high ++ low

expireHeads ::
  Ord peer => Time -> LeiosOutstanding peer -> R.RuntimeState m peer -> R.RuntimeState m peer
expireHeads now out = R.expireHeads now (`workStillNeeded` out)
