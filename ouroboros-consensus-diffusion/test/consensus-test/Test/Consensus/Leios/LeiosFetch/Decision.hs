{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests of the LeiosFetch decision logic, on scenarios built directly from
-- its outstanding state and the peers' offers.
module Test.Consensus.Leios.LeiosFetch.Decision (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.Function ((&))
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.NonEmpty as NEIntSet
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word64)
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbByteSizeBounds (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (MkPeerOffer)
  , acquireEbBody
  , completeTxRequest
  , defaultLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , fetchPriorityTiers
  , focusCertifiedEb
  , focusElectionIfUnfocused
  , leiosFetchLogicIteration
  , newLeiosJobPool
  , peerBudget
  , pruneOffers
  , pruneOutstandingToImmTip
  , recordReferencedPoint
  , refundEbRequest
  , refundTxRequest
  , removePeerFromOutstanding
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import System.Random (mkStdGen)
import Test.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , choose
  , counterexample
  , forAll
  , frequency
  , property
  , sublistOf
  , tabulate
  , vectorOf
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "decision"
    [ testGroup
        "EB body fetch"
        [ testCase "single missing body with one offering peer issues one request" $
            test_singleMissingBody
        , testCase "no request when no peer offers the body" $
            test_bodyNoOffer
        , testCase "a peer already asked for a body is not asked again" $
            test_bodyAlreadyRequestedPeerSkipped
        , testCase "both offering peers are selected (no per-EB cap)" $
            test_bodyTwoPeersOffer
        , testCase
            "a peer offering the body and the closure is asked for the body once, even after its reply"
            test_bodyAskedOnceOfBothOfferer
        , testCase
            "a closure offered while the decision is pruning a body offer is still asked of the peer"
            test_closureOfferedMeanwhile
        , testCase
            "an endorser block rejected as too big at a point is never fetched again at that point"
            test_noRefetchOversized
        , testCase "per-peer byte budget exhausted skips that peer" $
            test_perPeerByteBudget
        , testCase
            "a body that does not fit a peer's remaining budget is not requested, nor is any offer after it"
            test_perPeerByteBudgetHard
        ]
    , testGroup
        "EB closure fetch"
        [ testCase "a big-ledger peer has a larger, but still finite, closure budget" $
            test_bigLedgerPeerBudget
        , testCase
            "job assignment draws within the least-requested bucket, at random, respecting exclusions"
            test_pickJobs
        ]
    , testGroup
        "focus"
        [ testCase "a verified certificate for an election older than the pruned slot changes nothing" $
            test_focusCertifiedPruned
        , testCase
            "a verified certificate moves its election onto an endorser block that lost the announcement race"
            test_focusCertifiedEb
        ]
    , testGroup
        "bookkeeping"
        [ testProperty
            "the outstanding state agrees with the requests in flight, whatever settles them"
            prop_bookkeeping
        ]
    , testGroup
        "fetch priority"
        [ testCase "freshest window oldest-first, then rest freshest-first" $
            test_fetchPriorityOrder
        ]
    ]

-- | With current slot S=100 and window L=10, EBs at slot >= 90 (the voting
-- window) are prioritised oldest-first, EBs beyond S trail that first tier, and
-- everything older is freshest-first.
test_fetchPriorityOrder :: IO ()
test_fetchPriorityOrder =
  map (unSlot . (.pointSlotNo)) (hi ++ lo)
    @?= [90, 95, 100, 101, 105, 89, 50, 0]
 where
  (hi, lo) = fetchPriorityTiers (Just (SlotNo 100, 10)) offers
  offers = [(SlotNo (fromIntegral a), point a 'x') | a <- [0, 105, 90, 50, 100, 89, 101, 95]]
  unSlot (SlotNo n) = n

{-------------------------------------------------------------------------------
  Scenarios
-------------------------------------------------------------------------------}

-- | Peers are just numbers here.
type Peer = Int

peerA, peerB :: Peer
peerA = 0
peerB = 1

test_singleMissingBody :: IO ()
test_singleMissingBody =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [point 1 'a']
    & runIteration
    & assertBodyRequest peerA (point 1 'a') 1024

test_bodyNoOffer :: IO ()
test_bodyNoOffer =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [point 1 'b'] -- peer offers a different EB, not the one we need
    & runIteration
    & assertNoRequests

test_bodyAlreadyRequestedPeerSkipped :: IO ()
test_bodyAlreadyRequestedPeerSkipped =
  empty
    & withMissingBody (point 1 'a') 1024
    & alreadyRequestedEbFrom (point 1 'a') [peerA] -- already in flight from peerA
    & offersBody peerA [point 1 'a'] -- so peerA is not asked again
    & offersBody peerB [point 1 'a'] -- but a fresh offering peer is
    & runIteration
    & assertRequestPeers [peerB]

test_bodyTwoPeersOffer :: IO ()
test_bodyTwoPeersOffer =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [point 1 'a']
    & offersBody peerB [point 1 'a']
    & runIteration
    -- both peers offer; with no per-EB cap the body is requested from both
    & assertRequestPeerCount 2

-- | A peer that offers both the body and the closure is asked for the body, and
-- not again once its reply has arrived but before the body is held. Its
-- closure is still asked of it once the body is held.
test_bodyAskedOnceOfBothOfferer :: IO ()
test_bodyAskedOnceOfBothOfferer = do
  assertBodyRequest peerA p 1024 reqs1
  assertNoRequests reqs2
  requestedOffsets reqs3 @?= IntSet.singleton 0
 where
  p = point 1 'a'
  offers0 = Map.singleton p (MkPeerOffer SNothing (OfferedBodyAndClosure 1024))
  decide outstanding offers =
    leiosFetchLogicIteration
      env
      anyBounds
      (Just (minBound, priorityWindow))
      (Map.singleton peerA offers)
      Map.empty
      outstanding
  (outstanding1, reqs1, prunes1) = decide (scOutstanding (empty & withMissingBody p 1024)) offers0
  offers1 = pruneOffers (maybe Map.empty NEMap.toMap (Map.lookup peerA prunes1)) offers0
  -- The reply arrived, but its write has not settled, so the body is not held
  -- yet.
  outstanding2 = refundEbRequest peerA p 1024 outstanding1
  (_, reqs2, _) = decide outstanding2 offers1
  jobPool =
    Jobs.mkJobPool
      (maxJobBytesSize env)
      (maxJobTxCount env)
      (IntMap.singleton 0 (txHashOf 0, 100))
  (_, reqs3, _) = decide (acquireEbBody p jobPool outstanding2) offers1

-- | A peer offers the body, and then the closure after a decision has read its
-- offers but before they are pruned. Whether the body was still to fetch or
-- already held, the closure is asked of the peer once the body is held.
test_closureOfferedMeanwhile :: IO ()
test_closureOfferedMeanwhile = do
  requestedOffsets (closureRequests missing) @?= IntSet.singleton 0
  requestedOffsets (closureRequests (acquireEbBody p jobPool missing)) @?= IntSet.singleton 0
 where
  p = point 1 'a'
  missing = scOutstanding (empty & withMissingBody p 1024)
  jobPool =
    Jobs.mkJobPool
      (maxJobBytesSize env)
      (maxJobTxCount env)
      (IntMap.singleton 0 (txHashOf 0, 100))
  bodyOffered = Map.singleton p (MkPeerOffer SNothing (OfferedBody 1024))
  closureOffered = MkPeerOffer SNothing OfferedClosure
  decide outstanding offers =
    leiosFetchLogicIteration
      env
      anyBounds
      (Just (minBound, priorityWindow))
      (Map.singleton peerA offers)
      Map.empty
      outstanding
  closureRequests outstanding0 = reqs
   where
    (outstanding1, _, prunes) = decide outstanding0 bodyOffered
    offers =
      pruneOffers
        (maybe Map.empty NEMap.toMap (Map.lookup peerA prunes))
        (Map.insertWith (<>) p closureOffered bodyOffered)
    (_, reqs, _) = decide (acquireEbBody p jobPool (refundEbRequest peerA p 1024 outstanding1)) offers

-- | An endorser block rejected as too big, then offered again at the same point,
-- under bounds that admit it. The decision logic does not consult the bounds
-- for a body it holds, so this guards against it starting to.
test_noRefetchOversized :: IO ()
test_noRefetchOversized =
  empty
    & withMissingBody (point 1 'a') 1024
    & onOutstanding
      (acquireEbBody (point 1 'a') Jobs.emptyJobPool)
    & offersBody peerA [point 1 'a']
    & runIterationWith (\_slot -> Right (MkLeiosEbByteSizeBounds 1024 5000))
    & assertNoRequests

test_perPeerByteBudget :: IO ()
test_perPeerByteBudget =
  empty
    & withMissingBody (point 1 'a') 1024
    & withRequestedBytesPerPeer peerA (maxRequestedBytesSizePerPeer env + 1)
    & offersBody peerA [point 1 'a']
    & offersBody peerB [point 1 'a']
    & runIteration
    & assertRequestPeers [peerB]

-- | A peer is asked for a body only if it fits in what remains of the peer's
-- cap, and the first body that does not fit ends the peer's walk, so a smaller
-- one offered after it is not asked for either.
test_perPeerByteBudgetHard :: IO ()
test_perPeerByteBudgetHard = do
  bodiesRequested (cap - 1024) @?= [point 1 'a']
  bodiesRequested (cap - 1023) @?= []
 where
  cap = maxRequestedBytesSizePerPeer env
  bodiesRequested used =
    let reqs =
          empty
            & withMissingBody (point 1 'a') 1024
            & withMissingBody (point 2 'b') 100
            & withRequestedBytesPerPeer peerA used
            & offersBody peerA [point 1 'a', point 2 'b']
            -- Both points are in the high-priority tier, which is walked
            -- oldest first.
            & runIterationAt (SlotNo 2)
     in [r.lbrPoint | LeiosBlockRequest r <- maybe [] toList (Map.lookup peerA reqs)]

test_focusCertifiedPruned :: IO ()
test_focusCertifiedPruned =
  (Map.size (elFocus outstanding), Map.size (ebState outstanding)) @?= (0, 0)
 where
  outstanding =
    focusCertifiedEb (ElectionId (SlotNo 10) fixtureElectee) (eb 'a') $
      emptyLeiosOutstanding (mkStdGen 0) (SlotNo 20)

-- | An announcement focuses an election on its endorser block, and a second
-- announcement for that election does not move the focus. A verified
-- certificate for the second does, and tracks its point, so a peer offering
-- both is asked only for the certified one. Its offer of the other is kept, in
-- case some election fetches that later.
test_focusCertifiedEb :: IO ()
test_focusCertifiedEb = do
  Map.lookup elId (elFocus announced) @?= Just (eb 'a')
  Map.lookup elId (elFocus certified) @?= Just (eb 'b')
  Map.member pB (ebState certified) @?= True
  [r.lbrPoint | LeiosBlockRequest r <- maybe [] toList (Map.lookup peerA reqs)] @?= [pB]
  maybe [] (toList . NEMap.keys) (Map.lookup peerA prunes) @?= [pB]
 where
  elId = ElectionId (SlotNo 10) fixtureElectee
  pA = point 10 'a'
  pB = point 10 'b'
  -- The first announcement takes the election's focus and tracks its point;
  -- the second changes nothing.
  announced =
    focusElectionIfUnfocused elId (eb 'b')
      . focusElectionIfUnfocused elId (eb 'a')
      . recordReferencedPoint pA SNothing
      $ emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
  certified = focusCertifiedEb elId (eb 'b') announced
  offers = Map.fromList [(p, MkPeerOffer SNothing (OfferedBody 1024)) | p <- [pA, pB]]
  (_, reqs, prunes) =
    leiosFetchLogicIteration
      env
      anyBounds
      (Just (SlotNo 11, priorityWindow))
      (Map.singleton peerA offers)
      Map.empty
      certified

test_bigLedgerPeerBudget :: IO ()
test_bigLedgerPeerBudget = do
  -- past the ordinary cap, an ordinary peer is asked for nothing ...
  run ordinary (ordinaryCap + 1) @?= IntSet.empty
  -- ... but a big-ledger peer still has budget for the whole pool at once
  run bigLedger (ordinaryCap + 1) @?= IntSet.fromList offsets
  -- past even the big-ledger cap, though, a big-ledger peer is bounded too
  run bigLedger (bigLedgerCap + 1) @?= IntSet.empty
 where
  offsets = [0 .. 4]
  p = point 10 'a'
  misses = IntMap.fromList [(off, (txHashOf off, 100)) | off <- offsets]
  jobPool = Jobs.mkJobPool (maxJobBytesSize env) (maxJobTxCount env) misses
  -- The body is already held in these runs, so the offer names no size; only
  -- its closure half is consulted.
  offers = Map.singleton peerA $ Map.singleton p (MkPeerOffer SNothing OfferedClosure)
  ordinaryCap = maxRequestedBytesSizePerPeer env
  bigLedgerCap = maxRequestedBytesSizePerBigLedgerPeer env
  -- hold the body (so the pool is live), with the peer's in-flight bytes
  -- preloaded to 'used'
  run bigLedgerPeers used =
    let outstanding =
          (\o -> o{requestedBytesSizePerPeer = Map.singleton peerA used})
            -- the election fetching it, without which nothing is requested
            . focusElectionIfUnfocused (ElectionId (SlotNo 10) fixtureElectee) (eb 'a')
            . acquireEbBody p jobPool
            . recordReferencedPoint p SNothing
            $ emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
        (_o, reqs, _d) =
          leiosFetchLogicIteration
            env
            anyBounds
            (Just (SlotNo 11, priorityWindow))
            offers
            bigLedgerPeers
            outstanding
     in requestedOffsets reqs
  ordinary = Map.empty
  bigLedger = Map.singleton peerA IsBigLedgerPeer

test_pickJobs :: IO ()
test_pickJobs = do
  -- across seeds the draw isn't pinned to the lowest id, and every draw is a
  -- real job id.
  let drawn = Set.fromList [i | s <- [0 .. 99], Just i <- [pickId pool0 IntSet.empty s]]
  assertBool "shuffles: more than one distinct job is drawn" (Set.size drawn > 1)
  assertBool "only ever draws real job ids" (drawn `Set.isSubsetOf` Set.fromList [0 .. 5])
  -- an excluded job is never drawn: exclude all but job 5.
  Set.fromList [i | s <- [0 .. 50], Just i <- [pickId pool0 (IntSet.fromList [0 .. 4]) s]]
    @?= Set.singleton 5
  -- the least-requested bucket wins regardless of the draw: force job 0 to
  -- multiplicity 1 (by excluding the rest), then an unrestricted draw comes
  -- only from the still-least-requested jobs 1..5, never job 0.
  case Jobs.pickLeastRequestedJobExcept (mkStdGen 0) (IntSet.fromList [1 .. 5]) pool0 of
    Nothing -> assertFailure "forced pick of the sole non-excluded job failed"
    Just (_jid, _job, pool1, _prng') -> do
      let drawnFromPool1 = Set.fromList [i | s <- [0 .. 50], Just i <- [pickId pool1 IntSet.empty s]]
      assertBool
        "least-requested bucket wins: job 0 (multiplicity 1) is not drawn"
        (not (0 `Set.member` drawnFromPool1))
      assertBool "still shuffles among the least-requested jobs" (Set.size drawnFromPool1 > 1)
 where
  misses = IntMap.fromList [(off, (txHashOf off, 100)) | off <- [0 .. 5]]
  -- 'maxJobTxCount' 1 makes each tx its own job, so job ids 0..5 all start at
  -- multiplicity 0 (one bucket).
  pool0 = Jobs.mkJobPool 1000000 1 misses
  pickId pool excluded s =
    case Jobs.pickLeastRequestedJobExcept (mkStdGen s) excluded pool of
      Just (Jobs.MkLeiosJobId i, _job, _pool', _prng') -> Just i
      Nothing -> Nothing

{-------------------------------------------------------------------------------
  Bookkeeping
-------------------------------------------------------------------------------}

-- | Decide, and settle what was decided, in any order: after every step, the
-- outstanding state's accounting of each peer's requests must agree with the
-- requests actually in flight.
prop_bookkeeping :: Property
prop_bookkeeping =
  forAll genSetup $ \setup ->
    forAll (vectorOf 100 genStep) $ \steps ->
      let sims = scanl (simulate setup) (initialSim setup) steps
          settled = [req | sim <- sims, (_peer, req) <- simSettled sim]
       in tabulate "body requests settled" [bucket (filter isBodyRequest settled)] $
            tabulate "txs requests settled" [bucket (filter (not . isBodyRequest) settled)] $
              case [(n, vs) | (n, sim) <- zip [0 :: Int ..] sims, let vs = violations sim, not (null vs)] of
                [] -> property True
                (n, vs) : _ -> counterexample (unlines (("after " <> show n <> " steps:") : vs)) False
 where
  bucket reqs = case length reqs of
    0 -> "none"
    n
      | n < 10 -> "fewer than 10"
      | otherwise -> "10 or more"

-- | The peers, the endorser blocks they offer, and what the closures lack.
data Setup = MkSetup
  { setupSeed :: !Int
  , setupOffers :: !(Map.Map Peer (Map.Map LeiosPoint PeerOffer))
  , setupMisses :: !(Map.Map LeiosPoint (IntMap.IntMap (TxHash, BytesSize)))
  -- ^ The transactions each point lacks once its body is held
  }
  deriving Show

-- | One step of the decision loop or of what settles its requests.
data Step
  = -- | Decide, and send the requests.
    Decide
  | -- | One of the body requests in flight is answered, and its write settles.
    BodyReply Int
  | -- | One of the txs requests in flight is answered, and its write settles.
    TxsReply Int
  | -- | A peer disconnects.
    Disconnect Int
  | -- | A request of a disconnected peer settles.
    LateReply Int
  | -- | The immutable tip reaches this slot.
    Prune Word64
  deriving Show

data Sim = MkSim
  { simOutstanding :: !(LeiosOutstanding Peer)
  , simOffers :: !(Map.Map Peer (Map.Map LeiosPoint PeerOffer))
  -- ^ The offers of each connected peer
  , simInFlight :: !(Map.Map Peer [LeiosFetchRequest])
  -- ^ The requests in flight from each connected peer
  , simLate :: ![(Peer, LeiosFetchRequest)]
  -- ^ The requests that were in flight when their peer disconnected
  , simSettled :: ![(Peer, LeiosFetchRequest)]
  -- ^ What the last step settled
  }

-- | Small enough that the per-peer budgets and the job and request sizes bind.
propEnv :: LeiosFetchStaticEnv
propEnv =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 3000
    , maxRequestedBytesSizePerBigLedgerPeer = 6000
    , maxRequestBytesSize = 1000
    , maxJobBytesSize = 500
    , maxJobTxCount = 4
    }

propPeers :: [Peer]
propPeers = [0, 1, 2]

propBigLedgerPeers :: Map.Map Peer IsBigLedgerPeer
propBigLedgerPeers = Map.singleton 2 IsBigLedgerPeer

-- | Each in its own slot, so each is its own election's.
propPoints :: [LeiosPoint]
propPoints = zipWith point [1 ..] "abcdef"

genSetup :: Gen Setup
genSetup =
  MkSetup
    <$> arbitrary
    <*> (Map.fromList <$> traverse (\peer -> (,) peer <$> genOffers) propPeers)
    <*> (Map.fromList <$> traverse (\p -> (,) p <$> genMisses) propPoints)
 where
  genOffers = do
    points <- sublistOf propPoints
    Map.fromList <$> traverse (\p -> (,) p . MkPeerOffer SNothing <$> genHalves) points
  genHalves =
    frequency
      [ (1, OfferedBody <$> choose (100, 2000))
      , (1, pure OfferedClosure)
      , (3, OfferedBodyAndClosure <$> choose (100, 2000))
      ]
  genMisses = do
    n <- choose (1, 12)
    sizes <- vectorOf n (choose (50, 400))
    pure $ IntMap.fromList [(off, (txHashOf off, sz)) | (off, sz) <- zip [0 ..] sizes]

genStep :: Gen Step
genStep =
  frequency
    [ (10, pure Decide)
    , (8, BodyReply <$> arbitrary)
    , (8, TxsReply <$> arbitrary)
    , (1, Disconnect <$> arbitrary)
    , (1, LateReply <$> arbitrary)
    , -- Never past the last two points.
      (1, Prune <$> choose (0, 5))
    ]

-- | Every point announced, each focusing its own election.
initialSim :: Setup -> Sim
initialSim setup =
  MkSim
    { simOutstanding =
        F.foldl' (flip announce) (emptyLeiosOutstanding (mkStdGen (setupSeed setup)) (SlotNo 0)) propPoints
    , simOffers = setupOffers setup
    , simInFlight = Map.empty
    , simLate = []
    , simSettled = []
    }
 where
  announce p@(MkLeiosPoint slot ebHash) =
    focusElectionIfUnfocused (ElectionId slot fixtureElectee) ebHash
      . recordReferencedPoint p SNothing

simulate :: Setup -> Sim -> Step -> Sim
simulate setup sim0 = \case
  Decide ->
    let (outstanding', reqs, prunes) =
          leiosFetchLogicIteration
            propEnv
            anyBounds
            (Just (SlotNo 0, priorityWindow))
            (simOffers sim)
            propBigLedgerPeers
            (simOutstanding sim)
     in sim
          { simOutstanding = outstanding'
          , simInFlight = Map.unionWith (<>) (simInFlight sim) (Map.map toList reqs)
          , simOffers =
              Map.mapWithKey
                (\peer -> pruneOffers (maybe Map.empty NEMap.toMap (Map.lookup peer prunes)))
                (simOffers sim)
          }
  BodyReply i -> reply i isBodyRequest
  TxsReply i -> reply i (not . isBodyRequest)
  Disconnect i -> case pickIndex i (Map.keys (simOffers sim)) of
    Nothing -> sim
    Just (peer, _rest) ->
      sim
        { simOutstanding = removePeerFromOutstanding peer (simOutstanding sim)
        , simOffers = Map.delete peer (simOffers sim)
        , simInFlight = Map.delete peer (simInFlight sim)
        , simLate = [(peer, req) | req <- Map.findWithDefault [] peer (simInFlight sim)] <> simLate sim
        }
  LateReply i -> case pickIndex i (simLate sim) of
    Nothing -> sim
    Just ((peer, req), rest) -> settle peer req sim{simLate = rest}
  Prune slot -> sim{simOutstanding = pruneOutstandingToImmTip (SlotNo slot) (simOutstanding sim)}
 where
  sim = sim0{simSettled = []}

  reply i isKind =
    case pickIndex
      i
      [ (peer, k)
      | (peer, reqs) <- Map.toList (simInFlight sim)
      , (k, req) <- zip [0 ..] reqs
      , isKind req
      ] of
      Nothing -> sim
      Just ((peer, k), _rest) -> case pickIndex k (Map.findWithDefault [] peer (simInFlight sim)) of
        Nothing -> sim
        Just (req, rest) -> settle peer req sim{simInFlight = Map.insert peer rest (simInFlight sim)}

  -- As the reply handlers do: refund on arrival, then record what the write
  -- settled.
  settle peer req s =
    s
      { simOutstanding = case req of
          LeiosBlockRequest r ->
            acquireEbBody
              r.lbrPoint
              (newLeiosJobPool propEnv (Map.findWithDefault IntMap.empty r.lbrPoint (setupMisses setup)))
              (refundEbRequest peer r.lbrPoint r.lbrOfferedSize (simOutstanding s))
          LeiosBlockTxsRequest r ->
            completeTxRequest peer r (refundTxRequest peer (requestBytes req) (simOutstanding s))
      , simSettled = (peer, req) : simSettled s
      }

isBodyRequest :: LeiosFetchRequest -> Bool
isBodyRequest = \case
  LeiosBlockRequest{} -> True
  LeiosBlockTxsRequest{} -> False

-- | The bytes a request was charged to its peer's budget.
requestBytes :: LeiosFetchRequest -> BytesSize
requestBytes = \case
  LeiosBlockRequest r -> r.lbrOfferedSize
  LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _point jobs) ->
    sum [bytes | Jobs.MkLeiosJob _offsets bytes _commitment <- toList jobs]

-- | The element at this index, modulo the list's length, and the others.
pickIndex :: Int -> [a] -> Maybe (a, [a])
pickIndex i xs = go (i `mod` max 1 (length xs)) xs
 where
  go _ [] = Nothing
  go 0 (x : rest) = Just (x, rest)
  go n (x : rest) = fmap (x :) <$> go (n - 1) rest

-- | How the outstanding state disagrees with the requests in flight.
violations :: Sim -> [String]
violations sim =
  concatMap peerViolations propPeers
    <> [ "requestedEbPeers is not the inverse of requestedEbsPerPeer"
       | requestedEbPeers o
           /= Map.fromListWith
             Set.union
             [(p, Set.singleton peer) | (peer, ps) <- Map.toList (requestedEbsPerPeer o), p <- toList ps]
       ]
    <> concatMap poolViolations (Map.toList (ebState o))
 where
  o = simOutstanding sim
  inFlight peer = Map.findWithDefault [] peer (simInFlight sim)
  jobsHeld peer =
    Map.map NEIntSet.toSet (Map.findWithDefault Map.empty peer (requestedJobsPerPeer o))

  peerViolations peer =
    [ "peer " <> show peer <> " is charged " <> show charged <> " bytes for " <> show owed <> " in flight"
    | let charged = Map.findWithDefault 0 peer (requestedBytesSizePerPeer o)
          owed = sum (map requestBytes (inFlight peer))
    , charged /= owed
    ]
      <> [ "peer " <> show peer <> " is charged past its cap"
         | peerBudget propEnv (Map.findWithDefault IsNotBigLedgerPeer peer propBigLedgerPeers) o peer < 0
         ]
      <> [ "peer "
             <> show peer
             <> " is tracked with bodies "
             <> show tracked
             <> " for "
             <> show bodies
             <> " in flight"
         | let tracked = maybe Set.empty NESet.toSet (Map.lookup peer (requestedEbsPerPeer o))
               bodies = Set.fromList [r.lbrPoint | LeiosBlockRequest r <- inFlight peer]
         , tracked /= bodies
         ]
      <> [ "peer "
             <> show peer
             <> " is tracked with jobs "
             <> show tracked
             <> " for "
             <> show jobs
             <> " in flight"
         | let tracked = jobsHeld peer
               jobs =
                 Map.fromListWith
                   IntSet.union
                   [ (p, NEIntSet.toSet (NEIntMap.keysSet js))
                   | LeiosBlockTxsRequest (MkLeiosBlockTxsRequest p js) <- inFlight peer
                   ]
         , tracked /= jobs
         ]

  poolViolations (p, MkEbState _onset fetchState) = case fetchState of
    NoBody -> []
    BodyAcquired pool ->
      [show p <> "'s job pool has an inexact reverse index" | not (Jobs.hasExactReverseIndex pool)]
        <> [ show p
               <> "'s job "
               <> show jid
               <> " counts "
               <> show m
               <> " requests for "
               <> show n
               <> " in flight"
           | (jid, m) <- IntMap.toList (Jobs.jobMultiplicities pool)
           , let n = length [() | peer <- propPeers, maybe False (IntSet.member jid) (Map.lookup p (jobsHeld peer))]
           , m /= n
           ]

{-------------------------------------------------------------------------------
  Scenario DSL
-------------------------------------------------------------------------------}

-- | A test fixture: peer offerings, outstanding work.
data Scenario = Scenario
  { scOfferings :: !(Map.Map Peer (Set.Set LeiosPoint))
  -- ^ The points each peer offers the body of.
  , scOfferedSizes :: !(Map.Map LeiosPoint BytesSize)
  -- ^ The size each point is offered at.
  --
  -- NOTE a peculiarity of this harness: every peer offering a point offers it
  -- at the same size, the one 'withMissingBody' named. The protocol does not
  -- work that way --- each offer carries its own size, and the decision logic
  -- asks for whatever the peer it is asking claimed --- so no scenario here can
  -- have two peers disagree about a size. Nothing these tests cover turns on
  -- that.
  --
  -- It is a field of its own because the size is the offering peer's claim
  -- rather than anything the outstanding state holds, so the fixture must
  -- supply it.
  , scOutstanding :: !(LeiosOutstanding Peer)
  }

env :: LeiosFetchStaticEnv
env = defaultLeiosFetchStaticEnv (60 * 1000 * 1000)

empty :: Scenario
empty =
  Scenario
    { scOfferings = Map.empty
    , scOfferedSizes = Map.empty
    , scOutstanding = emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
    }

-- | The body of this point is still to fetch, and every peer that offers it
-- offers it at this size.
withMissingBody :: LeiosPoint -> BytesSize -> Scenario -> Scenario
withMissingBody p@(MkLeiosPoint slot ebHash) size sc =
  onOutstanding seed sc{scOfferedSizes = Map.insert p size (scOfferedSizes sc)}
 where
  -- Seed what the announce path would: the 'ebState' NoBody entry the fetch
  -- loop drives bodies off of, and the election that is fetching it, without
  -- which the loop asks no one for it. Only a certificate overrules an election
  -- already fetching something, so this seeds it the way an announcement would.
  seed =
    focusElectionIfUnfocused (ElectionId slot fixtureElectee) ebHash
      . recordReferencedPoint p SNothing

-- | The one pool a scenario here pretends announced everything, since these
-- have no headers to take an issuer from. An endorser block's election is
-- therefore just its slot, so two announced in one slot share an election and
-- only the first is fetched.
fixtureElectee :: KeyHash r
fixtureElectee = KeyHash $ UnsafeHash $ SBS.pack (replicate 28 0)

alreadyRequestedEbFrom :: LeiosPoint -> [Peer] -> Scenario -> Scenario
alreadyRequestedEbFrom p peers =
  onOutstanding $ \o ->
    o
      { requestedEbPeers =
          Map.insertWith Set.union p (Set.fromList peers) (requestedEbPeers o)
      , requestedEbsPerPeer =
          foldr
            (\peer -> Map.insertWith NESet.union peer (NESet.singleton p))
            (requestedEbsPerPeer o)
            peers
      }

-- | Set a per-peer in-flight byte total. Use with the env's per-peer
-- cap to test the per-peer byte budget.
withRequestedBytesPerPeer :: Peer -> BytesSize -> Scenario -> Scenario
withRequestedBytesPerPeer peer n =
  onOutstanding $ \o ->
    o{requestedBytesSizePerPeer = Map.insert peer n (requestedBytesSizePerPeer o)}

-- | The peer offers the body (only) of these points.
offersBody :: Peer -> [LeiosPoint] -> Scenario -> Scenario
offersBody peer points sc =
  sc
    { scOfferings =
        Map.insertWith
          Set.union
          peer
          (Set.fromList points)
          (scOfferings sc)
    }

-- | Lift a function on 'LeiosOutstanding' to one on 'Scenario'.
onOutstanding :: (LeiosOutstanding Peer -> LeiosOutstanding Peer) -> Scenario -> Scenario
onOutstanding f sc = sc{scOutstanding = f (scOutstanding sc)}

-- | Run the iteration and project the decisions.
runIteration :: Scenario -> Map.Map Peer (NESeq LeiosFetchRequest)
runIteration =
  -- Any known current slot suffices here: these scenarios don't depend on the
  -- offer-visit order (the priority order is tested by 'test_fetchPriorityOrder').
  runIterationAt minBound

-- | Run the iteration at this current slot and project the decisions.
runIterationAt :: SlotNo -> Scenario -> Map.Map Peer (NESeq LeiosFetchRequest)
runIterationAt = runIterationWithAt anyBounds

-- | 'runIteration', with the ledger's bounds forecast thus.
runIterationWith ::
  (SlotNo -> Either OutsideForecastRange LeiosEbByteSizeBounds) ->
  Scenario ->
  Map.Map Peer (NESeq LeiosFetchRequest)
runIterationWith forecastEbBounds = runIterationWithAt forecastEbBounds minBound

-- | 'runIterationAt', with the ledger's bounds forecast thus.
runIterationWithAt ::
  (SlotNo -> Either OutsideForecastRange LeiosEbByteSizeBounds) ->
  SlotNo ->
  Scenario ->
  Map.Map Peer (NESeq LeiosFetchRequest)
runIterationWithAt forecastEbBounds currentSlot sc =
  let (_out, reqs, _drops) =
        -- No big-ledger peers in these scenarios.
        leiosFetchLogicIteration
          env
          forecastEbBounds
          (Just (currentSlot, priorityWindow))
          (Map.map (Map.fromSet (resolveOfferSize sc.scOfferedSizes)) sc.scOfferings)
          Map.empty
          sc.scOutstanding
   in reqs

-- | Give a fixture's body offer the size 'withMissingBody' said peers offer that
-- point at, which is the size the decision logic then requests. A point no
-- 'withMissingBody' named is offered at size 0, which matters to no scenario:
-- no election fetches it.
resolveOfferSize :: Map.Map LeiosPoint BytesSize -> LeiosPoint -> PeerOffer
resolveOfferSize sizes p = MkPeerOffer SNothing (OfferedBody (Map.findWithDefault 0 p sizes))

-- | How many slots before the current one a point is still worth voting on, in
-- these tests.
priorityWindow :: Word64
priorityWindow = 10

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

assertBodyRequest ::
  Peer -> LeiosPoint -> BytesSize -> Map.Map Peer (NESeq LeiosFetchRequest) -> IO ()
assertBodyRequest peer p size m =
  case Map.lookup peer m of
    Nothing -> assertFailure $ "no request for peer " <> show peer
    Just reqs ->
      [ (pt.pointEbHash, sz)
      | LeiosBlockRequest (MkLeiosBlockRequest pt sz _) <- toList reqs
      ]
        @?= [(p.pointEbHash, size)]

assertNoRequests :: Map.Map Peer (NESeq LeiosFetchRequest) -> IO ()
assertNoRequests m = Map.keys m @?= []

-- | Assert that requests target exactly the given set of peers (regardless of
-- what each request is).
assertRequestPeers :: [Peer] -> Map.Map Peer (NESeq LeiosFetchRequest) -> IO ()
assertRequestPeers expected m = Map.keysSet m @?= Set.fromList expected

-- | Assert how many distinct peers received a request.
assertRequestPeerCount :: Int -> Map.Map Peer (NESeq LeiosFetchRequest) -> IO ()
assertRequestPeerCount n m = Map.size m @?= n

{-------------------------------------------------------------------------------
  Fixture helpers
-------------------------------------------------------------------------------}

-- | A 'LeiosPoint' with a slot and an EB hash derived from a Char,
-- so tests read `point 1 'a'` instead of long byte literals.
point :: Word -> Char -> LeiosPoint
point slot c = MkLeiosPoint (SlotNo (fromIntegral slot)) (eb c)

-- | Every offset the requests ask a peer for.
requestedOffsets :: Map.Map Peer (NESeq LeiosFetchRequest) -> IntSet.IntSet
requestedOffsets m =
  IntSet.unions
    [ offs
    | reqs <- Map.elems m
    , LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) <- toList reqs
    , Jobs.MkLeiosJob offs _ _ <- toList jobs
    ]

-- | Distinct tx hash from an offset.
txHashOf :: Int -> TxHash
txHashOf i = UnsafeHash $ SBS.pack $ replicate 32 (fromIntegral i)

-- | Distinct EB hash from a Char.
eb :: Char -> EndorserBlockHash
eb c = UnsafeHash $ SBS.pack $ replicate 32 (fromIntegral (fromEnum c))

-- | The bounds these scenarios forecast: large enough that nothing here trips
-- them.
anyBounds :: SlotNo -> Either OutsideForecastRange LeiosEbByteSizeBounds
anyBounds _slot = Right maxBound
