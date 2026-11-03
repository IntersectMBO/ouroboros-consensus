{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests of the LeiosFetch decision logic, on scenarios built directly from
-- its outstanding state and the peers' offers.
module Test.Consensus.Leios.LeiosFetch.Decision (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlockHash
  , LeiosEbSizes (..)
  , LeiosPoint (..)
  , TxHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbBounds (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , PeerOffer (MkPeerOffer)
  , WhetherTxsClosureOffered (..)
  , acquireEbBody
  , defaultLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , fetchPriorityTiers
  , focusCertifiedEb
  , focusElectionIfUnfocused
  , leiosFetchLogicIteration
  , recordReferencedPoint
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

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
            "an endorser block rejected as too big at a point is never fetched again at that point"
            test_noRefetchOversized
        , testCase "per-peer byte budget exhausted skips that peer" $
            test_perPeerByteBudget
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
  (hi, lo) = fetchPriorityTiers (Just (SlotNo 100)) 10 offers
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

-- | An endorser block rejected as too big, then offered at the same point at
-- bounds that admit its closure but not its body, at bounds that admit its body
-- but not its closure, and at bounds that admit both.
test_noRefetchOversized :: IO ()
test_noRefetchOversized = do
  let sc =
        empty
          & withMissingBody (point 1 'a') 1024
          & onOutstanding
            (acquireEbBody (point 1 'a') (MkLeiosEbSizes 1024 5000) Jobs.emptyLeiosJobPool)
          & offersBody peerA [point 1 'a']
      within maxBody maxTxs _slot = Right (MkLeiosEbBounds maxBody maxTxs)
  runIterationWith (within 1023 5000) sc & assertNoRequests
  runIterationWith (within 1024 4999) sc & assertNoRequests
  runIterationWith (within 1024 5000) sc & assertNoRequests

test_perPeerByteBudget :: IO ()
test_perPeerByteBudget =
  empty
    & withMissingBody (point 1 'a') 1024
    & withRequestedBytesPerPeer peerA (maxRequestedBytesSizePerPeer env + 1)
    & offersBody peerA [point 1 'a']
    & offersBody peerB [point 1 'a']
    & runIteration
    & assertRequestPeers [peerB]

test_focusCertifiedPruned :: IO ()
test_focusCertifiedPruned =
  (Map.size (elFocus outstanding), Map.size (ebState outstanding)) @?= (0, 0)
 where
  outstanding =
    focusCertifiedEb (ElectionId (SlotNo 10) fixtureElectee) (eb 'a') $
      emptyLeiosOutstanding (mkStdGen 0) (SlotNo 20)

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
  jobPool = Jobs.mkLeiosJobPool (maxJobBytesSize env) (maxJobTxCount env) misses
  -- The body is already held in these runs, so the offer names no size; only
  -- its closure half is consulted.
  offers = Map.singleton peerA $ Map.singleton p (MkPeerOffer SNothing SNothing TxsClosureOffered)
  ordinaryCap = maxRequestedBytesSizePerPeer env
  bigLedgerCap = maxRequestedBytesSizePerBigLedgerPeer env
  -- hold the body (so the pool is live), with the peer's in-flight bytes
  -- preloaded to 'used'
  run bigLedgerPeers used =
    let outstanding =
          (\o -> o{requestedBytesSizePerPeer = Map.singleton peerA used})
            -- the election fetching it, without which nothing is requested
            . focusElectionIfUnfocused (ElectionId (SlotNo 10) fixtureElectee) (eb 'a')
            . acquireEbBody p (MkLeiosEbSizes 1000 0) jobPool
            . recordReferencedPoint p SNothing
            $ emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
        (_o, reqs, _d) =
          leiosFetchLogicIteration
            env
            anyBounds
            (Just (SlotNo 11))
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
  pool0 = Jobs.mkLeiosJobPool 1000000 1 misses
  pickId pool excluded s =
    case Jobs.pickLeastRequestedJobExcept (mkStdGen s) excluded pool of
      Just (Jobs.MkLeiosJobId i, _job, _pool', _prng') -> Just i
      Nothing -> Nothing

{-------------------------------------------------------------------------------
  Scenario DSL
-------------------------------------------------------------------------------}

-- | A test fixture: peer offerings, outstanding work.
data Scenario = Scenario
  { scOfferings :: !(Map.Map Peer (Map.Map LeiosPoint WhetherTxsClosureOffered))
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
          Map.union
          peer
          (Map.fromList [(p, TxsClosureNotOffered) | p <- points])
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
  (SlotNo -> Either OutsideForecastRange LeiosEbBounds) ->
  Scenario ->
  Map.Map Peer (NESeq LeiosFetchRequest)
runIterationWith forecastEbBounds = runIterationWithAt forecastEbBounds minBound

-- | 'runIterationAt', with the ledger's bounds forecast thus.
runIterationWithAt ::
  (SlotNo -> Either OutsideForecastRange LeiosEbBounds) ->
  SlotNo ->
  Scenario ->
  Map.Map Peer (NESeq LeiosFetchRequest)
runIterationWithAt forecastEbBounds currentSlot sc =
  let (_out, reqs, _drops) =
        -- No big-ledger peers in these scenarios.
        leiosFetchLogicIteration
          env
          forecastEbBounds
          (Just currentSlot)
          (Map.map (Map.mapWithKey (resolveOfferSize sc.scOfferedSizes)) sc.scOfferings)
          Map.empty
          sc.scOutstanding
   in reqs

-- | Give a fixture's offer the size 'withMissingBody' said peers offer that
-- point at, which is the size the decision logic then requests. A point no
-- 'withMissingBody' named is offered with no size, which is the closure-only
-- case.
resolveOfferSize ::
  Map.Map LeiosPoint BytesSize -> LeiosPoint -> WhetherTxsClosureOffered -> PeerOffer
resolveOfferSize sizes p = MkPeerOffer SNothing (maybe SNothing SJust (Map.lookup p sizes))

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
anyBounds :: SlotNo -> Either OutsideForecastRange LeiosEbBounds
anyBounds _slot = Right maxBound
