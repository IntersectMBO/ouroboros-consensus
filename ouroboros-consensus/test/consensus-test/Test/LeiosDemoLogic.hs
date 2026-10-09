{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for 'leiosFetchLogicIteration', the pure decision
-- function at the heart of the Leios fetch loop.
--
-- Each test is a small data fixture built via the 'Scenario' DSL
-- below: start from an 'empty' scenario, layer in missing work
-- ('withMissingBody') and peer offers
-- ('offersBody'), then run and assert on the resulting
-- decisions. The DSL chains via '&' (`x & f = f x`), keeping each
-- test ~5 lines so the scenario reads top-to-bottom.
--
-- The point of testing the pure function directly (not the
-- NodeKernel wrapper) is that the failure modes attributed to the
-- fetch logic — size-match predicate, lifetime cap, peer-key
-- consistency — all live in this function or one ply below it.
-- Reproducing them as data fixtures pins which predicate is firing,
-- instead of guessing.
module Test.LeiosDemoLogic (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Exception (try)
import Control.Monad (void)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( LeiosDbWriter (..)
  , Promise (..)
  , newLeiosDBInMemory
  , withReader
  , withWriter
  )
import LeiosDemoException (LeiosDbException)
import LeiosDemoLogic
  ( ClosureOfferRejection (..)
  , admitClosureOffer
  , fetchPriorityTiers
  , leiosFetchLogicIteration
  , msgLeiosBlockRequest
  , newLeiosFetchContext
  )
import LeiosDemoLogic.Announcements.ElBimap (ElId (MkElId))
import LeiosDemoTypes
  ( BytesSize
  , ClosureOffer (..)
  , EbHash (..)
  , LeiosBlockRequest (..)
  , LeiosEb (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , PeerId (..)
  , PeerOffer (MkPeerOffer)
  , demoLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , encodeLeiosEbSize
  , focusElectionIfUnfocused
  , hashLeiosEb
  , hashLeiosTx
  , markBodyImminent
  , maxTxsPerEb
  , recordMaxAnnouncementSlot
  , wholeClosureOffer
  )
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Util.LeiosHash (unsafeEbHashFromBytes)

tests :: TestTree
tests =
  testGroup
    "LeiosDemoLogic"
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
        , testCase "per-peer byte budget exhausted skips that peer" $
            test_perPeerByteBudget
        ]
    , testGroup
        "self-forged EB"
        [ testCase "an offer of a self-forged EB is never re-fetched" $
            test_forgedEbOfferIgnored
        ]
    , testGroup
        "fetch priority"
        [ testCase "freshest window oldest-first, then rest freshest-first" $
            test_fetchPriorityOrder
        ]
    , testGroup
        "msgLeiosBlockRequest"
        [ testCase "serves a stored body of maxTxsPerEb entries" $
            test_serveBodyAtLimit
        , testCase "refuses a stored body of more than maxTxsPerEb entries" $
            test_refuseBodyOverLimit
        ]
    , testGroup
        "closure offer admission"
        [ testCase "ranges are non-empty, long enough, within the closure and disjoint" $
            test_closureOfferAdmission
        ]
    ]

-- | 'admitClosureOffer': a range must be non-empty, start within the largest
-- closure, span the minimum unless it runs to the end, and not overlap what the
-- peer already offered; adjacent ranges merge.
test_closureOfferAdmission :: IO ()
test_closureOfferAdmission = do
  let env = demoLeiosFetchStaticEnv{maxEbClosureBytesSize = 1000}
      minLength = 100
      step start end = admitClosureOffer env minLength start end
      admitted r = case r of
        Right offered -> pure offered
        Left why -> assertFailure ("offer rejected: " <> show why)
      rejected why r = case r of
        Right _ -> assertFailure "offer admitted"
        Left why' -> why' @?= why
  -- an empty or inverted range is invalid, and so is one shorter than the minimum
  rejected ClosureRangeInvalid (step 100 100 mempty)
  rejected ClosureRangeInvalid (step 200 100 mempty)
  rejected ClosureRangeInvalid (step 0 99 mempty)
  b1 <- admitted (step 0 100 mempty)
  -- overlapping an earlier range is invalid, a repeat included
  rejected ClosureRangeOverlaps (step 50 200 b1)
  rejected ClosureRangeOverlaps (step 0 100 b1)
  -- adjacent ranges merge
  b2 <- admitted (step 100 250 b1)
  b2 @?= MkClosureOffer (Map.singleton 0 250)
  -- a range to the end of the closure is exempt from the minimum ...
  b3 <- admitted (step 300 maxBound b2)
  -- ... and nothing may overlap it either
  rejected ClosureRangeOverlaps (step 500 600 b3)
  -- a range starting at or past the most a closure can hold is invalid, even to the end
  rejected ClosureRangeInvalid (step 1000 maxBound mempty)
  _ <- admitted (step 999 maxBound mempty)
  pure ()

-- | With current slot S=100 and window L=10, EBs at slot >= 90 (the voting
-- window) are prioritised oldest-first, EBs beyond S trail that first tier, and
-- everything older is freshest-first.
test_fetchPriorityOrder :: IO ()
test_fetchPriorityOrder =
  map (unSlot . (.pointSlotNo) . fst) (hi ++ lo)
    @?= [90, 95, 100, 101, 105, 89, 50, 0]
 where
  (hi, lo) = fetchPriorityTiers (Just (SlotNo 100)) 10 offers
  offers = Map.fromList [(point a 'x', ()) | a <- [0, 105, 90, 50, 100, 89, 101, 95]]
  unSlot (SlotNo n) = n

------------------------------------------------------------
-- Scenarios
------------------------------------------------------------

-- | Named peer IDs so tests don't pepper themselves with type
-- annotations on integer literals. The numeric value carries no
-- meaning beyond identity.
type Pid = Int

peerA, peerB, _peerC :: Pid
peerA = 0
peerB = 1
_peerC = 2

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
    & alreadyRequestedEbFrom (eb 'a') [peerA] -- already in flight from peerA
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

test_perPeerByteBudget :: IO ()
test_perPeerByteBudget =
  empty
    & withMissingBody (point 1 'a') 1024
    & withRequestedBytesPerPeer
      peerA
      (maxRequestedBytesSizePerPeer demoLeiosFetchStaticEnv + 1)
    & offersBody peerA [point 1 'a']
    & offersBody peerB [point 1 'a']
    & runIteration
    & assertRequestPeers [peerB]

-- | Regression for the devnet crash where a node re-fetched an EB it had just
-- forged: the redundant closure acquisition emitted a second 'AcquiredEbTxs',
-- which 'runLeiosVoting' rejected ('AlreadyKnown') and died on. The forge marks
-- the EB in 'ebState' (as 'BodyImminent'), so the fetch logic must drop any peer
-- offer of it -- even a full body+closure offer -- rather than request it.
test_forgedEbOfferIgnored :: IO ()
test_forgedEbOfferIgnored =
  empty
    & withForgedEb (point 1 'a')
    & offersBodyAndClosure peerA [point 1 'a']
    & runIteration
    & assertNoRequests

------------------------------------------------------------
-- Scenario DSL
------------------------------------------------------------

-- | A test fixture: static env, peer offerings, outstanding work.
data Scenario pid = Scenario
  { scEnv :: !LeiosFetchStaticEnv
  , scOfferings :: !(Map.Map (PeerId pid) (Map.Map LeiosPoint ClosureOffer))
  , scOfferedSizes :: !(Map.Map LeiosPoint BytesSize)
  -- ^ The size each point is offered at.
  --
  -- NOTE a peculiarity of this harness: every peer offering a point offers it
  -- at the same size, the one 'withMissingBody' named. The protocol does not
  -- work that way --- each offer carries its own size, and 'assignBody' asks
  -- for whatever the peer it is asking claimed --- so no scenario here can have
  -- two peers disagree about a size. Nothing these tests cover turns on that.
  --
  -- It is a field of its own because the size is the offering peer's claim
  -- rather than anything the outstanding state holds, so the fixture must
  -- supply it.
  , scOutstanding :: !(LeiosOutstanding pid)
  }

empty :: Scenario pid
empty =
  Scenario
    { scEnv = demoLeiosFetchStaticEnv
    , scOfferings = Map.empty
    , scOfferedSizes = Map.empty
    , scOutstanding = emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
    }

-- | Outstanding-work combinators -----------------------------------------

-- | The body of this point is still to fetch, and every peer that offers it
-- offers it at this size.
withMissingBody :: LeiosPoint -> BytesSize -> Scenario pid -> Scenario pid
withMissingBody p@(MkLeiosPoint slot ebHash) size sc =
  onOutstanding seed sc{scOfferedSizes = Map.insert p size (scOfferedSizes sc)}
 where
  -- Seed what the announce path would: the 'ebState' NoBody entry the fetch
  -- loop drives bodies off of, and the election that is fetching it, without
  -- which the loop asks no one for it. Only a certificate overrules an election
  -- already fetching something, so this seeds it the way an announcement would.
  seed =
    focusElectionIfUnfocused (MkElId slot fixtureIssuer) ebHash
      . recordMaxAnnouncementSlot ebHash slot SNothing

-- | The one pool a scenario here pretends announced everything, since these
-- have no headers to take an issuer from. An endorser block's election is
-- therefore just its slot, so two announced in one slot share an election and
-- only the first is fetched.
fixtureIssuer :: ShortByteString
fixtureIssuer = SBS.pack [0]

-- | Mark an EB as one our own forge produced -- the 'BodyImminent' 'ebState'
-- entry that 'onForgedLeiosEb' installs at announcement time.
withForgedEb :: LeiosPoint -> Scenario pid -> Scenario pid
withForgedEb (MkLeiosPoint slot ebHash) =
  onOutstanding $ markBodyImminent ebHash slot

alreadyRequestedEbFrom :: Ord pid => EbHash -> [pid] -> Scenario pid -> Scenario pid
alreadyRequestedEbFrom ebHash pids =
  onOutstanding $ \o ->
    o
      { requestedEbPeers =
          Map.insertWith
            Set.union
            ebHash
            (Set.fromList (map MkPeerId pids))
            (requestedEbPeers o)
      }

-- | Set a per-peer in-flight byte total. Use with the env's per-peer
-- cap to test the per-peer byte budget.
withRequestedBytesPerPeer ::
  Ord pid => pid -> BytesSize -> Scenario pid -> Scenario pid
withRequestedBytesPerPeer pid n =
  onOutstanding $ \o ->
    o
      { requestedBytesSizePerPeer =
          Map.insert (MkPeerId pid) n (requestedBytesSizePerPeer o)
      }

-- | Per-peer offer combinators -------------------------------------------

-- | Peer @p@ offers the body (only) of these points.
offersBody :: Ord pid => pid -> [LeiosPoint] -> Scenario pid -> Scenario pid
offersBody pid points =
  insertOffering (MkPeerId pid) (Map.fromList [(p, mempty) | p <- points])

-- | Peer @p@ offers both the body and the tx-closure of these points.
offersBodyAndClosure :: Ord pid => pid -> [LeiosPoint] -> Scenario pid -> Scenario pid
offersBodyAndClosure pid points =
  insertOffering (MkPeerId pid) (Map.fromList [(p, wholeClosureOffer) | p <- points])

insertOffering ::
  Ord pid =>
  PeerId pid ->
  Map.Map LeiosPoint ClosureOffer ->
  Scenario pid ->
  Scenario pid
insertOffering pid offers sc =
  sc
    { scOfferings =
        Map.insertWith (Map.unionWith (<>)) pid offers (scOfferings sc)
    }

-- | Internal: lift a function on 'LeiosOutstanding' to one on 'Scenario'.
onOutstanding ::
  (LeiosOutstanding pid -> LeiosOutstanding pid) ->
  Scenario pid ->
  Scenario pid
onOutstanding f sc = sc{scOutstanding = f (scOutstanding sc)}

-- | Run the iteration and project the decisions.
--
-- (The tx side of the fetch logic is disarmed, so only EB-body requests are
-- emitted; the old tx-soundness check is gone with it.)
runIteration :: Ord pid => Scenario pid -> Map.Map (PeerId pid) (NESeq LeiosFetchRequest)
runIteration sc =
  -- Any known current slot suffices here: these scenarios don't depend on the
  -- offer-visit order (the priority order is tested by 'test_fetchPriorityOrder').
  let (_out, reqs, _drops) =
        -- No big-ledger peers in these scenarios (the aggressive-fetch path is
        -- exercised in "Test.LeiosDemoLogic.Invariants").
        leiosFetchLogicIteration
          sc.scEnv
          anyClosureSize
          (Just minBound)
          (Map.map (Map.mapWithKey (resolveOfferSize sc.scOfferedSizes)) sc.scOfferings)
          Map.empty
          sc.scOutstanding
   in reqs

-- | Give a fixture's offer the size 'withMissingBody' said peers offer that
-- point at, which is the size 'assignBody' then requests. A point no
-- 'withMissingBody' named is offered with no size, which is the closure-only
-- case.
resolveOfferSize ::
  Map.Map LeiosPoint BytesSize -> LeiosPoint -> ClosureOffer -> PeerOffer
resolveOfferSize sizes p closure =
  MkPeerOffer SNothing (maybe SNothing SJust (Map.lookup p sizes)) closure

------------------------------------------------------------
-- Assertions
------------------------------------------------------------

assertBodyRequest ::
  (Ord pid, Show pid) =>
  pid ->
  LeiosPoint ->
  BytesSize ->
  Map.Map (PeerId pid) (NESeq LeiosFetchRequest) ->
  IO ()
assertBodyRequest pid p size m =
  case Map.lookup (MkPeerId pid) m of
    Nothing -> assertFailure $ "no request for peer " <> show pid
    Just reqs ->
      [ (pt.pointEbHash, sz)
      | LeiosBlockRequest (MkLeiosBlockRequest pt sz _) <- toList reqs
      ]
        @?= [(p.pointEbHash, size)]

assertNoRequests :: (Ord pid, Show pid) => Map.Map (PeerId pid) (NESeq LeiosFetchRequest) -> IO ()
assertNoRequests m = Map.keys m @?= []

-- | Assert that requests target exactly the given set of peers (regardless of
-- what each request is).
assertRequestPeers ::
  (Ord pid, Show pid) =>
  [pid] -> Map.Map (PeerId pid) (NESeq LeiosFetchRequest) -> IO ()
assertRequestPeers expected m =
  Set.fromList (Map.keys m) @?= Set.fromList (map MkPeerId expected)

-- | Assert how many distinct peers received a request.
assertRequestPeerCount :: Int -> Map.Map (PeerId pid) (NESeq LeiosFetchRequest) -> IO ()
assertRequestPeerCount n m = Map.size m @?= n

------------------------------------------------------------
-- Fixture helpers
------------------------------------------------------------

-- | A 'LeiosPoint' with a slot and an EB hash derived from a Char,
-- so tests read `point 1 'a'` instead of long byte literals.
point :: Word -> Char -> LeiosPoint
point slot c = MkLeiosPoint (SlotNo (fromIntegral slot)) (eb c)

-- | Distinct EB hash from a Char.
eb :: Char -> EbHash
eb c = unsafeEbHashFromBytes $ BS.pack $ replicate 32 (fromIntegral (fromEnum c))

-- | A body of exactly 'maxTxsPerEb' entries fits the server buffer.
test_serveBodyAtLimit :: IO ()
test_serveBodyAtLimit = do
  served <- serveStoredBody maxTxsPerEb
  V.length (leiosEbTxs served) @?= maxTxsPerEb

-- | A database written by an older build can hold more rows than the server
-- buffer has room for. The server must refuse with a 'LeiosDbException'.
test_refuseBodyOverLimit :: IO ()
test_refuseBodyOverLimit = do
  result <- try (serveStoredBody (maxTxsPerEb + 1))
  case result of
    Left (_ :: LeiosDbException) -> pure ()
    Right _ -> assertFailure "served a body with more entries than maxTxsPerEb"

-- | Store a body of @n@ distinct entries in an in-memory LeiosDb, then request
-- it from the Fetch server. The in-memory writer does not check the entry
-- count, so it can stand for rows that an older build wrote.
serveStoredBody :: Int -> IO LeiosEb
serveStoredBody n = do
  db <- newLeiosDBInMemory
  let body = MkLeiosEb $ V.generate n $ \i -> (hashLeiosTx (MkLeiosTx (BS8.pack (show i))), 100)
      bodyPoint = MkLeiosPoint (SlotNo 0) (hashLeiosEb body)
  -- The point first: a body can only be written for a registered point.
  withWriter db $ \w -> do
    void . await =<< writeEbPoint w bodyPoint (encodeLeiosEbSize body)
    void . await =<< writeEbBody w bodyPoint body []
  withReader db $ \r -> do
    ctx <- newLeiosFetchContext r
    msgLeiosBlockRequest nullTracer ctx bodyPoint

-- | The closure bound these scenarios forecast: large enough that nothing here
-- trips it. 'Test.Consensus.Leios.RecoveryPath' is where the bound itself is
-- exercised, against a real node.
anyClosureSize :: SlotNo -> Either OutsideForecastRange BytesSize
anyClosureSize _slot = Right maxBound
