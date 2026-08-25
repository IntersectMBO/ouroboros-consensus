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
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import LeiosDemoLogic (fetchPriorityTiers, leiosFetchLogicIteration)
import LeiosDemoTypes
  ( AlsoOfferedTxsClosure (..)
  , BytesSize
  , EbHash (..)
  , LeiosBlockRequest (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , LeiosPoint (..)
  , PeerId (..)
  , demoLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , markBodyImminent
  , mergeOffer
  , recordMaxAnnouncementSlot
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
    ]

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
  , scOfferings :: !(Map.Map (PeerId pid) (Map.Map LeiosPoint AlsoOfferedTxsClosure))
  , scOutstanding :: !(LeiosOutstanding pid)
  }

empty :: Scenario pid
empty =
  Scenario
    { scEnv = demoLeiosFetchStaticEnv
    , scOfferings = Map.empty
    , scOutstanding = emptyLeiosOutstanding (SlotNo 0)
    }

-- | Outstanding-work combinators -----------------------------------------
withMissingBody :: LeiosPoint -> BytesSize -> Scenario pid -> Scenario pid
withMissingBody p@(MkLeiosPoint slot ebHash) size =
  onOutstanding $ \o ->
    -- Seed everything the announce path would: the missing-body point and its
    -- reverse index, plus (via 'recordMaxAnnouncementSlot') the 'ebState' NoBody
    -- entry that the fetch loop now drives bodies off of.
    recordMaxAnnouncementSlot ebHash slot $
      o
        { missingEbBodies = Map.insert p size (missingEbBodies o)
        , reverseSlotIndexByEbHash =
            Map.insertWith NESet.union ebHash (NESet.singleton slot) (reverseSlotIndexByEbHash o)
        }

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
  insertOffering (MkPeerId pid) (Map.fromList [(p, TxsClosureNotAlsoOffered) | p <- points])

-- | Peer @p@ offers both the body and the tx-closure of these points.
offersBodyAndClosure :: Ord pid => pid -> [LeiosPoint] -> Scenario pid -> Scenario pid
offersBodyAndClosure pid points =
  insertOffering (MkPeerId pid) (Map.fromList [(p, TxsClosureAlsoOffered) | p <- points])

insertOffering ::
  Ord pid =>
  PeerId pid ->
  Map.Map LeiosPoint AlsoOfferedTxsClosure ->
  Scenario pid ->
  Scenario pid
insertOffering pid offers sc =
  sc
    { scOfferings =
        Map.insertWith (Map.unionWith mergeOffer) pid offers (scOfferings sc)
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
        leiosFetchLogicIteration sc.scEnv (Just minBound) sc.scOfferings Map.empty sc.scOutstanding
   in reqs

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
      | LeiosBlockRequest (MkLeiosBlockRequest pt sz) <- toList reqs
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
eb c = MkEbHash $ BS.pack $ replicate 32 (fromIntegral (fromEnum c))

