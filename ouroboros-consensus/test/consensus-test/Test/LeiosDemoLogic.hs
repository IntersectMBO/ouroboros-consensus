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
import qualified Data.DList as DList
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LeiosDemoLogic
  ( LeiosFetchDecisions (..)
  , leiosFetchLogicIteration
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosFetchStaticEnv (..)
  , LeiosOutstanding (..)
  , LeiosPoint (..)
  , PeerId (..)
  , demoLeiosFetchStaticEnv
  , emptyLeiosOutstanding
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
        , testCase "per-EB request cap blocks further selection" $
            test_bodyPerEbCap
        , testCase "offering peers selected up to the per-EB cap" $
            test_bodyTwoPeersOffer
        , testCase "global byte budget exhausted blocks further selection" $
            test_globalByteBudget
        , testCase "per-peer byte budget exhausted skips that peer" $
            test_perPeerByteBudget
        ]
    ]

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
    & offersBody peerA [eb 'a']
    & runIteration
    & assertBodyRequest peerA (point 1 'a') 1024

test_bodyNoOffer :: IO ()
test_bodyNoOffer =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [eb 'b'] -- peer offers a different EB, not the one we need
    & runIteration
    & assertNoRequests

test_bodyPerEbCap :: IO ()
test_bodyPerEbCap =
  empty
    & withMissingBody (point 1 'a') 1024
    & alreadyRequestedEbFrom (eb 'a') [0 .. ebCap - 1] -- the per-EB cap is used up
    & offersBody ebCap [eb 'a'] -- so this additional peer is not selected
    & runIteration
    & assertNoRequests
 where
  ebCap = maxRequestsPerEb demoLeiosFetchStaticEnv

test_bodyTwoPeersOffer :: IO ()
test_bodyTwoPeersOffer =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [eb 'a']
    & offersBody peerB [eb 'a']
    & runIteration
    -- two peers offer; the fetch logic selects up to the per-EB cap of them
    & assertRequestPeerCount (min 2 (maxRequestsPerEb demoLeiosFetchStaticEnv))

test_globalByteBudget :: IO ()
test_globalByteBudget =
  empty
    & withMissingBody (point 1 'a') 1024
    & withTotalRequestedBytes (maxRequestedBytesSize demoLeiosFetchStaticEnv)
    & offersBody peerA [eb 'a']
    & runIteration
    & assertNoRequests

test_perPeerByteBudget :: IO ()
test_perPeerByteBudget =
  empty
    & withMissingBody (point 1 'a') 1024
    & withRequestedBytesPerPeer
      peerA
      (maxRequestedBytesSizePerPeer demoLeiosFetchStaticEnv + 1)
    & offersBody peerA [eb 'a']
    & offersBody peerB [eb 'a']
    & runIteration
    & assertRequestPeers [peerB]

------------------------------------------------------------
-- Scenario DSL
------------------------------------------------------------

-- | A test fixture: static env, peer offerings, outstanding work.
data Scenario pid = Scenario
  { scEnv :: !LeiosFetchStaticEnv
  , scOfferings :: !(Map.Map (PeerId pid) (Set.Set EbHash, Set.Set EbHash))
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
withMissingBody p size =
  onOutstanding $ \o ->
    o{missingEbBodies = Map.insert p size (missingEbBodies o)}

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

-- | Set the global in-flight byte total. Use with the env's cap to
-- test the global byte budget.
withTotalRequestedBytes :: BytesSize -> Scenario pid -> Scenario pid
withTotalRequestedBytes n =
  onOutstanding $ \o -> o{requestedBytesSize = n}

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

-- | Peer @p@ offers the body of these EBs.
offersBody :: Ord pid => pid -> [EbHash] -> Scenario pid -> Scenario pid
offersBody pid ebs =
  insertOffering (MkPeerId pid) (Set.fromList ebs) Set.empty

insertOffering ::
  Ord pid =>
  PeerId pid ->
  Set.Set EbHash ->
  Set.Set EbHash ->
  Scenario pid ->
  Scenario pid
insertOffering pid bodies txs sc =
  sc
    { scOfferings =
        Map.insertWith
          (\(a, b) (c, d) -> (a <> c, b <> d))
          pid
          (bodies, txs)
          (scOfferings sc)
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
runIteration :: Ord pid => Scenario pid -> LeiosFetchDecisions pid
runIteration sc =
  -- A known current slot selects freshest-first (i.e. youngest-first), which is
  -- the ordering these scenarios were written against.
  snd $ leiosFetchLogicIteration sc.scEnv (Just minBound) sc.scOfferings sc.scOutstanding

------------------------------------------------------------
-- Assertions
------------------------------------------------------------

assertBodyRequest ::
  (Ord pid, Show pid) =>
  pid ->
  LeiosPoint ->
  BytesSize ->
  LeiosFetchDecisions pid ->
  IO ()
assertBodyRequest pid p size (MkLeiosFetchDecisions m) =
  case Map.lookup (MkPeerId pid) m of
    Nothing -> assertFailure $ "no request for peer " <> show pid
    Just slotMap -> case Map.lookup p.pointSlotNo slotMap of
      Nothing -> assertFailure "no request at expected slot"
      Just (_txs, bodies) ->
        DList.toList bodies @?= [(p.pointEbHash, size)]

assertNoRequests :: (Ord pid, Show pid) => LeiosFetchDecisions pid -> IO ()
assertNoRequests (MkLeiosFetchDecisions m) = Map.keys m @?= []

-- | Assert that the decision set has requests targeting exactly the
-- given set of peers (regardless of what each request is).
assertRequestPeers ::
  (Ord pid, Show pid) =>
  [pid] -> LeiosFetchDecisions pid -> IO ()
assertRequestPeers expected (MkLeiosFetchDecisions m) =
  Set.fromList (Map.keys m) @?= Set.fromList (map MkPeerId expected)

-- | Assert how many distinct peers received a request. Order-independent, so it
-- holds for any 'maxRequestsPerEb' \/ 'maxRequestsPerTx': at a cap below the
-- number of offering peers, /which/ peers win is a selection-order detail, but
-- the count is not.
assertRequestPeerCount :: Int -> LeiosFetchDecisions pid -> IO ()
assertRequestPeerCount n (MkLeiosFetchDecisions m) = Map.size m @?= n

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

