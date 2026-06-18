{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for 'leiosFetchLogicIteration', the pure decision
-- function at the heart of the Leios fetch loop.
--
-- Each test is a small data fixture built via the 'Scenario' DSL
-- below: start from an 'empty' scenario, layer in missing work
-- ('withMissingBody', 'withMissingTx') and peer offers
-- ('offersBody', 'offersTxs'), then run and assert on the resulting
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
import qualified Data.IntMap.Strict as IntMap
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
  , TxHash (..)
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
        , testCase "two offering peers both selected (up to cap)" $
            test_bodyTwoPeersOffer
        , testCase "global byte budget exhausted blocks further selection" $
            test_globalByteBudget
        , testCase "per-peer byte budget exhausted skips that peer" $
            test_perPeerByteBudget
        ]
    , testGroup
        "EB tx fetch"
        [ testCase "single missing tx with one offering peer issues one request" $
            test_singleMissingTx
        , testCase "per-tx request cap blocks further selection" $
            test_txPerTxCap
        , testCase "tx referenced in multiple EBs, peer offering only target is selected" $
            test_txTwoEbsSinglePeerOffer
        , testCase "tx referenced in two EBs of different recorded size, peer offering both is selected" $
            test_txTwoEbsDifferentSize
        ]
    ]

------------------------------------------------------------
-- Scenarios
------------------------------------------------------------

-- | Named peer IDs so tests don't pepper themselves with type
-- annotations on integer literals. The numeric value carries no
-- meaning beyond identity.
type Pid = Int

peerA, peerB, peerC :: Pid
peerA = 0
peerB = 1
peerC = 2

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
    & offersTxs peerA [eb 'a'] -- offers tx-closure, not the body
    & runIteration
    & assertNoRequests

test_bodyPerEbCap :: IO ()
test_bodyPerEbCap =
  empty
    & withMissingBody (point 1 'a') 1024
    & alreadyRequestedEbFrom (eb 'a') [peerA, peerB] -- default cap = 2
    & offersBody peerC [eb 'a']
    & runIteration
    & assertNoRequests

test_singleMissingTx :: IO ()
test_singleMissingTx =
  empty
    & withMissingTx (point 1 'a') 0 (tx 'x') 500
    & offersTxs peerA [eb 'a']
    & runIteration
    & assertTxRequest peerA (point 1 'a') (tx 'x')

test_txPerTxCap :: IO ()
test_txPerTxCap =
  empty
    & withMissingTx (point 1 'a') 0 (tx 'x') 500
    & alreadyRequestedTxFrom (tx 'x') [peerA, peerB] -- default cap = 2
    & offersTxs peerC [eb 'a']
    & runIteration
    & assertNoRequests

test_bodyTwoPeersOffer :: IO ()
test_bodyTwoPeersOffer =
  empty
    & withMissingBody (point 1 'a') 1024
    & offersBody peerA [eb 'a']
    & offersBody peerB [eb 'a']
    & runIteration
    & assertRequestPeers [peerA, peerB]

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

-- | Tx X is referenced in two EBs (A and B), but the peer only
-- offers the tx-closure of EB A (the target). Expected: request
-- targets the peer with offset taken from A's entry.
test_txTwoEbsSinglePeerOffer :: IO ()
test_txTwoEbsSinglePeerOffer =
  empty
    & withMissingTx (point 1 'a') 0 (tx 'x') 100
    & alsoReferencedInEb (tx 'x') (eb 'b') 7 100 -- same recorded size in B
    & offersTxs peerA [eb 'a']
    & runIteration
    & assertTxRequest peerA (point 1 'a') (tx 'x')

-- | Tx X is referenced in EB A (size 100) and EB B (size 200) —
-- inconsistent sizes for the same content hash, e.g. one peer
-- delivered a malformed body for the other EB before the cap on
-- size-validation was tightened. Peer P offers the tx-closure of
-- BOTH. The gate must still issue the request: the target EB is A,
-- the recorded size on A is the authority, and B's stale/wrong
-- entry must not veto the selection.
--
-- Today the size predicate at 'choosePeerTx' (LeiosDemoLogic.hs:403)
-- uses @Map.lookupMax txOffsets'@, which picks whichever EB hash is
-- bytewise-larger; on disagreement the request is silently dropped.
test_txTwoEbsDifferentSize :: IO ()
test_txTwoEbsDifferentSize =
  empty
    & withMissingTx (point 1 'a') 0 (tx 'x') 100
    & alsoReferencedInEb (tx 'x') (eb 'b') 7 200 -- different recorded size
    & offersTxs peerA [eb 'a', eb 'b']
    & runIteration
    & assertTxRequest peerA (point 1 'a') (tx 'x')

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
    , scOutstanding = emptyLeiosOutstanding
    }

-- | Outstanding-work combinators -----------------------------------------
withMissingBody :: LeiosPoint -> BytesSize -> Scenario pid -> Scenario pid
withMissingBody p size =
  onOutstanding $ \o ->
    o{missingEbBodies = Map.insert p size (missingEbBodies o)}

withMissingTx ::
  LeiosPoint ->
  Int -> -- offset within the EB
  TxHash ->
  BytesSize ->
  Scenario pid ->
  Scenario pid
withMissingTx p offset h size =
  onOutstanding $ \o ->
    o
      { missingEbTxs =
          Map.insertWith
            IntMap.union
            p
            (IntMap.singleton offset (h, size))
            (missingEbTxs o)
      , reverseEbIndexByTx =
          Map.insertWith
            Map.union
            h
            (Map.singleton p.pointEbHash (offset, size))
            (reverseEbIndexByTx o)
      }

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

alreadyRequestedTxFrom :: Ord pid => TxHash -> [pid] -> Scenario pid -> Scenario pid
alreadyRequestedTxFrom txHash pids =
  onOutstanding $ \o ->
    o
      { requestedTxPeers =
          Map.insertWith
            Set.union
            txHash
            (Set.fromList (map MkPeerId pids))
            (requestedTxPeers o)
      }

-- | Tag a tx as also referenced by another EB at the given offset
-- and recorded size, without adding the EB to 'missingEbTxs'.
-- 'choosePeerTx' consults 'reverseEbIndexByTx' for "which EBs does
-- this tx appear in?" when evaluating peer offerings; this helper
-- lets us seed that cross-reference.
alsoReferencedInEb ::
  TxHash -> EbHash -> Int -> BytesSize -> Scenario pid -> Scenario pid
alsoReferencedInEb txHash ebHash offset size =
  onOutstanding $ \o ->
    o
      { reverseEbIndexByTx =
          Map.insertWith
            Map.union
            txHash
            (Map.singleton ebHash (offset, size))
            (reverseEbIndexByTx o)
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

-- | Peer @p@ offers the tx-closure of these EBs.
offersTxs :: Ord pid => pid -> [EbHash] -> Scenario pid -> Scenario pid
offersTxs pid ebs =
  insertOffering (MkPeerId pid) Set.empty (Set.fromList ebs)

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
runIteration :: Ord pid => Scenario pid -> LeiosFetchDecisions pid
runIteration sc =
  snd $ leiosFetchLogicIteration sc.scEnv sc.scOfferings sc.scOutstanding

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

assertTxRequest ::
  (Ord pid, Show pid) =>
  pid ->
  LeiosPoint ->
  TxHash ->
  LeiosFetchDecisions pid ->
  IO ()
assertTxRequest pid p txHash (MkLeiosFetchDecisions m) =
  case Map.lookup (MkPeerId pid) m of
    Nothing -> assertFailure $ "no request for peer " <> show pid
    Just slotMap -> case Map.lookup p.pointSlotNo slotMap of
      Nothing -> assertFailure "no request at expected slot"
      Just (txs, _bodies) -> case DList.toList txs of
        [(h, _size, _offsets)] -> h @?= txHash
        xs -> assertFailure $ "expected one tx request, got " <> show (length xs)

assertNoRequests :: (Ord pid, Show pid) => LeiosFetchDecisions pid -> IO ()
assertNoRequests (MkLeiosFetchDecisions m) = Map.keys m @?= []

-- | Assert that the decision set has requests targeting exactly the
-- given set of peers (regardless of what each request is).
assertRequestPeers ::
  (Ord pid, Show pid) =>
  [pid] -> LeiosFetchDecisions pid -> IO ()
assertRequestPeers expected (MkLeiosFetchDecisions m) =
  Set.fromList (Map.keys m) @?= Set.fromList (map MkPeerId expected)

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

-- | Distinct tx hash from a Char.
tx :: Char -> TxHash
tx c = MkTxHash $ BS.pack $ replicate 32 (fromIntegral (fromEnum c))
