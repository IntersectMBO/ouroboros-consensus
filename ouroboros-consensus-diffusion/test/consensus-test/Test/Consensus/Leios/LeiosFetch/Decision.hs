{-# LANGUAGE OverloadedRecordDot #-}

module Test.Consensus.Leios.LeiosFetch.Decision (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.NonEmpty as NEIntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( LeiosFetchClientContext (lfcOutstanding)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.Util.IOLike (readMVar)
import System.Random (mkStdGen)
import qualified Test.Consensus.Leios.LeiosFetch.Bookkeeping as Bookkeeping
import Test.Consensus.Leios.LeiosFetch.Replies (newContext)
import Test.Tasty
import Test.Tasty.HUnit

type Peer = Int

peerA, peerB :: Peer
peerA = 0
peerB = 1

point :: Word -> Char -> LeiosPoint
point slot tag =
  MkLeiosPoint
    (SlotNo (fromIntegral slot))
    (UnsafeHash (SBS.pack (replicate 32 (fromIntegral (fromEnum tag)))))

txHashOf :: Int -> TxHash
txHashOf n = hashLeiosTx (MkLeiosTx (BS.singleton (fromIntegral n)))

tests :: TestTree
tests =
  testGroup
    "decision semantics"
    [ testCase
        "confirmed jobs complete; missing jobs release only their own picks"
        test_settlePartialReceipt
    , testCase "partial indivisible job remains requestable in full" test_settlePartialJob
    , testCase "late missing receipt cannot release another peer's pick" test_settleRemovedPeer
    , testCase
        "late confirmation completes globally without restoring departed owner"
        test_settleRemovedPeerConfirmed
    , testCase "random least multiplicity drawing retains exclusions and reverse indices" test_pickJobs
    , testCase "consensus voting tiers precede freshest stale work" test_fetchPriorityOrder
    , testCase "priority window arithmetic does not wrap at the maximum slot" $
        fetchPriorityTiers (Just (maxBound, maxBound)) [(maxBound, ())] @?= ([()], [])
    , testCase "verified certificate refocuses an election and pruning cannot resurrect it" $ do
        (ctx, _) <- newContext
        initial <- readMVar (lfcOutstanding ctx)
        let (election, oldHash) = Map.findMin (elFocus initial)
            other = pointEbHash (point 0 'z')
            attempted = focusElectionIfUnfocused election other initial
            certified = focusCertifiedEb election other attempted
            ElectionId slot _ = election
            retired = pruneOutstandingToImmTip (slot + 1) certified
        Map.lookup election (elFocus attempted) @?= Just oldHash
        Map.lookup election (elFocus certified) @?= Just other
        Map.member (MkLeiosPoint slot other) (ebState certified) @?= True
        Map.lookup election (elFocus (focusCertifiedEb election oldHash retired)) @?= Nothing
    , Bookkeeping.tests
    ]

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

-- | Build one request per peer for the same two offsets. A count limit of one
-- gives two independent jobs; two gives one job containing both offsets.
settlementFixture :: Int -> [Peer] -> (LeiosPoint, LeiosBlockTxsRequest, LeiosOutstanding Peer)
settlementFixture maxCount peers = (p, MkLeiosBlockTxsRequest p requestJobs, outstanding)
 where
  p = point 5 'r'
  initial = settlementBase maxCount
  jobIds = [0 .. if maxCount == 1 then 1 else 0]
  jobMap =
    IntMap.fromList
      [ (jid, fromMaybe (error "missing fixture job") (Jobs.lookupJob (Jobs.MkLeiosJobId jid) initial))
      | jid <- jobIds
      ]
  requestJobs =
    fromMaybe (error "empty settlement fixture") $
      NEIntMap.nonEmptyMap $
        jobMap
  ids =
    fromMaybe (error "empty settlement fixture ids") $
      NEIntSet.nonEmptySet $
        IntMap.keysSet jobMap
  pick pool jid =
    case Jobs.pickLeastRequestedJobExcept (mkStdGen 0) (IntSet.delete jid (NEIntSet.toSet ids)) pool of
      Just (_, _, next, _) -> next
      Nothing -> error "could not pick fixture job"
  picked = foldl (\pool _ -> foldl pick pool jobIds) initial peers
  outstanding =
    (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
      { ebState = Map.singleton p (MkEbState SNothing (BodyAcquired picked))
      , requestedJobsPerPeer = Map.fromList [(ppeer, Map.singleton p ids) | ppeer <- peers]
      }

settlementBase :: Int -> Jobs.LeiosJobPool
settlementBase maxCount =
  Jobs.mkJobPool 1000000 maxCount $
    IntMap.fromList [(off, (txHashOf off, 100)) | off <- [0, 1]]

settledPool :: LeiosPoint -> LeiosOutstanding Peer -> Jobs.LeiosJobPool
settledPool p o = case Map.lookup p o.ebState of
  Just (MkEbState _ (BodyAcquired pool)) -> pool
  _ -> error "settlement lost its body"

test_settlePartialReceipt :: IO ()
test_settlePartialReceipt = do
  let (p, req, initial) = settlementFixture 1 [peerA]
      settled = settleTxRequest peerA req (IntSet.singleton 0) initial
      pool = settledPool p settled
  Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool @?= Nothing
  pool @?= Jobs.completeJob (Jobs.MkLeiosJobId 0) (settlementBase 1)
  Map.null settled.requestedJobsPerPeer @?= True
  -- Settling again must not release a pick belonging to another request.
  settledPool p (settleTxRequest peerA req IntSet.empty settled) @?= pool

test_settlePartialJob :: IO ()
test_settlePartialJob = do
  let (p, req, initial) = settlementFixture 2 [peerA]
      settled = settleTxRequest peerA req (IntSet.singleton 0) initial
      pool = settledPool p settled
  pool @?= settlementBase 2
  Map.null settled.requestedJobsPerPeer @?= True

test_settleRemovedPeer :: IO ()
test_settleRemovedPeer = do
  let (p, req, initial) = settlementFixture 1 [peerA, peerB]
      removed = removePeerFromOutstanding peerA initial
      settled = settleTxRequest peerA req IntSet.empty removed
  settledPool p settled @?= settledPool p removed
  let (_, _, onePeer) = settlementFixture 1 [peerB]
  settledPool p settled @?= settledPool p onePeer
  settled.requestedJobsPerPeer @?= removed.requestedJobsPerPeer

test_settleRemovedPeerConfirmed :: IO ()
test_settleRemovedPeerConfirmed = do
  let (p, req, initial) = settlementFixture 1 [peerA, peerB]
      removed = removePeerFromOutstanding peerA initial
      settled = settleTxRequest peerA req (IntSet.fromList [0, 1]) removed
  Jobs.nullJobPool (settledPool p settled) @?= True
  Map.member peerA settled.requestedJobsPerPeer @?= False
  Map.member peerB settled.requestedJobsPerPeer @?= True
