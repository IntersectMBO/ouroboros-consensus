module Test.Consensus.Leios.LeiosFetch.RequestState (tests) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "request credit"
    [ testCase "valid defaults and fatal-guard validation" $ do
        validateLeiosFetchStaticEnv 60000000 candidateEnv @?= Right candidateEnv
        assertBool
          "zero rate"
          (isLeft (validateLeiosFetchStaticEnv 60000000 candidateEnv{minimumServiceRate = 0}))
        assertBool
          "fatal guard"
          (isLeft (validateLeiosFetchStaticEnv 60000000 candidateEnv{minimumServiceRate = 80000}))
        assertBool
          "undersized credit"
          (isLeft (validateLeiosFetchStaticEnv 60000000 candidateEnv{maxRequestedBytesSizePerPeer = 16000}))
    , testCase "legal indivisible transaction and conservative framing" $ do
        let small = candidateEnv{maxJobBytesSize = 1000, maxRequestedBytesSizePerPeer = 32768}
        validateLeiosFetchStaticEnv 60000000 small @?= Right small
        assertBool
          "legal maximum transaction must fit"
          (isLeft (validateLeiosFetchStaticEnv 60000000 small{maxRequestedBytesSizePerPeer = 32767}))
        assertBool
          "encoding and zero-byte TX count need ingress headroom"
          (isLeft (validateLeiosFetchStaticEnv 1000000 small))
        st <-
          requireRight
            (reserve small False (AttemptKey (0 :: Int) 0 0) (Lease (Job (0 :: Int) 0) 32768) emptyLedger)
        usage 0 st @?= Usage 32768 0
    , testCase "whole-job credit fits exactly and cannot overshoot" $ do
        let add acc i =
              acc
                >>= reserve
                  candidateEnv
                  False
                  (AttemptKey (0 :: Int) 0 i)
                  (Lease (Job (0 :: Int) (fromIntegral i)) 65536)
        s <- requireRight (foldl add (Right emptyLedger) [0 .. 3])
        usage 0 s @?= Usage 262144 0
        reserve candidateEnv False (AttemptKey 0 0 4) (Lease (Job 0 4) 1) s
          @?= Left PeerLimit
    , testCase "ordinary and BLP credit are independent across EBs" $ do
        let cfg = candidateEnv{maxRequestedBytesSizePerPeer = 65536}
        ordinary <-
          requireRight $
            reserve cfg False (AttemptKey (0 :: Int) 0 0) (Lease (Job (0 :: Int) 0) 65536) emptyLedger
        reserve cfg False (AttemptKey 0 0 1) (Lease (Job 1 0) 1) ordinary @?= Left PeerLimit
        allPeers <-
          requireRight $
            foldl
              (\acc i -> acc >>= reserve cfg True (AttemptKey 1 0 i) (Lease (Job (100 + fromIntegral i) 0) 65536))
              (Right ordinary)
              [0 .. 3]
        usage 0 allPeers @?= Usage 65536 0
        usage 1 allPeers @?= Usage 262144 0
        reserve cfg True (AttemptKey 1 0 4) (Lease (Job 4 0) 1) allPeers @?= Left PeerLimit
    , testCase "four bodies independent of TX credit and exactly-once cleanup" $ do
        full <-
          requireRight $
            reserve
              candidateEnv
              False
              (AttemptKey (0 :: Int) 0 10)
              (Lease (Job (10 :: Int) 0) 262144)
              emptyLedger
        s <-
          requireRight $
            foldl
              ( \acc i ->
                  acc
                    >>= reserve
                      candidateEnv
                      False
                      (AttemptKey (0 :: Int) 0 i)
                      (Lease (Body (fromIntegral i :: Int)) 600000)
              )
              (Right full)
              [0 .. 3]
        usage 0 s @?= Usage 262144 4
        reserve candidateEnv False (AttemptKey 0 0 4) (Lease (Body 4) 10) s
          @?= Left PeerLimit
        release (AttemptKey 0 0 0) (removePeer 0 s) @?= (False, emptyLedger)
    , testCase "ledger permits cross-peer copies but rejects duplicate peer work" $ do
        let work = Job (0 :: Int) 0
        a <-
          requireRight $
            reserve candidateEnv False (AttemptKey (0 :: Int) 0 0) (Lease work 20000) emptyLedger
        reserve candidateEnv False (AttemptKey 0 0 1) (Lease work 20000) a @?= Left SamePeerCopy
        b <- requireRight $ reserve candidateEnv False (AttemptKey 1 0 0) (Lease work 20000) a
        c <- requireRight $ reserve candidateEnv False (AttemptKey 2 0 0) (Lease work 20000) b
        copies work c @?= 3
        usage 2 c @?= Usage 20000 0
    , testProperty "executed reserve/release/remove scripts agree with independent sums" propLedger
    ]
 where
  isLeft (Left _) = True
  isLeft _ = False

-- Generated invalid/duplicate commands are deliberately retained: rejection
-- and repeated settlement must leave the independently reconstructed sums valid.
propLedger :: [(NonNegative Int, NonNegative Int, Bool)] -> Property
propLedger commands = conjoin (map check (scanl step emptyLedger (take 200 commands)))
 where
  step st (NonNegative a, NonNegative b, body)
    | a `mod` 5 == 0 = removePeer peer st
    | a `mod` 5 == 1 = snd (release key st)
    | otherwise = either (const st) id (reserve candidateEnv False key lease st)
   where
    peer = a `mod` 4
    key = AttemptKey peer 0 (fromIntegral (b `mod` 20))
    lease = Lease (if body then Body (b `mod` 7) else Job (b `mod` 7) 0) 65536
  check st =
    conjoin
      [ counterexample (show (peer, st)) $
          conjoin
            [ usage peer st === expected peer st
            , property (transactionBytes (usage peer st) <= 262144 && bodyRequests (usage peer st) <= 4)
            , let owned = [leaseWork lease | (AttemptKey p _ _, lease) <- Map.toList (leases st), p == peer]
               in length owned === length (nub owned)
            ]
      | peer <- [0 .. 3]
      ]
  expected peer st =
    foldl
      add
      (Usage 0 0)
      [lease | (AttemptKey p _ _, lease) <- Map.toList (leases st), p == peer]
  add (Usage bytes bodies) (Lease work n) = case work of
    Body{} -> Usage bytes (bodies + 1)
    Job{} -> Usage (bytes + fromIntegral n :: Word64) bodies

requireRight :: Show e => Either e a -> IO a
requireRight = either (\err -> assertFailure (show err) >> fail "unreachable") pure

candidateEnv :: LeiosFetchStaticEnv
candidateEnv = (defaultLeiosFetchStaticEnv){maxRequestedBytesSizePerPeer = 262144}
