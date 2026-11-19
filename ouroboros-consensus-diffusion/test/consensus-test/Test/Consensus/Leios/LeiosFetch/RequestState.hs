module Test.Consensus.Leios.LeiosFetch.RequestState (tests) where

import Control.Monad (foldM)
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
        usage 0 st @?= Usage 32768 0 1
    , testCase "whole-job credit fits exactly and cannot overshoot" $ do
        let add acc i =
              acc
                >>= reserve
                  candidateEnv
                  False
                  (AttemptKey (0 :: Int) 0 i)
                  (Lease (Job (0 :: Int) (fromIntegral i)) 65536)
        s <- requireRight (foldl add (Right emptyLedger) [0 .. 3])
        usage 0 s @?= Usage 262144 0 4
        reserve candidateEnv False (AttemptKey 0 0 4) (Lease (Job 0 4) 1) s
          @?= Left TransactionCreditLimit
    , testCase "ordinary and BLP credit are independent across EBs" $ do
        let cfg = candidateEnv{maxRequestedBytesSizePerPeer = 65536}
        ordinary <-
          requireRight $
            reserve cfg False (AttemptKey (0 :: Int) 0 0) (Lease (Job (0 :: Int) 0) 65536) emptyLedger
        reserve cfg False (AttemptKey 0 0 1) (Lease (Job 1 0) 1) ordinary
          @?= Left TransactionCreditLimit
        allPeers <-
          requireRight $
            foldl
              (\acc i -> acc >>= reserve cfg True (AttemptKey 1 0 i) (Lease (Job (100 + fromIntegral i) 0) 65536))
              (Right ordinary)
              [0 .. 3]
        usage 0 allPeers @?= Usage 65536 0 1
        usage 1 allPeers @?= Usage 262144 0 4
        reserve cfg True (AttemptKey 1 0 4) (Lease (Job 4 0) 1) allPeers
          @?= Left TransactionCreditLimit
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
        usage 0 s @?= Usage 262144 4 5
        reserve candidateEnv False (AttemptKey 0 0 4) (Lease (Body 4) 10) s
          @?= Left BodyCountLimit
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
        usage 2 c @?= Usage 20000 0 1
    , testCase "shared count binds bodies and zero-byte jobs at exactly 150" $ do
        let bodies = [(AttemptKey (0 :: Int) 0 i, Lease (Body (fromIntegral i :: Int)) 0)
                     | i <- [0 .. 2]]
            jobs = [(AttemptKey 0 0 i, Lease (Job (fromIntegral i) 0) 0) | i <- [3 .. 149]]
            add ledger (key, lease) = requireRight (reserve candidateEnv False key lease ledger)
        full <- foldM add emptyLedger (bodies ++ jobs)
        usage 0 full @?= Usage 0 3 150
        reserve candidateEnv False (AttemptKey 0 0 150) (Lease (Body 150) 1) full
          @?= Left RequestCountLimit
        reserve candidateEnv False (AttemptKey 0 0 150) (Lease (Job 150 0) 0) full
          @?= Left RequestCountLimit
        let (removed, space) = release (AttemptKey 0 0 3) full
        removed @?= True
        usage 0 space @?= Usage 0 3 149
        release (AttemptKey 0 0 3) space @?= (False, space)
        refilled <- requireRight $
          reserve candidateEnv False (AttemptKey 0 0 150) (Lease (Body 150) 1) space
        usage 0 refilled @?= Usage 0 4 150
        fresh <- requireRight $
          reserve candidateEnv False (AttemptKey 0 1 0) (Lease (Job 0 0) 0) (removePeer 0 refilled)
        release (AttemptKey 0 0 0) fresh @?= (False, fresh)
        usage 0 fresh @?= Usage 0 0 1
    , testCase "permanently unfit jobs differ from temporary byte pressure" $ do
        let cfg = candidateEnv{maxRequestedBytesSizePerPeer = 32768}
            key = AttemptKey (0 :: Int) 0 0
        reserve cfg False key (Lease (Job (0 :: Int) 0) 32769) emptyLedger
          @?= Left RequestExceedsCredit
        full <- requireRight $ reserve cfg False key (Lease (Job (0 :: Int) 0) 32768) emptyLedger
        reserve cfg False (AttemptKey 0 0 1) (Lease (Job 1 0) 1) full
          @?= Left TransactionCreditLimit
        reserve cfg False key (Lease (Job 2 0) 1) full @?= Left DuplicateAttempt
    , testProperty "executed reserve/release/remove scripts agree with independent sums" propLedger
    ]
 where
  isLeft (Left _) = True
  isLeft _ = False

-- Generated invalid/duplicate commands are deliberately retained: rejection
-- and repeated settlement must leave the independently reconstructed sums valid.
propLedger :: [(NonNegative Int, NonNegative Int, Bool)] -> Property
propLedger commands = conjoin (snd (foldl step ((emptyLedger, Map.empty), []) (take 400 commands)))
 where
  step ((actual, model), checks) (NonNegative a, NonNegative b, body) =
    let peer = mod a 4
        key = AttemptKey peer (fromIntegral (mod a 3)) (fromIntegral (mod b 200))
        work = if body then Body (mod b 211) else Job (mod b 211) (mod b 5)
        bytes = case mod b 4 of
          0 -> 0
          1 -> 20000
          2 -> 65536
          _ -> 300000
        lease = Lease work bytes
        (following, expected, outcome) = case mod a 5 of
          0 ->
            (removePeer peer actual,
             Map.filterWithKey (\(AttemptKey owner _ _) _ -> owner /= peer) model,
             property True)
          1 ->
            let (owned, next) = release key actual
             in (next, Map.delete key model, owned === Map.member key model)
          _ ->
            let reference = reserveModel peer key lease model
                executed = reserve candidateEnv False key lease actual
                next = either (const actual) id executed
                expectedNext = either (const model) id reference
                verdict = fmap (const ()) executed === fmap (const ()) reference
             in (next, expectedNext, verdict)
        valid = counterexample (show (a, b, body, following, expected)) $
          conjoin
            ( outcome : (leases following === expected) :
                [ conjoin
                    [ usage owner following === sums owner expected
                    , property (requestCount (usage owner following) <= 150)
                    , let owned =
                            [ leaseWork entry
                            | (AttemptKey p _ _, entry) <- Map.toList expected, p == owner
                            ]
                       in length owned === length (nub owned)
                    ]
                | owner <- [0 .. 3]
                ]
            )
     in ((following, expected), valid : checks)
  -- This model has no cached totals and does not call the implementation's
  -- reservation/release functions. Rejected commands leave its map unchanged.
  reserveModel peer key lease@(Lease work bytes) model
    | Map.member key model = Left DuplicateAttempt
    | any ((== work) . leaseWork . snd) owned = Left SamePeerCopy
    | Job{} <- work, bytes > 262144 = Left RequestExceedsCredit
    | length owned >= 150 = Left RequestCountLimit
    | Body{} <- work, length [() | (_, Lease Body{} _) <- owned] >= 4 = Left BodyCountLimit
    | Job{} <- work
    , sum [fromIntegral n :: Word64 | (_, Lease Job{} n) <- owned] + fromIntegral bytes > 262144 =
        Left TransactionCreditLimit
    | otherwise = Right (Map.insert key lease model)
   where
    owned = [(k, entry) | (k@(AttemptKey p _ _), entry) <- Map.toList model, p == peer]
  sums peer model =
    let owned = [entry | (AttemptKey p _ _, entry) <- Map.toList model, p == peer]
     in Usage
          (sum [fromIntegral n | Lease Job{} n <- owned])
          (fromIntegral (length [() | Lease Body{} _ <- owned]))
          (fromIntegral (length owned))

requireRight :: Show e => Either e a -> IO a
requireRight = either (\err -> assertFailure (show err) >> fail "unreachable") pure

candidateEnv :: LeiosFetchStaticEnv
candidateEnv = (defaultLeiosFetchStaticEnv){maxRequestedBytesSizePerPeer = 262144}
