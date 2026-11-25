module Test.Consensus.Leios.LeiosFetch.RequestState (tests) where

import Control.Monad (foldM, forM_)
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
    , testCase "owners retain other peers and delete the last work bucket" $ do
        let body = Body (7 :: Int)
            job = Job 7 0
            a = AttemptKey (0 :: Int) 0 0
            b = AttemptKey 1 2 0
            c = AttemptKey 0 1 1
        first <- requireRight $ reserve candidateEnv False a (Lease body 0) emptyLedger
        both <- requireRight $ reserve candidateEnv False b (Lease body 0) first
        mixed <- requireRight $ reserve candidateEnv False c (Lease job 0) both
        owners mixed @?= Map.fromList [(body, Map.fromList [(0, a), (1, b)]), (job, Map.singleton 0 c)]
        copies body mixed @?= 2
        let retained = removePeer 0 mixed
        owners retained @?= Map.singleton body (Map.singleton 1 b)
        copies job retained @?= 0
        release b retained @?= (True, emptyLedger)
        removePeer 1 retained @?= emptyLedger
    , testCase "late release preserves a replacement of the same body or job" $
        forM_ [Body (9 :: Int), Job 9 0] $ \work -> do
          let old = AttemptKey (0 :: Int) 0 0
              fresh = AttemptKey 0 1 0
          first <- requireRight $ reserve candidateEnv False old (Lease work 0) emptyLedger
          reserve candidateEnv False fresh (Lease work 0) first @?= Left SamePeerCopy
          let (released, vacant) = release old first
          released @?= True
          vacant @?= emptyLedger
          replacement <- requireRight $ reserve candidateEnv False fresh (Lease work 0) vacant
          release old replacement @?= (False, replacement)
          owners replacement @?= Map.singleton work (Map.singleton 0 fresh)
          copies work replacement @?= 1
          removePeer 0 replacement @?= emptyLedger
          release fresh (removePeer 0 replacement) @?= (False, emptyLedger)
    , testProperty "executed reserve/release/remove scripts agree with independent leases, totals and owners" propLedger
    ]
 where
  isLeft (Left _) = True
  isLeft _ = False

-- Keep literal invalid commands, and also target live model entries for
-- duplicates, copies and releases. Shrinking still uses the input tuple list;
-- an adaptive command falls back to its literal form if its source disappeared.
propLedger :: [(NonNegative Int, NonNegative Int, Bool)] -> Property
propLedger commands =
  checkCoverage $
    cover 50 ("reserve" `elem` events) "successful reserve" $
      cover 10 ("copy" `elem` events) "cross-peer copy" $
        cover 10 ("release" `elem` events) "successful release" $
          cover 10 ("last owner" `elem` events) "last-owner release" $
            cover 10 ("remove" `elem` events) "nonempty peer removal" $
              cover 10 ("rejection" `elem` events) "rejected reserve" $
                cover 10 ("same peer" `elem` events) "same-peer rejection" $
                  cover 10 ("duplicate" `elem` events) "duplicate-attempt rejection" $
                    cover 10 ("stale release" `elem` events) "absent-key release" $
                      cover 10 ("zero" `elem` events) "zero-byte reserve" $
                        cover 10 ("body" `elem` events) "body reserve" $
                          conjoin (cleanupCheck : checks)
 where
  ((final, _), checks, events) = foldl step ((emptyLedger, Map.empty), [], []) (take 400 commands)
  cleanupCheck = foldr removePeer final [0 .. 3] === emptyLedger
  step ((actual, model), checksSoFar, eventsSoFar) (NonNegative a, NonNegative b, body) =
    let peer = mod (div a 8) 4
        literalKey = AttemptKey peer (fromIntegral (mod a 3)) (fromIntegral (mod b 200))
        work = if body then Body (mod b 211) else Job (mod b 211) (mod b 5)
        bytes = case mod b 4 of
          0 -> 0
          1 -> 20000
          2 -> 65536
          _ -> 300000
        literalLease = Lease work bytes
        selected = if Map.null model then Nothing else Just (Map.elemAt (mod b (Map.size model)) model)
        -- Change the invocation to test same-peer exclusion across lifetimes;
        -- a repeated literal key separately exercises DuplicateAttempt.
        (key, lease) = case (mod a 8, selected) of
          (3, Just entry) -> entry
          (4, Just (AttemptKey owner generation ordinal, entry)) ->
            (AttemptKey owner (generation + 1) ordinal, entry)
          (5, Just (AttemptKey owner _ _, entry)) ->
            (AttemptKey (mod (owner + 1) 4) (fromIntegral (mod a 3)) (fromIntegral (mod b 200)), entry)
          _ -> (literalKey, literalLease)
        (following, expected, outcome, stepEvents) = case mod a 8 of
          0 ->
            ( removePeer peer actual
            , Map.filterWithKey (\(AttemptKey owner _ _) _ -> owner /= peer) model
            , property True
            , ["remove" | any (\(AttemptKey owner _ _) -> owner == peer) (Map.keys model)]
            )
          op | op == 1 || op == 2 ->
            let releasing = case (op, selected) of (2, Just (live, _)) -> live; _ -> key
                (owned, next) = release releasing actual
                lastOwner = case Map.lookup releasing model of
                  Nothing -> False
                  Just entry -> length [() | other <- Map.elems model, leaseWork other == leaseWork entry] == 1
             in (next, Map.delete releasing model, owned === Map.member releasing model,
                 ["release" | owned] ++ ["last owner" | owned && lastOwner] ++ ["stale release" | not owned])
          _ ->
            let AttemptKey owner _ _ = key
                reference = reserveModel owner key lease model
                executed = reserve candidateEnv False key lease actual
                next = either (const actual) id executed
                expectedNext = either (const model) id reference
                verdict = fmap (const ()) executed === fmap (const ()) reference
                successful = either (const False) (const True) reference
                copied = any ((== leaseWork lease) . leaseWork) (Map.elems model)
                tags = ["reserve" | successful]
                  ++ ["copy" | successful && copied]
                  ++ ["zero" | successful && leaseBytes lease == 0]
                  ++ ["body" | successful, Body{} <- [leaseWork lease]]
                  ++ ["rejection" | not successful]
                  ++ ["same peer" | reference == Left SamePeerCopy]
                  ++ ["duplicate" | reference == Left DuplicateAttempt]
             in (next, expectedNext, verdict, tags)
        expectedOwners = Map.fromListWith Map.union
          [(leaseWork entry, Map.singleton owner attempt) | (attempt@(AttemptKey owner _ _), entry) <- Map.toList expected]
        valid = counterexample (show (a, b, body, key, lease, following, expected)) $
          conjoin
            ( outcome : (leases following === expected) : (owners following === expectedOwners) :
                [copies target following === Map.size owned | (target, owned) <- Map.toList expectedOwners] ++
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
     in ((following, expected), valid : checksSoFar, stepEvents ++ eventsSoFar)
  -- This model has no cached totals or owner index and does not call the implementation's
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
