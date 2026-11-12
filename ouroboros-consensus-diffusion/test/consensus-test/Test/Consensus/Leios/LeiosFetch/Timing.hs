module Test.Consensus.Leios.LeiosFetch.Timing (tests) where

import Control.Exception (evaluate)
import Control.Monad.Class.MonadTime.SI (Time (Time))
import NoThunks.Class (noThunks)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "head timing"
    [ testCase "stored clock facts retain no deferred runtime predicates or selectors" $ do
        inputs0 <- inputs 1000
        let arrival = Time (snapshotTransfer inputs0 + snapshotRtt inputs0)
            continuous = snapshotTransfer inputs0 > 0
        clock <-
          evaluate
            ( received
                arrival
                (activate (Just arrival) (Just arrival) (sent arrival continuous (newClock inputs0)))
            )
        noThunks [] clock >>= maybe (pure ()) (assertFailure . show)
    , testCase "raw RTT freshness, cap and invalid samples" $ do
        conservativeRttWithSource (Time 31) Nothing @?= (0.5, MissingRtt)
        conservativeRttWithSource (Time 30) (Just (RttObservation 0.1 (Time 0))) @?= (0.15, KeepAliveSample)
        conservativeRttWithSource (Time 31) (Just (RttObservation 0.1 (Time 0))) @?= (0.5, StaleRtt)
        conservativeRttWithSource (Time 0) (Just (RttObservation 1 (Time 0))) @?= (0.5, KeepAliveSample)
        conservativeRttWithSource (Time 0) (Just (RttObservation 0 (Time 0))) @?= (0.5, InvalidRtt)
        conservativeRttWithSource (Time 0) (Just (RttObservation 0.1 (Time 1))) @?= (0.5, InvalidRtt)
    , testCase "startup cumulative windows and widened maximum payload" $ do
        initialRounds 14600 0 @?= Right 1
        initialRounds 14600 43800 @?= Right 2
        initialRounds 14600 43801 @?= Right 3
        initialRounds 1 maxBound @?= Right 32
        initialRounds 0 10 @?= Left "zero initial window"
    , testCase "actual request transfer and capped startup compete" $ do
        s <- inputs 65536
        snapshotTransfer s @?= fromRational (65536 * 8 / 300000)
        snapshotCold s @?= 0.45
        snapshotRttSource s @?= KeepAliveSample
        s14 <- inputs 14600
        snapshotCold s14 @?= 0.15
        s43 <- inputs 43800
        snapshotCold s43 @?= 0.3
        s102 <- inputs 102200
        snapshotCold s102 @?= 0.45
        s103 <- inputs 102201
        snapshotCold s103 @?= 0.5
    , testCase "unsent and inactive followers have no deadline" $ do
        s <- inputs 1000
        deadline (newClock s) @?= Nothing
        deadline (sent (Time 0) True (newClock s)) @?= Nothing
    , testCase "FIFO transit is spent once; local handler readiness is separate" $ do
        s <- inputs 1000
        let fresh = sent (Time 0) False (newClock s)
        deadline (activate Nothing Nothing fresh) @?= Just (Time 0.3)
        deadline (activate (Just (Time 0.2)) Nothing fresh) @?= Just (Time 0.35)
        deadline (activate (Just (Time 0.2)) (Just (Time 0.7)) fresh) @?= Just (Time 0.85)
    , testCase "known terminal wins over late planner observation" $ do
        s <- inputs 1000
        let active = activate Nothing Nothing (sent (Time 0) False (newClock s))
            early = received (Time 0.1) active
            (fired, once) = expire (Time 1) active
        expire (Time 10) early @?= (False, early)
        fired @?= True
        fst (expire (Time 2) once) @?= False
        lateReply (received (Time 0.4) active) @?= True
    , testCase "fast terminal before send return retains original input time" $ do
        s <- inputs 1000
        let c = activate Nothing Nothing (sent (Time 0.2) False (received (Time 0.1) (newClock s)))
        clockReceipt c @?= Just (Time 0.1)
        lateReply c @?= False
        fst (expire (Time 10) c) @?= False
    , testCase "later head activation cannot renew a clock" $ do
        s <- inputs 1000
        let c = activate Nothing Nothing (sent (Time 0) False (newClock s))
        activate (Just (Time 9)) (Just (Time 10)) c @?= c
    ]
 where
  inputs bytes =
    either (\err -> assertFailure err >> fail "unreachable") pure $
      snapshot candidateEnv (Time 0) (Just (RttObservation 0.1 (Time 0))) bytes

candidateEnv :: LeiosFetchStaticEnv
candidateEnv = (defaultLeiosFetchStaticEnv 262144){maxRequestedBytesSizePerPeer = 262144}
