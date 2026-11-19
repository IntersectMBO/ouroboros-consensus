{-# LANGUAGE OverloadedStrings #-}

module Test.Consensus.Tracing.LeiosFetch (tests) where

import Cardano.Logging
import Control.Monad (forM_)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Tracer (nullTracer)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config (defaultLeiosFetchStaticEnv)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing (RttSource (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace.Policy as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Tracing ()
import Ouroboros.Consensus.Util.IOLike
  ( atomically
  , modifyTVar
  , readTVar
  , uncheckedNewTVarM
  , writeTVar
  )
import Test.Tasty
import Test.Tasty.HUnit

data Peer = Peer deriving (Eq, Ord, Show)
instance LogFormatting Peer where
  forMachine _ _ = "peerId" .= (0 :: Int)
  forHuman _ = "peer"

tests :: TestTree
tests =
  testGroup
    "LeiosFetch production schema"
    [ testCase "every policy and detail constructor names a configured namespace" $ do
        forM_ routine $ \event -> known (TraceLeiosFetchClientPolicy Peer 7 event)
        known (TraceLeiosFetchClientClosureSummary point summary :: TraceLeiosFetchClientEvent Peer)
        forM_ details $ \event -> known (TraceLeiosFetchDetailEvent Peer 7 event)
    , testCase "selection and charge fields have the expected schema" $ do
        let event =
              TraceLeiosFetchDetailEvent
                Peer
                7
                ( FetchAdmitted
                    request
                    budget
                    (Time 10)
                    usage
                    ( SJust
                        ( FetchSelection
                            (Time 10)
                            (P.effectiveScore (Time 10) (P.chargePenalty (Time 10) (P.chargePenalty (Time 10) P.emptyPenalty)))
                            (SJust (Time 3))
                        )
                    )
                )
            rendered = forMachine DNormal event
        KM.lookup "selectionAt" rendered @?= Just (Number 10)
        KM.lookup "firstClosureOfferAt" rendered @?= Just (Number 3)
        KM.lookup "selectionScore" rendered @?= Just (Number 2)
        KM.lookup "invocation" rendered @?= Just (Number 7)
        KM.lookup "ordinal" rendered @?= Just (Number 9)
        let charge =
              forMachine
                DNormal
                ( TraceLeiosFetchClientPolicy
                    Peer
                    7
                    ( FetchPenaltyCharged
                        request
                        (Time 20)
                        (P.penaltySnapshot P.emptyPenalty)
                        P.zeroScore
                        (P.penaltySnapshot (P.chargePenalty (Time 20) P.emptyPenalty))
                    )
                )
        KM.lookup "eventAt" charge @?= Just (Number 20)
        KM.lookup "effectiveBefore" charge @?= Just (Number 0)
    , testCase "production admission captures original RTT and invocation before delayed emission" $ do
        ready <- atomically W.newReadiness
        rt <- R.newRuntime defaultLeiosFetchStaticEnv ready (pure False)
        commands <- uncheckedNewTVarM Seq.empty
        sample <- uncheckedNewTVarM (Just (T.RttObservation 0.1 (Time 10)))
        client <- atomically $ R.openClient rt Peer False commands (readTVar sample)
        let req = LeiosBlockRequest (MkLeiosBlockRequest point 1000 maxBound)
        key <- atomically (R.enqueueRequest client (Time 10) req) >>= either (fail . show) pure
        captured <-
          atomically $
            A.admitted rt (Time 10) (SJust (FetchSelection (Time 10) P.zeroScore (SJust (Time 3)))) key
        length captured @?= 1
        _ <- atomically $ do
          writeTVar sample (Just (T.RttObservation 0.3 (Time 11)))
          R.closeClient client
          R.openClient rt Peer True commands (readTVar sample)
        rendered <- uncheckedNewTVarM []
        A.emitObservations
          nullTracer
          (mkTracer (\event -> atomically $ modifyTVar rendered (forMachine DNormal event :)))
          captured
        events <- atomically $ readTVar rendered
        case events of
          [event] -> do
            KM.lookup "invocation" event @?= Just (Number 0)
            KM.lookup "ordinal" event @?= Just (Number 0)
            KM.lookup "rttAllowanceSeconds" event @?= Just (Number 0.15)
            KM.lookup "rttSource" event @?= Just (String "KeepAliveSample")
            KM.lookup "startupSeconds" event @?= Just (Number 0.15)
            KM.lookup "eventAt" event @?= Just (Number 10)
            KM.lookup "firstClosureOfferAt" event @?= Just (Number 3)
            KM.lookup "txBytesReserved" event @?= Just (Number 0)
            KM.lookup "bodiesReserved" event @?= Just (Number 1)
            KM.lookup "txByteLimit" event @?= Just (Number 262144)
            KM.lookup "bodyCountLimit" event @?= Just (Number 4)
          _ -> assertFailure "one production admission detail was expected"
    , testCase "confirmed origin summary does not masquerade as wire traffic" $ do
        let rendered =
              forMachine
                DNormal
                (TraceLeiosFetchClientClosureSummary point summary :: TraceLeiosFetchClientEvent Peer)
        KM.lookup "uniqueNetworkBytes" rendered @?= Just (Number 60)
        KM.lookup "localBytes" rendered @?= Just (Number 40)
        KM.lookup "wireBytes" rendered @?= Nothing
    ]
 where
  point = MkLeiosPoint (SlotNo 5) (endorserBlockHash (mkEndorserBlock []))
  request = FetchRequest 9 point (SJust 1) 60
  usage = FetchUsage 60 0 262144 4
  budget = FetchBudget 300000 0.15 0.002 0.15 KeepAliveSample
  summary = ClosureSummary 100 40 60 2 (Just 60) False
  routine =
    [ FetchUpstreamChanged True
    , FetchHedgeOpened request FetchOwnHeadExpiry
    , FetchHedgeOpened request FetchBlockedFollower
    , FetchBodyWon request 50
    , FetchPenaltyCharged
        request
        (Time 1)
        (P.penaltySnapshot P.emptyPenalty)
        P.zeroScore
        (P.penaltySnapshot (P.chargePenalty (Time 1) P.emptyPenalty))
    , FetchInvocationClosed usage True
    , FetchRegistrationRemoved usage
    ]
  details =
    [ FetchAdmitted request budget (Time 10) usage SNothing
    , FetchHeadActivated request (Time 10) (Time 11) False
    , FetchPressureChanged [minBound .. maxBound] usage
    ]
      ++ [FetchTransition request phase (Time 10) usage | phase <- [minBound .. maxBound]]
  known :: (MetaTrace a, LogFormatting a) => a -> Assertion
  known value = do
    let ns = namespaceFor value
    assertBool "missing namespace" (ns `elem` allNamespaces)
    assertBool "missing severity" (severityFor ns Nothing /= Nothing)
    assertBool "missing documentation" (documentFor ns /= Nothing)
    assertBool "missing JSON kind" (KM.member "kind" (forMachine DNormal value))
