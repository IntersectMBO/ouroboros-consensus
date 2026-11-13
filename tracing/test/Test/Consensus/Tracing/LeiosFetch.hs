{-# LANGUAGE OverloadedStrings #-}

module Test.Consensus.Tracing.LeiosFetch (tests) where

import Cardano.Logging
import Control.Monad (forM_)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing (RttSource (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import Ouroboros.Consensus.Tracing ()
import Test.Tasty
import Test.Tasty.HUnit

data Peer = Peer deriving Show
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
    , testCase "frozen selection and charge inputs survive delayed formatting" $ do
        let event =
              TraceLeiosFetchDetailEvent
                Peer
                7
                ( FetchAdmitted
                    request
                    budget
                    (Time 10)
                    usage
                    ( Just
                        ( FetchSelection
                            (Time 10)
                            (P.effectiveScore (Time 10) (P.chargePenalty (Time 10) (P.chargePenalty (Time 10) P.emptyPenalty)))
                            (Just (Time 3))
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
  request = FetchRequest 9 point (Just 1) 60
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
    [ FetchAdmitted request budget (Time 10) usage Nothing
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
