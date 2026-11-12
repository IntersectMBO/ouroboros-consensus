module Test.Consensus.Leios.LeiosFetch.Coverage (tests) where

import qualified Data.IntMap.Strict as IntMap
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Coverage
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "confirmed payload coverage"
    [ testCase "local fills and repeated network offsets are disjoint" $ do
        initial <- maybe (assertFailure "initial" >> fail "initial") pure (beginCoverage 100 40)
        first <-
          maybe (assertFailure "first" >> fail "first") pure (confirmNetwork (IntMap.singleton 0 20) initial)
        second <-
          maybe
            (assertFailure "second" >> fail "second")
            pure
            (confirmNetwork (IntMap.fromList [(0, 20), (1, 40)]) first)
        case finishCoverage False second of
          Nothing -> assertFailure "no summary"
          Just (summary, done) -> do
            summary @?= ClosureSummary 100 40 60 2 (Just 60) False
            finishCoverage False done @?= Nothing
            confirmNetwork (IntMap.singleton 2 100) done @?= Just done
    , testCase "all local includes empty closures" $
        mapM_
          ( \n -> do
              let result = beginCoverage n n >>= finishCoverage False
              fmap fst result @?= Just (ClosureSummary n n 0 0 (Just 0) False)
          )
          [0, 100]
    , testCase "forge preserves earlier network evidence without crediting its own bytes" $ do
        let value = beginCoverage 100 20 >>= confirmNetwork (IntMap.singleton 0 30)
        fmap fst (value >>= finishCoverage True) @?= Just (ClosureSummary 100 70 30 1 (Just 80) True)
        fmap fst (finishCoverage True (beginForgedCoverage 100))
          @?= Just (ClosureSummary 100 100 0 0 Nothing True)
    , testCase "invalid or unconfirmed origins are not invented" $ do
        beginCoverage 50 51 @?= Nothing
        (beginCoverage 50 0 >>= confirmNetwork (IntMap.singleton 0 51)) @?= Nothing
    ]
