{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.LeiosUtils.CallTrace (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , atomically
  , newTVar
  , readTVar
  , writeTVar
  )
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay))
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.Map as Map
import LeiosUtils.CallTrace
  ( CallCtx
  , CallState (CallState, csActiveCalls, csInactiveCalls, csTotalMeasure)
  , CallTrace
  , callTrace
  , foldCallTraceFromInit
  , rootCallCtx
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "CallTrace"
    [ testIOLike
        "fooBarBaz has a correct call trace"
        fooBarBaz
        ( \t -> case foldCallTraceFromInit t of
            Left err -> assertFailure $ show err
            Right CallState{..} -> do
              csActiveCalls @?= mempty
              Map.size csInactiveCalls @?= 7
              assertBool "Some measurements were observed" $ csTotalMeasure > mempty
        )
    ]

fooBarBaz :: IOLike m => m [CallTrace String String]
fooBarBaz = do
  (tracer, readTrace) <- newRecordingTracer
  rootCtx <- rootCallCtx "main"
  _ <-
    callTrace
      tracer
      rootCtx
      "main"
      "foobarbaz"
      ""
      ( \mainCtx -> do
          threadDelay 1000
          _ <- foo tracer mainCtx "hello foo"
          _ <- bar tracer mainCtx "hello bar"
          baz tracer mainCtx "hello baz"
      )

  readTrace

foo :: IOLike m => (CallTrace String String -> m ()) -> CallCtx m -> String -> m String
foo trace ctx fooArg =
  callTrace trace ctx "main" "foo" fooArg $ \fooCtx -> do
    threadDelay 1000
    barRes <- bar trace fooCtx "hello bar"
    return $ "bar says: " <> barRes

bar :: IOLike m => (CallTrace String String -> m ()) -> CallCtx m -> String -> m String
bar trace ctx barArg =
  callTrace trace ctx "main" "bar" barArg $ \barCtx -> do
    threadDelay 1000
    bazRes <- baz trace barCtx "hello baz"
    return $ "baz says: " <> bazRes

baz :: IOLike m => (CallTrace String String -> m ()) -> CallCtx m -> String -> m String
baz trace ctx bazArg =
  callTrace trace ctx "main" "baz" bazArg $ \_bazCtx -> do
    threadDelay 1000
    return $ "'sup"

-- | Utils
newRecordingTracer :: MonadSTM m => m (CallTrace a r -> m (), m [CallTrace a r])
newRecordingTracer = do
  var <- atomically $ newTVar []
  let tracer ct = atomically $ do
        xs <- readTVar var
        writeTVar var (ct : xs)
      readTrace = reverse <$> atomically (readTVar var)
  pure (tracer, readTrace)

testIOLike :: String -> (forall m. IOLike m => m a) -> (a -> Assertion) -> TestTree
testIOLike lbl act ass =
  testGroup
    lbl
    [ testCase "IOSim" $ testInIOSim act ass
    , testCase "IO" $ testInIO act ass
    ]

testInIOSim :: (forall s. IOSim s a) -> (a -> Assertion) -> Assertion
testInIOSim act ass = do
  let res = runSimOrThrow act
  ass res

testInIO :: IO a -> (a -> Assertion) -> Assertion
testInIO act ass = do
  res <- act
  ass res
