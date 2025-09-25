{-# OPTIONS -Wno-orphans #-}
module TickNonceSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

initEnv :: TickNonceEnv
initEnv = MkTickNonceEnv { ηc = 2, ηph = 5 }

initState :: TickNonceState
initState = MkTickNonceState { η₀ = 3, ηh = 4 }

testTickNonceStepFalse :: ComputationResult Text TickNonceState
testTickNonceStepFalse = ticknStep initEnv initState False

testTickNonceStepTrue :: ComputationResult Text TickNonceState
testTickNonceStepTrue = ticknStep initEnv initState True

spec :: Spec
spec = do
  describe "ticknStep" $ do
    it "ticknStep results in the expected state with signal False" $
      testTickNonceStepFalse @?= Success (MkTickNonceState { η₀ = 3, ηh = 4 })
    it "ticknStep results in the expected state with signal True" $
      testTickNonceStepTrue @?= Success (MkTickNonceState { η₀ = 6, ηh = 5 })
