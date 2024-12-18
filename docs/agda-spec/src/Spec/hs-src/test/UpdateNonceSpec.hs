{-# OPTIONS -Wno-orphans #-}
module UpdateNonceSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

initEnv :: UpdateNonceEnv
initEnv = MkUpdateNonceEnv { ueΗ = 2 }

initState :: UpdateNonceState
initState = MkUpdateNonceState { usΗv = 3, usΗc = 4 }

slotBeforeWindow :: Slot
slotBeforeWindow = 1

testUpdateNonceStepWithSlotBeforeWindow :: ComputationResult Text UpdateNonceState
testUpdateNonceStepWithSlotBeforeWindow = updnStep dummyExternalFunctions initEnv initState slotBeforeWindow

{-                      window = 20
                      ______________
                     /              \
                     s
 +---+---+---+---+---+---+---+---+---+---+---+---+
 0  1  2 ...         99  100         119 ... 199 200
 \___________________/   \___________________/
        epoch 0                 epoch 1
-}

slotAfterWindow :: Slot
slotAfterWindow = 99

testUpdateNonceStepWithSlotAfterWindow :: ComputationResult Text UpdateNonceState
testUpdateNonceStepWithSlotAfterWindow = updnStep dummyExternalFunctions initEnv initState slotAfterWindow

spec :: Spec
spec = do
  describe "updnStep" $ do
    it "updnStep results in the expected state with slot before window" $
      testUpdateNonceStepWithSlotBeforeWindow @?= Success (MkUpdateNonceState { usΗv = 3 + 2, usΗc = 3 + 2 })
    it "updnStep results in the expected state with slot after window" $
      testUpdateNonceStepWithSlotAfterWindow @?= Success (MkUpdateNonceState { usΗv = 3 + 2, usΗc = 4 })
