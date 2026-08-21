module TickForecastSpec (spec) where

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

nes :: NewEpochState
nes = 123

slot :: Slot
slot = 2

forecast :: NewEpochState
forecast = 126

{-
  NOTE: Why should this test succeed? Here's the explanation:

  	∙ _ ⊢ 123 ⇀⦇ epoch 2 ,NEWEPOCH⦈ (suc 123) ===> _ ⊢ 123 ⇀⦇ 0 ,NEWEPOCH⦈ 124

  and

  	adoptGenesisDelegs 124 2 = 124 + 2 = 126

  then

    _ ⊢ 123 ⇀⦇ 2 ,TICKF⦈ 126
-}

spec :: Spec
spec = do
  describe "tickfStep" $ do
    it "tickfStep results in the expected state" $
      tickfStep dummyExternalFunctions () nes slot @?= Success forecast
