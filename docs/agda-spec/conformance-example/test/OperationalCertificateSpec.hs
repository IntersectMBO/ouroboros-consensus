module OperationalCertificateSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

(.->) :: a -> b -> (a, b)
(.->) = (,)

stpools :: OCertEnv
stpools = MkHSSet []

cs :: OCertState
cs = MkHSMap
  [ hk .-> 233
  , 999 .-> 888
  ]

cs' :: OCertState
cs' = MkHSMap
  [ hk .-> 234
  , 999 .-> 888
  ]

oc :: OCert
oc = MkOCert
  { ocVkₕ = 123
  , ocN   = 234
  , ocC₀  = 0
  , ocΣ   = 345
  }

bhb :: BHBody
bhb = MkBHBody
  { bhbPrevHeader = Nothing
  , bhbIssuerVk   = 456
  , bhbVrfVk      = 567
  , bhbBlockNo    = 1
  , bhbSlot       = 0
  , bhbVrfRes     = 678
  , bhbVrfPrf     = 789
  , bhbBodySize   = 100
  , bhbBodyHash   = 890
  , bhbOc         = oc
  , bhbPv         = (1, 0)
  }

bh :: BHeader
bh = MkBHeader
  { bhBody = bhb
  , bhSig  = 901
  }

hk :: KeyHashS
hk = succ (bhbIssuerVk bhb) -- i.e., hash (bhbIssuerVk bhb)

externalFunctions :: ExternalFunctions
externalFunctions = dummyExternalFunctions { extIsSignedDSIG = \ _ _ sig -> sig > 0 }

-- NOTE: Why should this test succeed? Here's the explanation:
--
-- hk = hash bhbIssuerVk = hash 456 = 457
-- kp = kesPeriod bhbSlot = kesPeriod 0 = 0 / SlotsPerKESPeriodᶜ = 0 / 5 = 0
-- t = kp -ᵏ ocC₀ = 0 - 0 = 0
--
-- ∙ ocC₀ ≤ kp <=> 0 ≤ 0 <=> true
-- ∙ kp < ocC₀ +ᵏ MaxKESEvo <=> 0 < 0 + 30 <=> 0 < 30 <=> true
-- ∙ just 233 ≡ currentIssueNo stpools cs hk × (234 ≡ 233 ⊎ 234 ≡ suc 233))
-- ∙ isSignedˢ bhbIssuerVk (encode (ocVkₕ , ocN , ocC₀)) ocΣ
--    <=> isSignedˢ 456 (encode (123 , 234 , 0)) 345
--    <=> isSignedˢ 456 0 345
--    <=> true
-- ∙ isSignedᵏ ocVkₕ t (encode bhb) 901
--    <=> isSignedᵏ 123 0 (encode bhb) 901
--    <=> isSignedᵏ 123 0 0 901
--    <=> true

spec :: Spec
spec = do
  describe "ocertStep" $ do
    it "ocertStep results in the expected state" $
      ocertStep externalFunctions stpools cs bh @?= Success cs'
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ ocertDebug dummyExternalFunctions stpools cs bh) $ do
--    it "shows its argument" True
