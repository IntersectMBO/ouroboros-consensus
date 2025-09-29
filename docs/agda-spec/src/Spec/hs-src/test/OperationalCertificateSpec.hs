{-# LANGUAGE OverloadedRecordDot #-}

module OperationalCertificateSpec (
  spec
, bh
, bhb
, hk
) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib
import Base (
    externalFunctions
  , sampleSKeyDSIGN
  , deriveVkeyDSIGFromSkeyDSIG
  , sampleSKeyKES
  , deriveVkeyKESFromSkeyKES)

(.->) :: a -> b -> (a, b)
(.->) = (,)

coldSk :: Integer
coldSk = sampleSKeyDSIGN

coldVk :: Integer
coldVk = deriveVkeyDSIGFromSkeyDSIG coldSk

hotSk :: Integer
hotSk = sampleSKeyKES

hotVk :: Integer
hotVk = deriveVkeyKESFromSkeyKES hotSk

-- Since kp = kesPeriod bhbSlot = kesPeriod 0 = 0 / SlotsPerKESPeriodᶜ = 0 / 5 = 0,
-- then period = kp -ᵏ ocC₀ = 0 - 0 = 0.
period :: Integer
period = 0

skf :: Integer -> Integer
skf 0 = hotSk -- for period 0
skf _ = succ hotSk

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
  { ocVkₕ = hotVk
  , ocN   = 234
  , ocC₀  = 0
  , ocΣ   = ocΣ'
  }
  where
    encodedOc = 0 -- since encode (ocVkₕ , ocN , ocC₀) = 0 due to mock serialization
    ocΣ'      = externalFunctions.extSignDSIG coldSk encodedOc

bhb :: BHBody
bhb = MkBHBody
  { bhbPrevHeader = Nothing
  , bhbIssuerVk   = coldVk
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
  , bhSig  = bhSig'
  }
  where
    encodedBhb = 0 -- since encode bhb = 0 due to mock serialization
    bhSig'     = externalFunctions.extSignKES skf period encodedBhb

hk :: KeyHashS
hk = succ (bhbIssuerVk bhb) -- i.e., hash (bhbIssuerVk bhb)

-- NOTE: Why should this test succeed? Here's the explanation:
--
-- hk = hash bhbIssuerVk = succ bhbIssuerVk (due to mock hashing)
-- kp = kesPeriod bhbSlot = kesPeriod 0 = 0 / SlotsPerKESPeriodᶜ = 0 / 5 = 0
-- t = kp -ᵏ ocC₀ = 0 - 0 = 0
--
-- ∙ ocC₀ ≤ kp <=> 0 ≤ 0 <=> true
-- ∙ kp < ocC₀ +ᵏ MaxKESEvo <=> 0 < 0 + 30 <=> 0 < 30 <=> true
-- ∙ just 233 ≡ currentIssueNo stpools cs hk × (234 ≡ 233 ⊎ 234 ≡ suc 233))
-- ∙ isSignedˢ bhbIssuerVk (encode (ocVkₕ , ocN , ocC₀)) ocΣ
--    <=> isSignedˢ bhbIssuerVk (encode (ocVkₕ , 234 , 0)) ocΣ
--    <=> isSignedˢ bhbIssuerVk 0 ocΣ
--    <=> true
-- ∙ isSignedᵏ ocVkₕ t (encode bhb) bhSig
--    <=> isSignedᵏ ocVkₕ 0 (encode bhb) bhSig
--    <=> isSignedᵏ ocVkₕ 0 0 bhSig
--    <=> true

spec :: Spec
spec = do
  describe "ocertStep" $ do
    it "ocertStep results in the expected state" $
      ocertStep externalFunctions stpools cs bh @?= Success cs'
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ ocertDebug externalFunctions stpools cs bh) $ do
--    it "shows its argument" True
