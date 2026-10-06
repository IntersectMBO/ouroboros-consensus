module ProtocolSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

(.->) :: a -> b -> (a, b)
(.->) = (,)

pd :: PoolDistr
pd = MkHSMap
  [ hk .->  (1 / 3 , hkv) , 111 .-> (1 / 3 , 222) , 333 .-> (1 / 3 , 444) ]

pe :: PrtclEnv
pe = MkPrtclEnv
  { pePd = pd
  , peΗ₀ = 2
  }

ps :: PrtclState
ps = MkPrtclState
  { psCs = MkHSMap [ hk  .-> 233 , 999 .-> 888 ]
  , psΗv = 3
  , psΗc = 5
  }

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

hkv :: KeyHashV
hkv = 568

ps' :: PrtclState
ps' = MkPrtclState
  { psCs = MkHSMap [(hk, 234), (999, 888)],
    psΗv = 4,
    psΗc = 4
  }

{-
  NOTE: Why should this test succeed? Here's the explanation:

  η = hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode vrfRes))
                  = serHashToNonce (hash (encode "N" ∥ encode 678))
                  = serHashToNonce (hash (0 ∥ 0))
                  = serHashToNonce (hash (0 + 0))
                  = serHashToNonce (hash 0)
                  = serHashToNonce (succ 0)
                  = serHashToNonce 1
                  = id 1
                  = 1

  ∙ ⟦ 1 ⟧ᵘᵉ ⊢ ⟦ 3 , 5 ⟧ᵘˢ ⇀⦇ 0 ,UPDN⦈ ⟦ 3 + 1 , 3 + 1 ⟧ᵘˢ

  ∙ { hk } ⊢ [ hk .-> 233 , 999 .-> 888 ] ⇀⦇ bh ,OCERT⦈ [ hk .-> 234 , 999 .-> 888 ]

  ∙ vrfChecks 2 pd ActiveSlotCoeff bhb
    =
    vrfChecks 2 pd ½ bhb ???

	∙ hk = hash issuerVk = hash 456 = succ 456 = 457

  	∙ lookupPoolDistr pd 457 = just (1 / 3 , 568)

  		∙ 568 ≡ hash 567 = succ 567 = 568 ===> True

  		∙ verify 567 2 (789 , 678) = True

  		  since

  		    seed = slotToSeed 0 XOR nonceToSeed 2
  		         = id 0 XOR id 2
  		         = 0 + 2
  		         = 2

	    ∙ checkLeaderVal (hBLeader bhb) f σ
  	      =
	      checkLeaderVal 0 ActiveSlotCoeff (1 / 3)
	      =
	      checkLeaderVal 0 ½ (1 / 3)
	      =
	      let
		    p = pos 0 / (2 ^ 512) = 0
		    q = 1 - p = 1 - 0 = 1
		    c = ln (1 - ½) = ln ½ = -½ - ½ * -½ * -½ + (+ 1 / 3) * -½ * -½ * -½
		                          = -½ - 1/8 - 1/24 = -0.6666666666666666
	      in
		    1 / q < exp (- σ * c)
		    <===>
		    1 / 1 < exp ((- 1 / 3) * -0.6666666666666666)
		    <===>
		    1 < exp 0.2222222222222222
		    <===>
		    1 < 1 + 0.2222222222222222 + ½ * 0.2222222222222222 * 0.2222222222222222
		    <===>
		    1 < 1.2469135802469138
		    <===>
		    True

		  since

            hBLeader bhb
              = serHashToℕ (hash (encode "L" ∥ encode vrfRes))
              = serHashToℕ (hash (encode "L" ∥ encode 678))
              = serHashToℕ (hash (0 ∥ 0))
              = serHashToℕ (hash (0 + 0))
              = serHashToℕ (hash 0)
              = serHashToℕ (succ 0)
              = serHashToℕ 1
              = 0
  ===>

  ⟦ pd , 2 ⟧ᵖᵉ ⊢ ⟦ [ hk .-> 233 , 999 .-> 888 ] , 3 , 5 ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ [ hk .-> 234 , 999 .-> 888 ] , 4 , 4 ⟧ᵖˢ

-}

spec :: Spec
spec = do
  describe "prtclStep" $ do
    it "prtclStep results in the expected state" $
      prtclStep dummyExternalFunctions pe ps bh @?= Success ps'
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ prtclDebug dummyExternalFunctions pe ps bh) $ do
--    it "shows its argument" True
