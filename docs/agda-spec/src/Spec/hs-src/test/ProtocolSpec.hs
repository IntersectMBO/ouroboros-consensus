{-# LANGUAGE OverloadedRecordDot #-}

module ProtocolSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import qualified OperationalCertificateSpec as OCS
import Lib
import Base (
    externalFunctions
  , sampleSKeyVRF
  , deriveVkeyVRFFromSkeyVRF)

(.->) :: a -> b -> (a, b)
(.->) = (,)

vrfSk :: Integer
vrfSk = sampleSKeyVRF

vrfVk :: Integer
vrfVk = deriveVkeyVRFFromSkeyVRF vrfSk

-- seed = slotToSeed 0 XOR nonceToSeed 2
--      = id 0 XOR id 2
--      = 0 + 2
--      = 2
seed :: Integer
seed = 2

pd :: PoolDistr
pd = MkHSMap
  [ OCS.hk .->  (1 / 3 , succ vrfVk) ]  -- i.e., hash vrfVk

pe :: PrtclEnv
pe = MkPrtclEnv
  { pePd = pd
  , peΗ₀ = 2
  }

ps :: PrtclState
ps = MkPrtclState
  { psCs = MkHSMap [ OCS.hk .-> 233 , 999 .-> 888 ]
  , psΗv = 3
  , psΗc = 5
  }

bhb :: BHBody
bhb = OCS.bhb
  { bhbVrfVk  = vrfVk
  , bhbVrfRes = bhbVrfRes'
  , bhbVrfPrf = bhbVrfPrf'
  }
  where
    (bhbVrfPrf', bhbVrfRes') = externalFunctions.extEvaluate vrfSk seed

bh :: BHeader
bh = OCS.bh { bhBody = bhb }

ps' :: PrtclState
ps' = MkPrtclState
  { psCs = MkHSMap [ OCS.hk .-> 234, 999 .-> 888 ],
    psΗv = 4,
    psΗc = 4
  }

{-
  NOTE: Why should this test succeed? Here's the explanation:

  η = hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode bhbVrfRes))
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

	∙ hk = hash issuerVk = hash coldVk = succ coldVk

  	∙ lookupPoolDistr pd (succ coldVk) = just (1 / 3 , succ vrfVk)

  		∙ succ vrfVk ≡ hash vrfVk = succ vrfVk ===> True

  		∙ verify vrfVk 2 (bhbVrfPrf , bhbVrfRes) = True

  		  since

  		    seed = 2

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
              = serHashToℕ (hash (encode "L" ∥ encode bhbVrfRes))
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
      prtclStep externalFunctions pe ps bh @?= Success ps'
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ prtclDebug externalFunctions pe ps bh) $ do
--    it "shows its argument" True
