module ChainHeadSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

(.->) :: a -> b -> (a, b)
(.->) = (,)

che :: ChainHeadEnv
che = 123

lab :: LastAppliedBlock
lab = MkLastAppliedBlock
  { labBℓ = 0
  , labSℓ = 0
  , labH  = 2
  }

chs :: ChainHeadState
chs = MkChainHeadState
  { chsCs  = MkHSMap [ 457 .-> 233 , 999 .-> 888 ]
  , chsΗ₀  = 3
  , chsΗv  = 4
  , chsΗc  = 2
  , chsΗh  = 4
  , chsLab = Just lab
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
  { bhbPrevHeader = Just 2
  , bhbIssuerVk   = 456
  , bhbVrfVk      = 567
  , bhbBlockNo    = 1
  , bhbSlot       = 2
  , bhbVrfRes     = 678
  , bhbVrfPrf     = 789
  , bhbBodySize   = 1
  , bhbBodyHash   = 890
  , bhbOc         = oc
  , bhbPv         = (1, 0)
  }

bh :: BHeader
bh = MkBHeader
  { bhBody = bhb
  , bhSig  = 901
  }

lab' :: LastAppliedBlock
lab' = MkLastAppliedBlock
  { labBℓ = 1
  , labSℓ = 2
  , labH  = 2
  }

chs' :: ChainHeadState
chs' = MkChainHeadState
  { chsCs  = MkHSMap [ 457 .-> 234 , 999 .-> 888 ]
  , chsΗ₀  = 3
  , chsΗv  = 5
  , chsΗc  = 5
  , chsΗh  = 4
  , chsLab = Just lab'
  }

{-
  NOTE: Why should this test succeed? Here's the explanation:

  Given

    nes = 123
    lab = Just ⟦ 0 , 0 , 2 ⟧ℓ
    cs = [ 457  .-> 233 , 999 .-> 888 ]
    ⟦ cs , η₀ , ηv , ηc , ηh , lab ⟧ᶜˢ = ⟦ cs , 3 , 4 , 2 , 4 , lab ⟧ᶜˢ

  then

    _ ⊢ nes ⇀⦇ slot ,TICKF⦈ forecast <===> _ ⊢ 123 ⇀⦇ 2 ,TICKF⦈ 126 <===> True

  Also,

    lastAppliedHash lab = lastAppliedHash (Just ⟦ 0 , 0 , 2 ⟧ℓ) = Just 2

    e₁ = getEpoch nes = getEpoch 123 = 1
    e₂ = getEpoch forecast = getEpoch 126 = 1
    ne = (e₁ ≠ e₂) = (1 ≠ 1) = False
    pp = getPParams forecast = getPParams 126 = { maxHeaderSize = 1; maxBlockSize = 2; pv = (1 , 0) }
    nₚₕ = prevHashToNonce (lastAppliedHash lab) = prevHashToNonce 2 = 0
    pd = extractPoolDistr (getPoolDelegatedStake forecast)
       = extractPoolDistr (getPoolDelegatedStake 126)
       = extractPoolDistr ({(457 , (10 , 568)), (111 , (10 , 222)), (333 , (10 , 444))})
       = {(457 , (1 / 3 , 568)), (111 , (1 / 3 , 222)), (333 , (1 / 3 , 444))}
    lab′ = Just ⟦ blockNo , slot , headerHash bh ⟧ℓ = Just ⟦ 1 , 2 , 2 ⟧ℓ

  then

    prtlSeqChecks lab bh = prtlSeqChecks (just ⟦ 0 , 0 , 2 ⟧ℓ) bh = 0 < 2 × 0 + 1 ≡ 1 × Just 2 ≡ Just 2 = True

    chainChecks MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh
      = chainChecks 1 (1 , 2 , (1 , 0)) bh
      = 1 ≤ 1 × 1 ≤ 1 × 1 ≤ 2
      = True

    ⟦ ηc , nₚₕ ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ ne ,TICKN⦈ ⟦ η₀′ , ηh′ ⟧ᵗˢ
    <===>
    ⟦ 2 , 0 ⟧ᵗᵉ ⊢ ⟦ 3 , 4 ⟧ᵗˢ ⇀⦇ False ,TICKN⦈ ⟦ 3 , 4 ⟧ᵗˢ

    ⟦ pd , η₀′ ⟧ᵖᵉ ⊢ ⟦ cs , ηv , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , ηv′ , ηc′ ⟧ᵖˢ
    <===>
    ⟦ ❴ 457 , (1 / 3 , 568) ❵ , 3 ⟧ᵖᵉ ⊢ ⟦ cs , 4 , 2 ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ [ 457 .-> 234 , 999 .-> 888 ] , 5 , 5 ⟧ᵖˢ

  Finally,

    nes ⊢ ⟦ cs , η₀ , ηv , ηc , ηh , lab ⟧ᶜˢ ⇀⦇ bh ,CHAINHEAD⦈ ⟦ cs′ , η₀′ , ηv′ , ηc′ , ηh′ , lab′ ⟧ᶜˢ
    <===>
    123 ⊢
      ⟦ [ 457 .-> 233 , 999 .-> 888 ] , 3 , 4 , 2 , 4 , Just ⟦ 0 , 0 , 2 ⟧ℓ ⟧ᶜˢ
      ⇀⦇ bh ,CHAINHEAD⦈
      ⟦ [ 457 .-> 234 , 999 .-> 888 ] , 3 , 5 , 2 , 4 , Just ⟦ 1 , 2 , 2 ⟧ℓ ⟧ᶜˢ
-}

spec :: Spec
spec = do
  describe "chainheadStep" $ do
    it "chainheadStep results in the expected state" $
      chainheadStep dummyExternalFunctions che chs bh @?= Success chs'
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ chainheadDebug dummyExternalFunctions che chs bh) $ do
--    it "shows its argument" True
