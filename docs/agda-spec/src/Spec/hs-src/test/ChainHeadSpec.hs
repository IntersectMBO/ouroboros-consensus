module ChainHeadSpec (spec) where

import Data.Text

import Test.Hspec ( Spec, describe, it )
import Test.HUnit ( (@?=) )

import Lib

(.->) :: a -> b -> (a, b)
(.->) = (,)

che :: ChainHeadEnv
che = 123

isFailure :: ComputationResult e a -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False

lab :: LastAppliedBlock
lab = MkLastAppliedBlock
  { labBℓ  = 0
  , labSℓ  = 0
  , labH   = 2
  , labAeb = Just aeb
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

aeb :: AnnouncedEB
aeb = MkAnnouncedEB
  { aebHash = 1000
  , aebSize = 10
  }

bhb :: BHBody
bhb = MkBHBody
  { bhbPrevHeader  = Just 2
  , bhbIssuerVk    = 456
  , bhbVrfVk       = 567
  , bhbBlockNo     = 1
  , bhbSlot        = 2
  , bhbVrfRes      = 678
  , bhbVrfPrf      = 789
  , bhbBodySize    = 1
  , bhbBodyHash    = 890
  , bhbOc          = oc
  , bhbPv          = (1, 0)
  , bhbAnnouncedEB = Just aeb
  , bhbCertifiedEB = False
  }

bh :: BHeader
bh = MkBHeader
  { bhBody = bhb
  , bhSig  = 901
  }

lab' :: LastAppliedBlock
lab' = MkLastAppliedBlock
  { labBℓ  = 1
  , labSℓ  = 2
  , labH   = 2
  , labAeb = Just aeb
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
    aeb = ⟨ hash = 1000 , size = 10 ⟩
    lab = Just ⟦ 0 , 0 , 2 , Just aeb ⟧ℓ
    cs = [ 457  .-> 233 , 999 .-> 888 ]
    ⟦ cs , η₀ , ηv , ηc , ηh , lab ⟧ᶜˢ = ⟦ cs , 3 , 4 , 2 , 4 , lab ⟧ᶜˢ

  then

    _ ⊢ nes ⇀⦇ slot ,TICKF⦈ forecast <===> _ ⊢ 123 ⇀⦇ 2 ,TICKF⦈ 126 <===> True

  Also,

    lastAppliedHash lab = lastAppliedHash (Just ⟦ 0 , 0 , 2 , Just aeb ⟧ℓ) = Just 2

    e₁ = getEpoch nes = getEpoch 123 = 1
    e₂ = getEpoch forecast = getEpoch 126 = 1
    ne = (e₁ ≠ e₂) = (1 ≠ 1) = False
    pp = getPParams forecast = getPParams 126 = { maxHeaderSize = 1; maxBlockSize = 2; pv = (1 , 0) }
    certificationDelay = 3 * Lhdr + Lvote + Ldiff = 3 * 1 + 2 + 3 = 8
    nₚₕ = prevHashToNonce (lastAppliedHash lab) = prevHashToNonce 2 = 0
    pd = extractPoolDistr (getPoolDelegatedStake forecast)
       = extractPoolDistr (getPoolDelegatedStake 126)
       = extractPoolDistr ({(457 , (10 , 568)), (111 , (10 , 222)), (333 , (10 , 444))})
       = {(457 , (1 / 3 , 568)), (111 , (1 / 3 , 222)), (333 , (1 / 3 , 444))}
    lab′ = Just ⟦ blockNo , slot , headerHash bh , announcedEB ⟧ℓ = Just ⟦ 1 , 2 , 2 , Just aeb ⟧ℓ

  then

    prtlSeqChecks lab bh = prtlSeqChecks (just ⟦ 0 , 0 , 2 , Just aeb ⟧ℓ) bh
      = 0 < 2 × 0 + 1 ≡ 1 × Just 2 ≡ Just 2
      = True

    chainChecks MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh
      = chainChecks 1 (1 , 2 , (1 , 0)) bh
      = 1 ≤ 1 × 1 ≤ 1 × 1 ≤ 2
      = True

    certChecks lab certifiedEB slot
      = certChecks lab False 2
      = ⊤                                   -- the header certifies nothing
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
      ⟦ [ 457 .-> 233 , 999 .-> 888 ] , 3 , 4 , 2 , 4 , Just ⟦ 0 , 0 , 2 , Just aeb ⟧ℓ ⟧ᶜˢ
      ⇀⦇ bh ,CHAINHEAD⦈
      ⟦ [ 457 .-> 234 , 999 .-> 888 ] , 3 , 5 , 5 , 4 , Just ⟦ 1 , 2 , 2 , Just aeb ⟧ℓ ⟧ᶜˢ
-}

-- Headers that certify the endorser block announced by `lab`. The certification
-- delay is 3 * Lhdr + Lvote + Ldiff = 3 * 1 + 2 + 3 = 8 slots, and `lab` is at
-- slot 0, so slot 8 is the earliest slot at which certification is permitted.

certifyingBhbAt :: Slot -> BHBody
certifyingBhbAt s = bhb { bhbSlot = s , bhbCertifiedEB = True }

-- Certifies at slot 2, i.e. 6 slots too early.
bhTooEarly :: BHeader
bhTooEarly = bh { bhBody = certifyingBhbAt 2 }

-- Certifies at slot 10, comfortably past the delay.
bhInTime :: BHeader
bhInTime = bh { bhBody = certifyingBhbAt 10 }

-- As `chs`, but the last applied block announced no endorser block, so there is
-- nothing for `bhInTime` to certify.
chsNoAnnouncement :: ChainHeadState
chsNoAnnouncement = chs { chsLab = Just lab { labAeb = Nothing } }

-- Expected result of applying `bhInTime` to `chs`. Only the slot recorded in the
-- last applied block differs from `chs'`: the nonces evolve identically because
-- hBNonce does not depend on the slot, and slot 10 is still far enough from the
-- end of the epoch (100) for UPDN to update both nonces.
chsCertified :: ChainHeadState
chsCertified = chs'
  { chsLab = Just lab' { labSℓ = 10 }
  }

spec :: Spec
spec = do
  describe "chainheadStep" $ do
    it "chainheadStep results in the expected state" $
      chainheadStep dummyExternalFunctions che chs bh @?= Success chs'
    it "chainheadStep accepts a header certifying after the required delay" $
      chainheadStep dummyExternalFunctions che chs bhInTime @?= Success chsCertified
    it "chainheadStep rejects a header certifying before the required delay" $
      isFailure (chainheadStep dummyExternalFunctions che chs bhTooEarly) @?= True
    it "chainheadStep rejects a header certifying an unannounced endorser block" $
      isFailure (chainheadStep dummyExternalFunctions che chsNoAnnouncement bhInTime) @?= True
-- NOTE: Uncomment to run the debug version.
--  describe (unpack $ chainheadDebug dummyExternalFunctions che chs bh) $ do
--    it "shows its argument" True
