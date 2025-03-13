module Spec.Foreign.HSConsensus.Protocol where

open import Spec.Foreign.ExternalFunctions

open import Foreign.Haskell.Coerce

open import Data.Rational as ℚ using (ℚ; 0ℚ; 1ℚ; positive)
open import Data.Rational.Ext using (InPosUnitInterval)
open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.Protocol DummyCrypto DummyNonces DummyEpochStructure DummyBlockStructure DummyAbstractFunctions DummyRationalExtStructure

-- TODO: Move to its own file Spec.Foreign.HSConsensus.InterfaceLibrary.Common.BaseTypes
open import InterfaceLibrary.Common.BaseTypes DummyCrypto using (PoolDistr)

unquoteDecl = do
  hsTypeAlias PoolDistr

instance
  -- TODO: Perhaps move to its own file (maybe Spec.HSConsensus.Data.Rational.Ext)
  HsTy-InPosUnitInterval = MkHsType InPosUnitInterval ℚ

  Conv-InPosUnitInterval : HsConvertible InPosUnitInterval
  Conv-InPosUnitInterval = λ where .to → proj₁; .from → (λ p → if p ℚ.> 0ℚ × p ≤ 1ℚ then (λ{ (p>0 , p≤1) } → from p , positive p>0 , p≤1) else error "Not in the positive unit interval")

  HsTy-PrtclEnv = autoHsType PrtclEnv ⊣ withConstructor "MkPrtclEnv"
                                      • fieldPrefix "pe"
  Conv-PrtclEnv = autoConvert PrtclEnv

  HsTy-PrtclState = autoHsType PrtclState ⊣ withConstructor "MkPrtclState"
                                          • fieldPrefix "ps"
  Conv-PrtclState = autoConvert PrtclState

module _ (ext : ExternalFunctions) where
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (BHeader; BHBody; ActiveSlotCoeff; slotToSeed; nonceToSeed; _XOR_; verify; VRFRes)
  open import Spec.Foreign.HSConsensus.BlockDefinitions
  open import Spec.Protocol.Properties HSCrypto HSNonces HSEpochStructure HSBlockStructure HSAbstractFunctions HSRationalExtStructure

  prtcl-step : HsType (PrtclEnv → PrtclState → BHeader → ComputationResult String PrtclState)
  prtcl-step = to (coerce ⦃ TrustMe ⦄ $ compute Computational-PRTCL)

  {-# COMPILE GHC prtcl-step as prtclStep #-}

  open import Data.String.Base renaming (_++_ to _+ˢ_) hiding (show; length; _≤_)
  open import Spec.Foreign.HSTypes using (Show-HSMap)
  open import Spec.BlockDefinitions DummyCrypto DummyNonces DummyEpochStructure
  open import InterfaceLibrary.Common.BaseTypes HSCrypto using (lookupPoolDistr)

  instance
    _ = Show-Maybe
    _ = Show-×

  prtcl-debug : HsType (PrtclEnv → PrtclState → BHeader → String)
  prtcl-debug pe ps bh =
    let
      〖 bhb , σ 〗 = from bh; open BHBody bhb
      η = hBNonce bhb
      ⟦ pd , η₀ ⟧ᵖᵉ = from pe
      ⟦ cs , ηv , ηc ⟧ᵖˢ = from ps
      hk = hash issuerVk
    in
      unlines
        $ ("\nbh                  \t\t\t: " +ˢ show (from bh))
        ∷ ("pe                    \t\t\t: ⟦ " +ˢ Show-HSMap .show (to pd) +ˢ " , " +ˢ show η₀ +ˢ " ⟧ᵖᵉ")
        ∷ ("ps                    \t\t\t: ⟦ " +ˢ Show-HSMap .show (to cs) +ˢ " , " +ˢ show ηv +ˢ " , " +ˢ show ηc +ˢ " ⟧ᵖˢ")
        ∷ ("η                     \t\t\t: " +ˢ show η)
        ∷ ("hk                    \t\t\t: " +ˢ show hk)
        ∷ ("lookupPoolDistr pd hk \t\t\t: " +ˢ show (lookupPoolDistr pd hk))
        ∷ (
          case lookupPoolDistr pd hk of λ where
            nothing → ""
            (just (σ , vrfHK)) →
              let
                stsslot = slotToSeed slot
                ntsη₀   = nonceToSeed η₀
                seed    = stsslot XOR ntsη₀
                f       = ActiveSlotCoeff .proj₁
                t       = hBLeader bhb
                p       = ℤ.pos (t .proj₁) ℚ./ (2 ^ 512)
                q       = 1ℚ ℚ.- p
                c       = ln (1ℚ ℚ.- f)
                e       = exp ((ℚ.- σ) ℚ.* c)
              in
                unlines
                  $ ("hash vrfVk                          \t: " +ˢ show (hash vrfVk))
                  ∷ ("vrfHK == hash vrfVk                 \t: " +ˢ show (vrfHK == hash vrfVk))
                  ∷ ("slotToSeed slot                     \t: " +ˢ show stsslot)
                  ∷ ("nonceToSeed η₀                      \t: " +ˢ show ntsη₀)
                  ∷ ("seed                                \t: " +ˢ show seed)
                  ∷ ("verify vrfVk seed (vrfPrf , vrfRes) \t: " +ˢ show (⌊ ¿ verify {T = VRFRes} ¿³ vrfVk seed (vrfPrf , vrfRes) ⌋))
                  ∷ ("hBLeader bhb                        \t: " +ˢ Show-Certifiedℕ {DummyBlockStructure} .show t)
                  ∷ ("f                                   \t: " +ˢ show ActiveSlotCoeff)
                  ∷ ("checkLeaderVal? (hBLeader bhb) f σ  \t: " +ˢ show (⌊ checkLeaderVal? t ActiveSlotCoeff σ ⌋))
                  ∷ ("p                                   \t: " +ˢ show p)
                  ∷ ("q                                   \t: " +ˢ show q)
                  ∷ ("c                                   \t: " +ˢ show c)
                  ∷ ("exp (- σ * c)                       \t: " +ˢ show e)
                  ∷ ("1 / q < exp (- σ * c)               \t: " +ˢ show (⌊ ℚ.1/ q ℚ.<? e ⌋))
                  ∷ []
          )
        ∷ []

  {-# COMPILE GHC prtcl-debug as prtclDebug #-}
