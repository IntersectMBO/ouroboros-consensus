module Spec.Foreign.HSConsensus.OperationalCertificate where

open import Spec.Foreign.ExternalFunctions

open import Foreign.Haskell.Coerce

open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.OperationalCertificate DummyCrypto DummyNonces DummyEpochStructure DummyBlockStructure DummyAbstractFunctions

open import Spec.BaseTypes DummyCrypto using (OCertCounters)

unquoteDecl = do
  hsTypeAlias OCertCounters
  hsTypeAlias OCertEnv
  hsTypeAlias OCertState

module _ (ext : ExternalFunctions) where
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (BHeader; BHBody; kesPeriod; _-ᵏ_; _+ᵏ_; MaxKESEvo; isSignedˢ; isSignedᵏ; encode; KeyHashˢ)
  open import Spec.Foreign.HSConsensus.BlockDefinitions
  open import Spec.OperationalCertificate.Properties HSCrypto HSNonces HSEpochStructure HSBlockStructure HSAbstractFunctions

  ocert-step : HsType (OCertEnv → OCertState → BHeader → ComputationResult String OCertState)
  -- FIXME: Investigate why `A` needs to be manually supplied.
  ocert-step = to {A = OCertEnv → OCertState → BHeader → ComputationResult String OCertState} (coerce ⦃ TrustMe ⦄ $ compute Computational-OCERT)

  {-# COMPILE GHC ocert-step as ocertStep #-}

  open import Data.String.Base renaming (_++_ to _+ˢ_) hiding (show; length; _≤_)
  open import Data.Maybe.Relation.Unary.Any as M
  open import Spec.Foreign.HSConsensus.BlockDefinitions
  open import Spec.Foreign.HSTypes using () renaming (ComputationResult to HSComputationResult)

  open import Data.Nat renaming (_≤_ to _≤ℕ_; _<_ to _<ℕ_)

  ocert-debug : HsType (OCertEnv → OCertState → BHeader → String)
  ocert-debug stpools cs bh =
    let 〖 bhb , σ 〗 = from bh; open BHBody bhb
        ⟦ vkₕ , n , c₀ , τ ⟧ᵒᶜ = oc
        hk = hash issuerVk
        kp = kesPeriod slot
        t = kp -ᵏ c₀
        res = ocert-step stpools cs bh
        hyps = ¿ c₀ ≤ℕ kp
               × kp <ℕ c₀ +ᵏ MaxKESEvo
               × M.Any (λ m → n ≡ m ⊎ n ≡ suc m) (currentIssueNo (from stpools) (from cs) hk)
               × isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ
               × isSignedᵏ vkₕ t (encode bhb) σ ¿
    in
      unlines
        $ ("cs = " +ˢ show cs)
        ∷ ("stpools = " +ˢ show stpools)
        ∷ ("issuerVk = " +ˢ show issuerVk)
        ∷ ("hk = " +ˢ show hk)
        ∷ ("kp = " +ˢ show kp)
        ∷ ("t = " +ˢ show t)
        ∷ ("n = " +ˢ show n)
        ∷ ("c₀ = " +ˢ show c₀)
        ∷ ("c₀ +ᵏ MaxKESEvo = " +ˢ show (c₀ +ᵏ MaxKESEvo))
        ∷ ("currentIssueNo stpools cs hk = " +ˢ Show-Maybe .show (currentIssueNo (from stpools) (from cs) hk))
        ∷ ("encode (vkₕ , n , c₀) = " +ˢ show (encode (vkₕ , n , c₀)))
        ∷ ("c₀ ≤ kp = " +ˢ show (⌊ ¿ c₀ ≤ℕ kp ¿ ⌋))
        ∷ ("kp < c₀ +ᵏ MaxKESEvo = " +ˢ show (⌊ ¿ kp <ℕ c₀ +ᵏ MaxKESEvo ¿ ⌋))
        ∷ ("M.Any (λ m → n ≡ m ⊎ n ≡ suc m) (currentIssueNo stpools cs hk) = " +ˢ show (⌊ ¿ M.Any (λ m → n ≡ m ⊎ n ≡ suc m) (currentIssueNo (from stpools) (from cs) hk) ¿ ⌋))
        ∷ ("isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ = " +ˢ show (⌊ ¿ isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ ¿ ⌋))
        ∷ ("isSignedᵏ vkₕ t (encode bhb) σ = " +ˢ show (⌊ ¿ isSignedᵏ vkₕ t (encode bhb) σ ¿ ⌋))
        ∷ ("hyps = " +ˢ show (⌊ hyps ⌋))
        ∷ ("ocert-step' stpools cs bh = " +ˢ show res)
        ∷ []

  {-# COMPILE GHC ocert-debug as ocertDebug #-}
