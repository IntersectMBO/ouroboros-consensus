module Spec.Foreign.HSConsensus.BlockDefinitions where

open import Spec.Foreign.HSConsensus.BaseTypes

instance
  HsTy-Certifiedℕ = MkHsType Certifiedℕ ℕ

  -- NOTE: Roll-over when converting a number that is out of bounds.
  Conv-Certifiedℕ : HsConvertible Certifiedℕ
  Conv-Certifiedℕ = λ where .to → proj₁; .from → (λ n → if n <ℕ 2 ^ 512 then (λ{n<2^512} → from n , n<2^512) else 0 , s≤s z≤n)
    where open import Data.Nat renaming (_<_ to _<ℕ_)

  HsTy-OCert = autoHsType OCert ⊣ withConstructor "MkOCert"
                                • fieldPrefix "oc"

  Conv-OCert = autoConvert OCert

  HsTy-BHBody = autoHsType BHBody ⊣ withConstructor "MkBHBody"
                                  • fieldPrefix "bhb"

  Conv-BHBody = autoConvert BHBody

  HsTy-BHeader = autoHsType BHeader ⊣ withConstructor "MkBHeader"
                                  • fieldPrefix "bh"

  Conv-BHeader = autoConvert BHeader

  HsTy-AbstractFunctions = autoHsType AbstractFunctions ⊣ withConstructor "MkAbstractFunctions"

  Conv-AbstractFunctions = autoConvert AbstractFunctions
