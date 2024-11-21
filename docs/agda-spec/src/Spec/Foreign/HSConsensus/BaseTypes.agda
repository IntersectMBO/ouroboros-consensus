module Spec.Foreign.HSConsensus.BaseTypes where

open import Spec.Foreign.ExternalFunctions
open import Spec.Foreign.HSConsensus.Core public
import Spec.Foreign.HSTypes as F

instance -- TODO: Complete
  iConvNat    = Convertible-Refl {ℕ}
  iConvString = Convertible-Refl {String}
  iConvBool   = Convertible-Refl {Bool}

instance -- TODO: Complete

  -- * ComputationResult

  HsTy-ComputationResult : ∀ {l} {Err} {A : Type l}
                           → ⦃ HasHsType Err ⦄ → ⦃ HasHsType A ⦄
                           → HasHsType (ComputationResult Err A)
  HsTy-ComputationResult {Err = Err} {A} = MkHsType _ (F.ComputationResult (HsType Err) (HsType A))

  Conv-ComputationResult : ConvertibleType ComputationResult F.ComputationResult
  Conv-ComputationResult = autoConvertible

open ExternalStructures dummyExternalFunctions -- TODO: Complete
  public
