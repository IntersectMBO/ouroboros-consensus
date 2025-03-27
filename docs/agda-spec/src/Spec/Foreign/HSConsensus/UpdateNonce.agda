module Spec.Foreign.HSConsensus.UpdateNonce where

open import Spec.Foreign.ExternalFunctions

open import Foreign.Haskell.Coerce

open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.UpdateNonce DummyCrypto DummyNonces DummyEpochStructure

instance

  HsTy-UpdateNonceEnv = autoHsType UpdateNonceEnv ⊣ withConstructor "MkUpdateNonceEnv"
                                                  • fieldPrefix "ue"
  Conv-UpdateNonceEnv = autoConvert UpdateNonceEnv

  HsTy-UpdateNonceState = autoHsType UpdateNonceState ⊣ withConstructor "MkUpdateNonceState"
                                                      • fieldPrefix "us"
  Conv-UpdateNonceState = autoConvert UpdateNonceState

module _ (ext : ExternalFunctions) where
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (Slot)
  open import Spec.UpdateNonce.Properties HSCrypto HSNonces HSEpochStructure

  updn-step : HsType (UpdateNonceEnv → UpdateNonceState → Slot → ComputationResult String UpdateNonceState)
  updn-step = to (coerce ⦃ TrustMe ⦄ $ compute Computational-UPDN)

  {-# COMPILE GHC updn-step as updnStep #-}
