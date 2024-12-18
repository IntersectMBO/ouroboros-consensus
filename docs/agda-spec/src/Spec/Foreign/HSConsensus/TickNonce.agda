module Spec.Foreign.HSConsensus.TickNonce where

open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.TickNonce it it it

open import Spec.TickNonce.Properties it it it using (Computational-TICKN)

instance

  HsTy-TickNonceEnv = autoHsType TickNonceEnv ⊣ withConstructor "MkTickNonceEnv"
  Conv-TickNonceEnv = autoConvert TickNonceEnv

  HsTy-TickNonceState = autoHsType TickNonceState ⊣ withConstructor "MkTickNonceState"
  Conv-TickNonceState = autoConvert TickNonceState

tickn-step : HsType (TickNonceEnv → TickNonceState → Bool → ComputationResult String TickNonceState)
tickn-step = to (compute Computational-TICKN)

{-# COMPILE GHC tickn-step as ticknStep #-}
