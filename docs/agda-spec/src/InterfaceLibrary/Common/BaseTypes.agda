{-# OPTIONS --safe #-}

open import Ledger.Prelude
open import Ledger.Crypto
open import Data.Rational using (ℚ)

module InterfaceLibrary.Common.BaseTypes
  (crypto : _) (open Crypto crypto)
  where

-- Stake pool distribution
PoolDistr : Type
PoolDistr = KeyHashˢ ⇀ (ℚ × KeyHashᵛ)

lookupPoolDistr : PoolDistr → KeyHashˢ → Maybe (ℚ × KeyHashᵛ)
lookupPoolDistr pd hk =
  if hk ∈ dom (pd ˢ) then
    just (lookupᵐ pd hk)
  else
    nothing

