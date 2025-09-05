\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Ledger.Crypto

module Spec.BaseTypes
  (crypto : _) (open Crypto crypto)
  where

open import Ledger.Prelude
open import Data.Rational using (ℚ)

record Nonces : Type₁ where
  infix 6 _⋆_
  field
    Nonce       : Type                  -- nonce
    _⋆_         : Nonce → Nonce → Nonce -- binary nonce operation (BLAKE2b-256 hash of the concatenation of the operands)
    nonceToSeed : Nonce → Seed          -- big-endian encoding of the nonce number in 8 bytes
    ⦃ DecEq-Nonce ⦄ : DecEq Nonce
    ⦃ Show-Nonce  ⦄ : Show Nonce

open import Data.Nat using (ℕ)

-- Operational certificate issue numbers
OCertCounters : Type
OCertCounters = KeyHashˢ ⇀ ℕ

Slot   = ℕ
Epoch  = ℕ

-- TODO: Use `UnitInterval` from the Ledger spec instead of `ℚ`
PoolDistr : Type
PoolDistr = KeyHashˢ ⇀ (ℚ × KeyHashᵛ)

\end{code}
