\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Ledger.Crypto

module Spec.BaseTypes
  (crypto : _) (open Crypto crypto)
  where

open import Ledger.Prelude

record Nonces : Type₁ where
  infix 6 _⋆_
  field
    Nonce       : Type                  -- nonce
    _⋆_         : Nonce → Nonce → Nonce -- binary nonce operation (BLAKE2b-256 hash of the concatenation of the operands)
    nonceToSeed : Nonce → Seed          -- big-endian encoding of the nonce number in 8 bytes

open import Data.Nat using (ℕ)

-- Operational certificate issue numbers
OCertCounters : Type
OCertCounters = KeyHashˢ ⇀ ℕ

\end{code}
