{-# OPTIONS --safe #-}

module Ledger.Prelude.Base where

open import Data.Nat
open import Data.Nat.DivMod using (_/_)

Coin = ℕ

-- A wall-clock duration in whole milliseconds. The implementation bounds this
-- to 32 bits (`Milliseconds32`); we do not model the bound here.
Milliseconds = ℕ

-- The number of slots spanning a duration, rounded up. CIP-164 converts the
-- Leios periods to a slot count using the genesis slot length. Defined here
-- rather than against the genesis constant itself, so that it travels with
-- `Milliseconds` when both move to the common library.
durationToSlots : (slotLength : Milliseconds) → ⦃ NonZero slotLength ⦄ → Milliseconds → ℕ
durationToSlots l d = (d + l ∸ 1) / l
