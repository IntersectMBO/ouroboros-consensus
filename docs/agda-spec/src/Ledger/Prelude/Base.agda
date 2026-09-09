{-# OPTIONS --safe #-}

module Ledger.Prelude.Base where

open import Data.Nat

Coin = ℕ

-- A wall-clock duration in whole milliseconds. The implementation bounds this
-- to 32 bits (`Milliseconds32`); we do not model the bound here.
Milliseconds = ℕ
