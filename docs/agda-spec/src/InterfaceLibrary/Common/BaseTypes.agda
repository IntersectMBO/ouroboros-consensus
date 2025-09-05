{-# OPTIONS --safe #-}

open import Ledger.Prelude
open import Ledger.Crypto
open import Data.Rational using (ℚ)

module InterfaceLibrary.Common.BaseTypes
  (crypto : _) (open Crypto crypto)
  where

PoolDelegatedStake : Type
PoolDelegatedStake = KeyHashˢ ⇀ (Coin × KeyHashᵛ)
