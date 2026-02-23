{-# OPTIONS --safe #-}

--------------------------------------------------------------------------------
-- Ledger prelude
--
-- Re-exports modules relating to STS, set theory and other
-- miscellaneous things used to write the ledger rules. If something
-- is used in more than two Ledger.* modules, it should probably go
-- here.
--------------------------------------------------------------------------------

module Ledger.Prelude where

open import Prelude public

open import Ledger.Prelude.Base public
open import Ledger.Prelude.Instances public

open import Interface.ComputationalRelation public
open import Class.HasAdd public
open import Class.HasOrder public
open import Interface.HasSubtract public
open import Interface.HasSubtract.Instance public
open import Interface.Hashable public
open import Interface.IsCommutativeMonoid public
open import Class.ToBool public
open import Ledger.Interface.HasCoin public
open import MyDebugOptions public
open import Prelude.STS.GenPremises public
open import Class.CommutativeMonoid public

open import abstract-set-theory.FiniteSetTheory public
  renaming (_⊆_ to _⊆ˢ_)

dec-de-morgan : ∀{P Q : Type} → ⦃ P ⁇ ⦄ → ¬ (P × Q) → ¬ P ⊎ ¬ Q
dec-de-morgan ⦃ ⁇ no ¬p ⦄ ¬pq = inj₁ ¬p
dec-de-morgan ⦃ ⁇ yes p ⦄ ¬pq = inj₂ λ q → ¬pq (p , q)
