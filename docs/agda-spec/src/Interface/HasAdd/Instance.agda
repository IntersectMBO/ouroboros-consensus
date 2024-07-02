{-# OPTIONS --safe #-}
module Interface.HasAdd.Instance where

open import Interface.HasAdd
open import Data.Integer as ℤ using (ℤ)
open import Data.Nat     as ℕ using (ℕ)

instance
  addInt : HasAdd ℤ
  addInt ._+_ = ℤ._+_

  addNat : HasAdd ℕ
  addNat ._+_ = ℕ._+_
