{-# OPTIONS --safe #-}

open import Prelude
open import Data.Product.Ext
open import Interface.Hashable
open import Ledger.Set.Theory

module Ledger.Set.HashMap {A B : Type} ⦃ _ : Hashable A B ⦄ ⦃ _ : DecEq B ⦄ (X : ℙ A) where

setToHashRel : ℙ (B × A)
setToHashRel = mapˡ hash (mapˢ ×-dup X)

setToHashMap : B ⇀ A
setToHashMap = fromListᵐ (setToList setToHashRel)
