{-# OPTIONS --safe #-}

module Data.Product.Ext where

open import Data.Product.Base

×-dup : ∀ {ℓ} {A : Set ℓ} → A → A × A
×-dup x = x , x
