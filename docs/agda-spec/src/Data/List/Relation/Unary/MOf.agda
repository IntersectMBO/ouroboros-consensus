{-# OPTIONS --safe #-}
module Data.List.Relation.Unary.MOf where

open import Level using (Level; _⊔_) renaming (suc to lsuc)
open import Function using (_$_; case_of_)

open import Data.Empty using (⊥-elim)
open import Data.List using (List; []; _∷_; length)
open import Data.List.Relation.Unary.All using (All; []; _∷_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Nat.Properties using (suc-injective)

open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Nullary.Decidable using () renaming (map′ to mapDec)
open import Relation.Unary using (Decidable; Pred)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

open import Data.List.Relation.Binary.Sublist.Ext

-- States that "m-of-n" elements of a n-element list satisfy a given predicate.
data MOf {ℓ ℓ′}{A : Set ℓ} (m : ℕ) (P : Pred A ℓ′) (xs : List A) : Set (ℓ ⊔ ℓ′) where
  mOf : ∀ ys
    → m ≡ length ys
    → ys ⊆ xs
    → All P ys
    → MOf m P xs

private module _ {ℓ ℓ′} {A : Set ℓ} {P : Pred A ℓ′} where
  cons : ∀ {x xs m} → P x → MOf m P xs → MOf (suc m) P (x ∷ xs)
  cons p (mOf ys refl sub ps) = mOf (_ ∷ ys) refl (refl ∷ sub) (p ∷ ps)

  skip : ∀ {x xs m} → MOf m P xs → MOf m P (x ∷ xs)
  skip (mOf ys len sub ps) = mOf ys len (_ ∷ʳ sub) ps

  done : ∀ {xs} → MOf 0 P xs
  done = mOf [] refl []⊆ []

  wk : ∀ {m xs} → MOf (suc m) P xs → MOf m P xs
  wk (mOf (_ ∷ ys) refl (_ ∷ sub) (_ ∷ ps)) = mOf ys refl (_ ∷ʳ sub) ps
  wk (mOf ys len (_ ∷ʳ sub) ps) = skip (wk (mOf ys len sub ps))

  uncons : ∀ {m x xs} → MOf (suc m) P (x ∷ xs) → MOf m P xs
  uncons (mOf ys len (_ ∷ʳ sub) ps) = wk $ mOf ys len sub ps
  uncons (mOf (_ ∷ ys) refl (_ ∷ sub) (_ ∷ ps)) = mOf ys refl sub ps

  unskip : ∀ {m x xs} → ¬ P x → MOf (suc m) P (x ∷ xs) → MOf (suc m) P xs
  unskip ¬px (mOf ys len (_ ∷ʳ sub) ps) = mOf ys len sub ps
  unskip ¬px (mOf ys len (refl ∷ sub) (px ∷ ps)) = ⊥-elim $ ¬px px

module _ {ℓ ℓ′} {A : Set ℓ} {P : Pred A ℓ′} (P? : Decidable P) where
  MOf? : ∀ m xs → Dec (MOf m P xs)
  MOf? zero    xs = yes done
  MOf? (suc m) [] = no λ where (mOf (_ ∷ _) len≡ () _)
  MOf? (suc m) (x ∷ xs) =
    case (P? x) of λ where
      (yes px) → mapDec (cons px) uncons (MOf? m xs)
      (no ¬px) → mapDec skip (unskip ¬px) (MOf? (suc m) xs)
