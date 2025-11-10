{-# OPTIONS --safe #-} 

open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)

open import Ledger.Prelude

module Spec.OutsVec
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (G : Set)
  where

ListN : ℕ → ∀ (A : Set) → Set
ListN n A = Σ (List A) (λ xs → length xs ≡ n)

-- AttestedOutputs = ListN 82 (G × G) 

AttestedOutputsM = ListN 82 (Maybe (G × G))


buildList82 : List (Maybe (G × G))
buildList82 = nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷ nothing ∷
            nothing ∷ nothing ∷ []


buildList82-length : length (buildList82) ≡ 82
buildList82-length = refl

allN : AttestedOutputsM
allN = buildList82 , buildList82-length

atIndex' : ℕ → List (Maybe (G × G)) → Maybe (G × G)
atIndex' zero (a ∷ b) = a
atIndex' zero [] = nothing
atIndex' (suc n) [] = nothing
atIndex' (suc n) (a ∷ b) = atIndex' n b

atIndex : ℕ → AttestedOutputsM → Maybe (G × G)
atIndex a b = atIndex' a (proj₁ b)

updateListAt : ∀ {A : Set} → ℕ → A → List A → List A
updateListAt _ _ [] = []
updateListAt zero newVal (_ ∷ xs) = newVal ∷ xs
updateListAt (suc n) newVal (x ∷ xs) = x ∷ updateListAt n newVal xs

updateListAt-length : ∀ {A : Set} (i : ℕ) (newVal : A) (xs : List A) → 
                    length (updateListAt i newVal xs) ≡ length xs
updateListAt-length zero newVal [] = refl
updateListAt-length zero newVal (x ∷ xs) = refl
updateListAt-length (suc i) newVal [] = refl
updateListAt-length (suc i) newVal (x ∷ xs) = 
    cong suc (updateListAt-length i newVal xs)

unsafeUpdateAt82 : ℕ → G → G → 
                AttestedOutputsM → AttestedOutputsM
unsafeUpdateAt82 i x y (xs , lengthProof) = 
    updateListAt i (just( x , y)) xs , 
    trans (updateListAt-length i (just( x , y)) xs) lengthProof

allJust : ∀ {A : Set} → List (Maybe A) → Bool
allJust = foldr check true
  where
    check : ∀ {A : Set} → Maybe A → Bool → Bool
    check nothing acc = false
    check (just _) acc = acc

compAgg : (ℕ → (G × G) → (G × G × G) → G × G × G) → ℕ → Maybe (G × G) → Maybe (G × G × G) → Maybe (G × G × G)
compAgg aggU lam nothing _ = nothing
compAgg aggU lam _ nothing = nothing
compAgg aggU lam (just (xᵢ , yᵢ)) (just (x , y , π)) = just (aggU lam (xᵢ , yᵢ) (x , y , π))


-- foldMaybe : (Maybe G × Maybe G) → Maybe (List (G × G)) → Maybe (List (G × G))
-- foldMaybe (just x , just y) (just acc) = just ((x , y) ∷ acc)
-- foldMaybe _ _ = nothing

-- nothing≢just : ∀ {A : Set} {x : A} → ¬ (nothing ≡ just x)
-- nothing≢just ()

-- just-injective : ∀ {A : Set} {x y : A} → just x ≡ just y → x ≡ y
-- just-injective refl = refl  

-- postulate 
--     fold-length-preserved : ∀ (xs : List (Maybe G × Maybe G)) (ys : List (G × G)) → 
--                         foldr foldMaybe (just []) xs ≡ just ys → 
--                         length xs ≡ length ys

-- fold-length-preserved [] ys prf with prf
-- ... | refl = refl
-- fold-length-preserved ((just x , just y) ∷ xs) ys prf 
-- with foldr foldMaybe (just []) xs in eq
-- ... | nothing = ⊥-elim (helper prf eq)
-- where
--     helper : foldr foldMaybe (just []) ((just x , just y) ∷ xs) ≡ just ys →
--             foldr foldMaybe (just []) xs ≡ nothing → ⊥
--     helper prf₁ eq rewrite eq = nothing≢just prf₁
-- ... | just rest = cong suc (fold-length-preserved xs rest eq)
-- fold-length-preserved ((nothing , snd) ∷ xs) ys prf = ⊥-elim (nothing≢just prf)
-- fold-length-preserved ((just fst , nothing) ∷ xs) ys prf = ⊥-elim (nothing≢just prf)
