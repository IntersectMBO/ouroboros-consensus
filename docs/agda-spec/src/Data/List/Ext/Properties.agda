{-# OPTIONS --safe #-}

module Data.List.Ext.Properties where

open import Prelude hiding (lookup; map)

import Data.Product
import Data.Sum
import Function.Related.Propositional as R
open import Data.List using (List; [_]; []; _++_; head; tail; length; map; filter)
open import Data.List.Ext using (insert; subpermutations; sublists)
open import Data.List.Properties using (concat-++; map-++; ++-identityʳ; ++-assoc)
open import Data.List.Membership.Propositional using (_∈_)
open import Data.List.Membership.Propositional.Properties
  using (∈-++⁻; ∈-++⁺ˡ; ∈-++⁺ʳ; ∈-deduplicate⁻; ∈-deduplicate⁺; ∈-map⁺)
open import Data.List.Relation.Binary.BagAndSetEquality using (∼bag⇒↭)
open import Data.List.Relation.Binary.Disjoint.Propositional using (Disjoint)
open import Data.List.Relation.Binary.Permutation.Propositional using (_↭_)
open import Data.List.Relation.Binary.Subset.Propositional using (_⊆_)
open import Data.List.Relation.Binary.Subset.Propositional.Properties as P
  using (xs⊆ys++xs; xs⊆xs++ys; ⊆-reflexive; ⊆-trans)
open import Data.List.Relation.Unary.AllPairs using (AllPairs; []; _∷_)
open import Data.List.Relation.Unary.All using (all?; All; lookup) renaming (tail to Alltail)
open import Data.List.Relation.Unary.Any using (Any; here; there)
open import Data.List.Relation.Unary.Unique.Propositional using (Unique)
open import Data.List.Relation.Unary.Unique.Propositional.Properties using (drop⁺)
open import Data.List.Relation.Unary.Unique.Propositional.Properties.WithK using (unique∧set⇒bag)
open import Data.Nat.Properties using (_≤?_; ⊔-identityʳ; ≤-reflexive; ≤-trans; m≤n⊔m; m≤m⊔n)
open import Data.Nat using (_⊔_; _≤_)

open Equivalence

-- TODO: stdlib?
_×-cong_ : ∀ {a b c d} {A : Set a} {B : Set b} {C : Set c} {D : Set d} {k} → A R.∼[ k ] B → C R.∼[ k ] D → (A × C) R.∼[ k ] (B × D)
h ×-cong h' = (h M.×-cong h')
  where open import Data.Product.Function.NonDependent.Propositional as M

_⊎-cong_ : ∀ {a b c d} {A : Set a} {B : Set b} {C : Set c} {D : Set d} {k} → A R.∼[ k ] B → C R.∼[ k ] D → (A ⊎ C) R.∼[ k ] (B ⊎ D)
h ⊎-cong h' = (h M.⊎-cong h')
  where open import Data.Sum.Function.Propositional as M

-- TODO: stdlib?
AllPairs⇒≡∨R∨Rᵒᵖ : ∀ {ℓ ℓ'} {A : Set ℓ} {R : A → A → Set ℓ'} {a b l}
                 → AllPairs R l → a ∈ l → b ∈ l → a ≡ b ⊎ R a b ⊎ R b a
AllPairs⇒≡∨R∨Rᵒᵖ (_ ∷ _) (here refl) (here refl) = inj₁ refl
AllPairs⇒≡∨R∨Rᵒᵖ (x ∷ _) (here refl) (there b∈l) = inj₂ (inj₁ (lookup x b∈l))
AllPairs⇒≡∨R∨Rᵒᵖ (x ∷ _) (there a∈l) (here refl) = inj₂ (inj₂ (lookup x a∈l))
AllPairs⇒≡∨R∨Rᵒᵖ (x ∷ h) (there a∈l) (there b∈l) = AllPairs⇒≡∨R∨Rᵒᵖ h a∈l b∈l

--------------------------------------------------------------
------- duplicate entries in lists and deduplication ---------
--------------------------------------------------------------
module _ {a} {A : Set a} ⦃ _ : DecEq A ⦄ where
  open import Data.List.Relation.Unary.Unique.DecPropositional.Properties {A = A} _≟_

  deduplicate≡ : List A → List A
  deduplicate≡ = deduplicate _≟_

  disj-on-dedup : ∀ {l l'} → Disjoint l l' → Disjoint (deduplicate≡ l) (deduplicate≡ l')
  disj-on-dedup = _∘ Data.Product.map (∈-deduplicate⁻ _≟_ _) (∈-deduplicate⁻ _≟_ _)

  ∈-dedup : ∀ {l a} → a ∈ l ⇔ a ∈ deduplicate≡ l
  ∈-dedup = mk⇔ (∈-deduplicate⁺ _≟_) (∈-deduplicate⁻ _≟_ _)

  -- TODO: stdlib?
  dedup-++-↭ : {l l' : List A} → Disjoint l l' → deduplicate≡ (l ++ l') ↭ deduplicate≡ l ++ deduplicate≡ l'
  dedup-++-↭ {l = l} {l'} disj = let dedup-unique = λ {l} → deduplicate-! l in ∼bag⇒↭ $
    unique∧set⇒bag dedup-unique (++⁺ dedup-unique dedup-unique (disj-on-dedup disj)) λ {a} →
      a ∈ deduplicate≡ (l ++ l')                 ∼⟨ R.SK-sym ∈-dedup ⟩
      a ∈ l ++ l'                                ∼⟨ helper ⟩
      (a ∈ l ⊎ a ∈ l')                           ∼⟨ ∈-dedup ⊎-cong ∈-dedup ⟩
      (a ∈ deduplicate≡ l ⊎ a ∈ deduplicate≡ l') ∼⟨ R.SK-sym helper ⟩
      a ∈ deduplicate≡ l ++ deduplicate≡ l'       ∎
    where open R.EquationalReasoning
          helper : ∀ {l l' a} → a ∈ l ++ l' ⇔ (a ∈ l ⊎ a ∈ l')
          helper = mk⇔ (∈-++⁻ _) Data.Sum.[ ∈-++⁺ˡ , ∈-++⁺ʳ _ ]

------------------------------------------------------------
-------- maximum length of lists in a list of lists --------
------------------------------------------------------------
module _ {a} {A : Set a} where
  maxlen : List (List A) → ℕ
  maxlen ls = foldr (λ l n → length l ⊔ n) 0 ls

  maxlen≤∷ : ∀ ls l → maxlen ls ≤ maxlen (l ∷ ls)
  maxlen≤∷ [] _ = z≤n
  maxlen≤∷ (l' ∷ ls) l = subst (maxlen (l' ∷ ls) ≤_) refl (m≤n⊔m (length l) (maxlen (l' ∷ ls)))

  ∈-maxlen-≤ : ∀ {ls} l → l ∈ ls → length l ≤ maxlen ls
  ∈-maxlen-≤ {l ∷ ls} .l (here refl) = subst (length l ≤_) refl (m≤m⊔n (length l) (maxlen ls))
  ∈-maxlen-≤ {l' ∷ ls} l (there l∈)  = ≤-trans (∈-maxlen-≤ l l∈) (maxlen≤∷ ls l')

-------------------------------
------ properties of map ------
-------------------------------
module _ {a} {A : Set a} where

  ¬[]∈map : {ls : List (List A)} {z : A} → ¬ [] ∈ map (z ∷_) ls
  ¬[]∈map {_ ∷ _} (there p) = ¬[]∈map p

  map∷decomp∈ : {ls : List (List A)} {xs : List A} {y x : A} → x ∷ xs ∈ map (y ∷_) ls → x ≡ y × xs ∈ ls
  map∷decomp∈ {_ ∷ _} (here refl) = refl , (here refl)
  map∷decomp∈ {_ ∷ _} (there xxs∈) = (proj₁ (map∷decomp∈ xxs∈)) , there (proj₂ (map∷decomp∈ xxs∈))

  map∷decomp : {ls : List (List A)} {xs : List A} {y : A}
               → xs ∈ map (y ∷_) ls → ∃[ ys ] ys ∈ ls × y ∷ ys ≡ xs
  map∷decomp {l ∷ _} {.(_ ∷ l)} (here refl) = l , ((here refl) , refl)
  map∷decomp {_ ∷ _} {[]} (there xs∈) = ⊥-elim (¬[]∈map xs∈)
  map∷decomp {_ ∷ _} {x ∷ xs} (there xs∈) =
    xs , there (proj₂ (map∷decomp∈ xs∈)) , subst (λ u → u ∷ xs ≡ x ∷ xs) (proj₁ (map∷decomp∈ xs∈)) refl

  ∈-map : {ls : List (List A)} {y : A} → ∀ l → l ∈ map (y ∷_) ls → y ∈ l
  ∈-map {l ∷ _} .(_ ∷ l) (here refl) = here refl
  ∈-map {_ ∷ _} l (there l∈) = ∈-map l l∈

------------------------------------------------------
-------- Properties of list-subset inclusion ---------
------------------------------------------------------
module _ {a} {A : Set a} where
  ⊆y∷∧y∉→⊆ys : ∀ {ys xs} {y : A} → xs ⊆ y ∷ ys → ¬ y ∈ xs → xs ⊆ ys
  ⊆y∷∧y∉→⊆ys xsyys y∉xs x∈xs with xsyys x∈xs
  ... | here refl = ⊥-elim (y∉xs x∈xs)
  ... | there p = p

  ¬∈[] : {x : A} → ¬ x ∈ []
  ¬∈[] = λ ()

  ¬⊆[] : {xs : List A} {x : A} → ¬ x ∷ xs ⊆ []
  ¬⊆[] p = ¬∈[] (p (here refl))

  ∈∷∧⊆→∈ : {ys xs : List A} {y x : A} → x ∈ y ∷ xs → xs ⊆ ys → x ∈ y ∷ ys
  ∈∷∧⊆→∈ (here px) _ = here px
  ∈∷∧⊆→∈ (there x∈) xs⊆ = there (xs⊆ x∈)

------------------------------------------
-------- Properties of concatMap ---------
------------------------------------------
module _ {a} {A : Set a}  where

  concatMap⁺ : {ys xs : List A} {f : A → List A} → xs ⊆ ys → concatMap f xs ⊆ concatMap f ys
  concatMap⁺ = P.concat⁺ ∘ P.map⁺ _

  concatMap-++ : ∀ {b} {B : Set b} (f : A → List B) xs ys
                 → concatMap f (xs ++ ys) ≡ concatMap f xs ++ concatMap f ys
  concatMap-++ f xs ys = begin
    concatMap f (xs ++ ys)           ≡⟨⟩
    concat (map f (xs ++ ys))        ≡⟨ cong concat $ map-++ f xs ys ⟩
    concat (map f xs ++ map f ys)    ≡˘⟨ concat-++ (map f xs) (map f ys) ⟩
    concatMap f xs ++ concatMap f ys ∎ where open ≡-Reasoning

  concatMap-decomp : ∀ {b} {B : Set b} {l : List A} {x : B} {f : A → List B}
                     → x ∈ concatMap f l ⇔ (∃[ z ] z ∈ l × x ∈ f z)
  concatMap-decomp {l = []} = mk⇔ (λ ()) (λ (_ , v , _) → ⊥-elim (¬∈[] v))
  concatMap-decomp {l = y ∷ ys} {x} {f} = mk⇔ i ii
    where
    i : x ∈ concatMap f (y ∷ ys) → ∃[ z ] z ∈ y ∷ ys × x ∈ f z
    i x∈ with ∈-++⁻ _ x∈
    ...| inj₁ v = y , (here refl , v)
    ...| inj₂ v with to concatMap-decomp v
    ...| z , z∈ys , x∈fz = z , there z∈ys , x∈fz

    ii : ∃[ z ] z ∈ y ∷ ys × x ∈ f z → x ∈ concatMap f (y ∷ ys)
    ii (z , here refl , x∈) = xs⊆xs++ys (f z) _ x∈
    ii (z , there z∈ , x∈)  = xs⊆ys++xs _ (f y) (from concatMap-decomp (z , (z∈ , x∈)))

------------------------------------
------- properties of insert -------
------------------------------------
module _ {a} {A : Set a} where
  ∷∈insert : {ys : List A} {x : A} → x ∷ ys ∈ insert x ys
  ∷∈insert {[]}    = here refl
  ∷∈insert {_ ∷ _} = here refl

  ∈-insert : ∀ {ys} {x : A} l → l ∈ insert x ys → x ∈ l
  ∈-insert {[]}    _ (here refl) = here refl
  ∈-insert {_ ∷ _} _ (here refl) = here refl
  ∈-insert {_ ∷ _} {x} _ (there l∈) with map∷decomp l∈
  ... | (l' , l'∈ , yl'l) = subst (x ∈_) yl'l (there (∈-insert l' l'∈))

  ∈→∈-insert : ∀ {ys : List A} {y x : A} l → x ∈ ys → l ∈ insert y ys → x ∈ l
  ∈→∈-insert {_ ∷ _} _ x∈ (here refl) = there x∈
  ∈→∈-insert {_ ∷ _} l (here refl) (there l∈) = ∈-map l l∈
  ∈→∈-insert {_ ∷ _} {y} {x} l (there x∈) (there l∈) with map∷decomp l∈
  ... | (l' , l'∈ , y'l'l) = subst (x ∈_) y'l'l (there (∈→∈-insert l' x∈ l'∈))

  insert⊆∷ : ∀ {xs : List A} {x : A} l → l ∈ insert x xs → l ⊆ x ∷ xs
  insert⊆∷ {[]}    (_ ∷ _) (here refl) y∈ = y∈
  insert⊆∷ {_ ∷ _} (_ ∷ _) (here refl) y∈ = y∈
  insert⊆∷ {_ ∷ _} (_ ∷ _) (there l∈) {y} y∈ with map∷decomp l∈
  ... | (l' , l'∈ , x'l'zzs) = case (subst (y ∈ˡ_) (sym x'l'zzs) y∈) of λ where
    (here refl) → there (here refl)
    (there q) → case (insert⊆∷ l' l'∈ q) of λ where
      (here refl) → here refl
      (there x) → there (there x)

  insert-decomp : {ls : List (List A)} {x : A} → ∀ ys → ys ∈ concatMap (insert x) ls
                  → ∃[ l ] l ∈ ls × ys ⊆ x ∷ l
  insert-decomp _ h = case to concatMap-decomp h of λ where
    (x , y , z) → x , y , λ {_} → insert⊆∷ _ z

  insert-decomp≡ : ∀ {ys : List A} {y : A} xs → xs ∈ insert y ys
                   → ∃[ ll ] ∃[ lr ] xs ≡ ll ++ [ y ] ++ lr × ys ≡ ll ++ lr
  insert-decomp≡ {[]}     _        (here refl) = [] , [] , refl , refl
  insert-decomp≡ {y ∷ ys} _        (here refl) = [] , y ∷ ys , refl , refl
  insert-decomp≡ {_ ∷ _}  []       (there h)   = ⊥-elim (¬[]∈map h)
  insert-decomp≡ {y' ∷ _} (z ∷ zs) (there h)   =
    case insert-decomp≡ zs (proj₂ (map∷decomp∈ h)) , proj₁ (map∷decomp∈ h) of λ where
      ((ll , lr , zs≡llylr , ys≡lllr) , refl) →
        y' ∷ ll , lr , cong (y' ∷_) zs≡llylr , cong (y' ∷_) ys≡lllr

----------------------------------------------
------- properties of subpermutations --------
----------------------------------------------
module _ {a} {A : Set a} where
  []∈subpermutations : (l : List A) → [] ∈ subpermutations l
  []∈subpermutations []       = here refl
  []∈subpermutations (x ∷ xs) = xs⊆ys++xs _ (concatMap (insert x) (subpermutations xs))
                                            ([]∈subpermutations xs)

  Unique→dropSubheadUnique : {xs : List A} {x y : A} → Unique (x ∷ y ∷ xs) →  Unique (x ∷ xs)
  Unique→dropSubheadUnique ((_ All.∷ xxsU) ∷ yxsU) = xxsU ∷ (drop⁺ 1 yxsU)

  Unique→head∉tail : {xs : List A} {x : A} → Unique (x ∷ xs) → ¬ x ∈ xs
  Unique→head∉tail ((px All.∷ _) ∷ _) (here refl) = px refl
  Unique→head∉tail xxsU (there p) = Unique→head∉tail (Unique→dropSubheadUnique xxsU) p

  ∈-insert-cancelˡ : (ls : List (List A)) {xs : List A} {y : A}
                     → ¬ y ∈ xs → xs ∈ concatMap (insert y) ls ++ ls → xs ∈ ls
  ∈-insert-cancelˡ ls {xs} y∉xs xs∈yls = case (∈-++⁻ _ xs∈yls) of λ where
    (inj₁ v) → case (to (concatMap-decomp {l = ls}) v) of λ where
      (l' , l'∈ , l∈xl') → ⊥-elim (y∉xs (∈-insert xs l∈xl'))
    (inj₂ v) → v

module _ {a} {A : Set a} where
  ∈insert→∷∈insert' : {ys xs : List A} {y x : A} → x ∈ ys → ¬ x ∈ xs
                     → ∃[ sp ] sp ∈ subpermutations ys × xs ∈ insert y sp
                     → ∃[ sp' ] sp' ∈ subpermutations ys × x ∷ xs ∈ insert y sp'

  ∈insert→∷∈insert' {x ∷ ys} {xs} {y} {x} (here refl) h₂ (sp , h₃ , h₄)
    = case ∈-++⁻ (concatMap (insert x) (subpermutations ys)) h₃ of λ where
      (inj₁ v) → case to (concatMap-decomp {l = subpermutations ys}) v of λ where
        (x , y , z) → ⊥-elim (h₂ (∈→∈-insert xs (∈-insert sp z) h₄))
      (inj₂ v) → case insert-decomp≡ _ h₄ of λ where
        (ll , lr , xs≡ , sp≡) → x ∷ sp , ∈-++⁺ˡ (from concatMap-decomp (sp , v , ∷∈insert))
          , there (∈-map⁺ (x ∷_) h₄)

  ∈insert→∷∈insert' {y' ∷ ys} {xs} {y} {x} (there h₁) h₂ (sp , h₃ , h₄)
    = case ∈-++⁻ (concatMap (insert y') (subpermutations ys)) h₃ of λ where
      (inj₁ v) → case to concatMap-decomp v of λ where
        (l , l∈ , xs∈) → case ∈insert→∷∈insert' h₁ (λ p → h₂ (∈→∈-insert xs p h₄)) (l , l∈ , xs∈) of λ where
          f → let v' = x ∷ sp ∈ concatMap (insert y') (subpermutations ys) ∋ from concatMap-decomp f
              in to concatMap-decomp (subst (x ∷ xs ∈ˡ_)
                      (sym (concatMap-++ (insert y) (concatMap (insert y') (subpermutations ys)) _))
                      (∈-++⁺ˡ (from concatMap-decomp (x ∷ sp , v' , (there $ ∈-map⁺ (x ∷_) h₄)))))
      (inj₂ v) → case ∈insert→∷∈insert' h₁ h₂ (sp , v , h₄) of λ where
        (sp' , h'₁ , h'₂) → sp' , ∈-++⁺ʳ _ h'₁ , h'₂


  ∈insert→∷∈insert : {ys xs : List A} {y x : A} → x ∈ ys → ¬ x ∈ xs
                     → xs ∈ concatMap (insert y) (subpermutations ys)
                     → x ∷ xs ∈ concatMap (insert y) (subpermutations ys)

  ∈insert→∷∈insert h₁ h₂ h₃ = from concatMap-decomp $ ∈insert→∷∈insert' h₁ h₂ $ to concatMap-decomp h₃


  ∈-subperm-addhead : {ys xs : List A} {x : A} → x ∈ ys → ¬ x ∈ xs
                      → xs ∈ subpermutations ys → x ∷ xs ∈ subpermutations ys

  ∈-subperm-addhead {_ ∷ ys} (here refl) y∉xs xs∈sp =
    xs⊆xs++ys _ _ (from concatMap-decomp (_ , ∈-insert-cancelˡ (subpermutations ys) y∉xs xs∈sp , ∷∈insert))

  ∈-subperm-addhead {_ ∷ _} (there x∈ys) x∉xs xs∈sp = case ∈-++⁻ _ xs∈sp of λ where
    (inj₁ v) → xs⊆xs++ys _ _ (∈insert→∷∈insert x∈ys x∉xs v)
    (inj₂ v) → xs⊆ys++xs _ _ (∈-subperm-addhead x∈ys x∉xs v)


----------------------------------------------------------------
------------  maximal sublists satisfying a predicate ----------
----------------------------------------------------------------
module _ {a} {A : Set a}
         {p} {P : Pred (List A) p} where

  -- sublists of a given list which satisfy P
  sublists⊧P :  Decidable¹ P → List A → List (List A)
  sublists⊧P P? ys = filter P? (sublists ys)

  -- sublists of a given list which satisfy P and are of maximum length among those satisfying P
  maxsublists⊧P : Decidable¹ P → List A → List (List A)
  maxsublists⊧P P? ys = filter (λ l → length l ≟ maxlen (sublists⊧P P? ys)) (sublists⊧P P? ys)

--------------------------------------------------------------------------
------------  l ⊆ ys  ⋀  l Unique   ⇔   l ∈ subpermutations ys  ----------
--------------------------------------------------------------------------
module _ {a} {A : Set a} where
  -- If l ⊆ ys and l has no repeated elements, then l is a subpermutation of ys.
  uniqueSubset→subperm : ∀ {ys : List A} l → Unique l → l ⊆ ys → l ∈ subpermutations ys
  uniqueSubset→subperm {[]}     []       _  _  = here refl
  uniqueSubset→subperm {[]}     (_ ∷ _)  _  l⊆ = ⊥-elim (¬⊆[] l⊆)
  uniqueSubset→subperm {_ ∷ ys} []       _  _  = xs⊆ys++xs _ _ ([]∈subpermutations ys)
  uniqueSubset→subperm {_ ∷ _}  (_ ∷ xs) lU l⊆ = case l⊆ (here refl) of λ where
    (here refl) → let
      xs⊆ys = ⊆y∷∧y∉→⊆ys (l⊆ ∘ there) (Unique→head∉tail lU) in
      xs⊆xs++ys _ _ (from concatMap-decomp (xs , uniqueSubset→subperm xs (drop⁺ 1 lU) xs⊆ys , ∷∈insert))
    (there p) → case ∈-++⁻ _ (uniqueSubset→subperm xs (drop⁺ 1 lU) (l⊆ ∘ there)) of λ where
      (inj₁ v) → xs⊆xs++ys _ _ (∈insert→∷∈insert p (Unique→head∉tail lU) v)
      (inj₂ v) → xs⊆ys++xs _ _ (∈-subperm-addhead p (Unique→head∉tail lU) v)

  -- If l is a subpermutation of ys, then l ⊆ ys.
  subperm→subset : ∀ {ys : List A} l → l ∈ subpermutations ys → l ⊆ ys
  subperm→subset {[]} .[] (here refl) ()
  subperm→subset {y ∷ ys} l l∈ x∈l = case ∈-++⁻ (concatMap (insert y) (subpermutations ys)) l∈ of λ where
    (inj₂ v) → there (subperm→subset l v x∈l)
    (inj₁ v) → case insert-decomp l v of λ where
      (l' , l'∈sp ,  l⊆yl') → ∈∷∧⊆→∈ (l⊆yl' x∈l) (subperm→subset l' l'∈sp)

module _ {a} {p} {A : Set a} {L : List A} {P : Pred (List A) p} where

  ∃uniqueSubset→∃subperm : ∃[ l ] l ⊆ L × Unique l × P l → ∃[ l ] l ∈ subpermutations L × P l
  ∃uniqueSubset→∃subperm (l , l⊆L , lU , Pl) = l , uniqueSubset→subperm l lU l⊆L , Pl

  ∃subperm→∃subset : ∃[ l ] l ∈ subpermutations L × P l → ∃[ l ] l ⊆ L × P l
  ∃subperm→∃subset (l , l∈spL , Pl) = l , subperm→subset l l∈spL , Pl

  ∃uniqueSubset→∃uniqueSubperm : ∃[ l ] l ⊆ L × Unique l × P l
                               → ∃[ l ] l ∈ subpermutations L × Unique l × P l
  ∃uniqueSubset→∃uniqueSubperm (l , l⊆L , lU , Pl) = l , uniqueSubset→subperm l lU l⊆L , lU , Pl

  ∃uniqueSubperm→∃uniqueSubset : ∃[ l ] l ∈ subpermutations L × Unique l × P l
                               → ∃[ l ] l ⊆ L × Unique l × P l
  ∃uniqueSubperm→∃uniqueSubset (l , l∈spL , lU , Pl) = l , subperm→subset l l∈spL , lU , Pl

  ∃uniqueSubset⇔∃uniqueSubperm : (∃[ l ] l ⊆ L × Unique l × P l)
                               ⇔ (∃[ l ] l ∈ subpermutations L × Unique l × P l)
  ∃uniqueSubset⇔∃uniqueSubperm = mk⇔ ∃uniqueSubset→∃uniqueSubperm ∃uniqueSubperm→∃uniqueSubset
