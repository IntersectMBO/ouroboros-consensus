{-# OPTIONS --safe --no-import-sorts #-}
{-# OPTIONS -v allTactics:100 #-}

open import Prelude
open import Axiom.Set using (Theory)

module Axiom.Set.Map (th : Theory {lzero}) where

open Theory th renaming (map to mapˢ)

open import Axiom.Set.Rel th hiding (_∣'_; _∣^'_)
open import Axiom.Set.Properties th

import Data.Sum as ⊎
open import Data.List.Ext.Properties using (AllPairs⇒≡∨R∨Rᵒᵖ)
open import Data.Product.Ext using (×-dup)
open import Data.Product.Properties using (×-≡,≡→≡; ×-≡,≡←≡)
open import Data.Maybe.Properties using (just-injective)
open import Relation.Unary using (Decidable)
import Relation.Binary.PropositionalEquality as I
import Relation.Binary.Reasoning.Setoid as SetoidReasoning

open Equivalence

open import Class.DecEq using (DecEq; _≟_)

open import Reflection.Tactic using (initTac)
open import Tactic.AnyOf
open import Tactic.Assumption
open import Tactic.Defaults
open import Tactic.ByEq

-- Because of missing macro hygiene, we have to copy&paste this.
-- c.f. https://github.com/agda/agda/issues/3819
private macro
  ∈⇒P = anyOfⁿᵗ
    (quote ∈-filter⁻' ∷ quote ∈-∪⁻ ∷ quote ∈-map⁻' ∷ quote ∈-fromList⁻ ∷ [])
  P⇒∈ = anyOfⁿᵗ
    (quote ∈-filter⁺' ∷ quote ∈-∪⁺ ∷ quote ∈-map⁺' ∷ quote ∈-fromList⁺ ∷ [])
  ∈⇔P = anyOfⁿᵗ
    ( quote ∈-filter⁻' ∷ quote ∈-∪⁻ ∷ quote ∈-map⁻' ∷ quote ∈-fromList⁻
    ∷ quote ∈-filter⁺' ∷ quote ∈-∪⁺ ∷ quote ∈-map⁺' ∷ quote ∈-fromList⁺ ∷ [])

private variable
  A A' B B' C D : Type
  R R' : Rel A B
  X Y : Set A
  a : A
  a' : A'
  b : B
  b' : B'

left-unique : Rel A B → Type
left-unique R = ∀ {a b b'} → (a , b) ∈ R → (a , b') ∈ R → b ≡ b'

record IsLeftUnique (R : Rel A B) : Type where
  field isLeftUnique : left-unique R

instance
  ∅-left-unique : IsLeftUnique {A = A} {B = B} ∅
  ∅-left-unique .IsLeftUnique.isLeftUnique h h' = ⊥-elim $ ∉-∅ h

⊆-left-unique : R ⊆ R' → left-unique R' → left-unique R
⊆-left-unique R⊆R' h = R⊆R' -⟨ h ⟩- R⊆R' -- on isn't dependent enough

left-unique-mapˢ : {f : A → B} (X : Set A) → left-unique (mapˢ (λ y → (y , f y)) X)
left-unique-mapˢ _ p q with from ∈-map p | from ∈-map q
... | _ , refl , _ | _ , refl , _ = refl

Map : Type → Type → Type
Map A B = Σ (Rel A B) left-unique

_≡ᵐ_ : Map A B → Map A B → Type
(x , _) ≡ᵐ (y , _) = x ≡ᵉ y

private variable m m' : Map A B

_ˢ : Map A B → Rel A B
_ˢ = proj₁

_ᵐ : (R : Rel A B) → ⦃ IsLeftUnique R ⦄ → Map A B
_ᵐ R ⦃ record { isLeftUnique = h } ⦄ = R , h

infix 4 _≡ᵉᵐ_

_≡ᵉᵐ_ : Map A B → Map A B → Type
_≡ᵉᵐ_ = _≡ᵉ_ on _ˢ

⊆-map : (f : Rel A B → Rel A B) → (∀ {R} → f R ⊆ R) → Map A B → Map A B
⊆-map f H m = f (m ˢ) , ⊆-left-unique H (proj₂ m)

ˢ-left-unique : IsLeftUnique (m ˢ)
ˢ-left-unique {m = m} = record { isLeftUnique = proj₂ m }

instance
  _ = ˢ-left-unique

∅ᵐ : Map A B
∅ᵐ = _ᵐ ∅ ⦃ ∅-left-unique ⦄

fromListᵐ : ⦃ _ : DecEq A ⦄ → List (A × B) → Map A B
fromListᵐ l = fromList (deduplicate (λ x y → proj₁ x ≟ proj₁ y) l) ,
  (λ where (inj₁ refl)     → refl
           (inj₂ (inj₁ x)) → ⊥-elim (x refl)
           (inj₂ (inj₂ x)) → ⊥-elim (x refl))
  ∘₂ (∈⇒P -⟨ AllPairs⇒≡∨R∨Rᵒᵖ
           $ deduplicate-! (On.decSetoid (Prelude.decSetoid _≟_) proj₁) l ⟩- ∈⇒P)
  where open import Data.List.Relation.Unary.Unique.DecSetoid.Properties
        open import Relation.Binary.Construct.On as On

FinMap : Type → Type → Type
FinMap A B = Σ (Rel A B) (λ R → left-unique R × finite R)

toFinMap : (m : Map A B) → finite (m ˢ) → FinMap A B
toFinMap (m , h) fin = m , h , fin

toMap : FinMap A B → Map A B
toMap (m , l , _) = m , l

toRel : FinMap A B → Rel A B
toRel (m , l , _) = m

module Intersectionᵐ (sp-∈ : spec-∈ (A × B)) where
  open Intersection sp-∈
  open Intersectionᵖ sp-∈

  _∩ᵐ_ : Map A B → Map A B → Map A B
  m ∩ᵐ m' = (m ˢ ∩ m' ˢ , ⊆-left-unique ∩-⊆ˡ (proj₂ m))

disj-∪ : (m m' : Map A B) → disjoint (dom (m ˢ)) (dom (m' ˢ)) → Map A B
disj-∪ m m' disj = m ˢ ∪ m' ˢ , λ h h' → case ∈⇔P h , ∈⇔P h' of λ where
  (inj₁ hm  , inj₁ h'm)  → proj₂ m hm h'm
  (inj₂ hm' , inj₁ h'm)  → ⊥-elim $ disj (∈-map⁺'' h'm) (∈-map⁺'' hm')
  (inj₁ hm  , inj₂ h'm') → ⊥-elim $ disj (∈-map⁺'' hm)  (∈-map⁺'' h'm')
  (inj₂ hm' , inj₂ h'm') → proj₂ m' hm' h'm'

filterᵐ : {P : Pred (A × B) 0ℓ} → specProperty P → Map A B → Map A B
filterᵐ sp-P m = filter sp-P (m ˢ) , ⊆-left-unique filter-⊆ (proj₂ m)

filterᵐ-finite : {P : A × B → Type} → (sp : specProperty P) → Decidable P
  → finite (m ˢ) → finite (filterᵐ sp m ˢ)
filterᵐ-finite = filter-finite

filterKeys : {P : A → Type} → specProperty P → Map A B → Map A B
filterKeys sp-P = filterᵐ (sp-∘ sp-P proj₁)

singletonᵐ : A → B → Map A B
singletonᵐ a b = ❴ (a , b) ❵
               , (from ∈-singleton -⟨ (λ where refl refl → refl) ⟩- from ∈-singleton)

❴_❵ᵐ : A × B → Map A B
❴ k , v ❵ᵐ = singletonᵐ k v

disj-dom : ∀ {m m₁ m₂ : Map A B}
  → (m ˢ) ≡ (m₁ ˢ) ⨿ (m₂ ˢ)
  → disjoint (dom (m₁ ˢ)) (dom (m₂ ˢ))
disj-dom {m = m@(_ , uniq)} {m₁} {m₂} (m≡m₁∪m₂ , disj) a∈domm₁ a∈domm₂
  with (a , b₁) , (refl , h₁) ← ∈⇔P a∈domm₁
  with (_ , b₂) , (refl , h₂) ← ∈⇔P a∈domm₂
  = disj (subst _ (uniq (∈mᵢ⇒∈m (inj₁ h₁)) (∈mᵢ⇒∈m (inj₂ h₂))) h₁) h₂
  where
    ∈mᵢ⇒∈m : ∀ {a} → a ∈ (m₁ ˢ) ⊎ a ∈ (m₂ ˢ) → a ∈ (m ˢ)
    ∈mᵢ⇒∈m = proj₂ m≡m₁∪m₂ ∘ to ∈-∪

InjectiveOn : Set A → (A → B) → Type
InjectiveOn X f = ∀ {x y} → x ∈ X → y ∈ X → f x ≡ f y → x ≡ y

weaken-Injective : ∀ {X : Set A} {f : A → B} → Injective _≡_ _≡_ f → InjectiveOn X f
weaken-Injective p _ _ = p

mapˡ-uniq : {f : A → A'}
  → {@(tactic by-eq) inj : InjectiveOn (dom R) f}
  → left-unique R
  → left-unique (mapˡ f R)
mapˡ-uniq {inj = inj} uniq = λ h h' → case ∈⇔P h ,′ ∈⇔P h' of λ where
  (((_ , b) , refl , Ha) , ((_ , b') , eqb , Hb)) → uniq Ha
    $ subst _ ( sym
              $ ×-≡,≡→≡
              $ map₁ (inj (to dom∈ (b , Ha)) (to dom∈ (b' , Hb)))
                                  (×-≡,≡←≡ eqb))
              Hb

mapʳ-uniq : {f : B → B'} → left-unique R → left-unique (mapʳ f R)
mapʳ-uniq uniq = λ h h' → case ∈⇔P h ,′ ∈⇔P h' of λ where
  ((_ , refl , Ha) , (_ , refl , Hb)) → cong _ $ uniq Ha Hb

mapKeys : (f : A → A') → (m : Map A B)
  → {@(tactic by-eq) _ : InjectiveOn (dom (m ˢ)) f}
  → Map A' B
mapKeys f (R , uniq) {inj} = mapˡ f R , mapˡ-uniq {inj = inj} uniq

mapValues : (B → B') → Map A B → Map A B'
mapValues f (R , uniq) = mapʳ f R , mapʳ-uniq uniq

module Unionᵐ (sp-∈ : spec-∈ A) where
  infixr 6 _∪ˡ_

  _∪ˡ'_ : Rel A B → Rel A B → Rel A B
  m ∪ˡ' m' = m ∪ filter (sp-∘ (sp-¬ (sp-∈ {dom m})) proj₁) m'

  _∪ˡ_ : Map A B → Map A B → Map A B
  m ∪ˡ m' = disj-∪ m (filterᵐ (sp-∘ (sp-¬ sp-∈) proj₁) m')
      (∈⇔P -⟨ (λ where x (_ , refl , hy) → proj₁ (∈⇔P hy) (∈⇔P x)) ⟩- ∈⇔P)

  disjoint-∪ˡ-∪ : (H : disjoint (dom R) (dom R')) → R ∪ˡ' R' ≡ᵉ R ∪ R'
  disjoint-∪ˡ-∪ disj = from ≡ᵉ⇔≡ᵉ' λ _ → mk⇔
    (∈-∪⁺ ∘′ ⊎.map₂ (proj₂ ∘′ ∈⇔P) ∘′ ∈⇔P)
    (∈⇔P ∘′ ⊎.map₂ (to ∈-filter ∘′ (λ h → (flip disj (∈-map⁺'' h)) , h)) ∘ ∈⇔P)

  insert : Map A B → A → B → Map A B
  insert m a b = ❴ a , b ❵ᵐ ∪ˡ m

  insertIfJust : ⦃ DecEq A ⦄ → A → Maybe B → Map A B → Map A B
  insertIfJust x nothing  m  = m
  insertIfJust x (just y) m  = insert m x y

  disjoint-∪ˡ-mapValues : {M M' : Map A B}
                          (f : B → C)
                          → (H : disjoint (dom (M ˢ)) (dom (M' ˢ)))
                          → (mapValues f (M ∪ˡ M')) ˢ ≡ᵉ (mapValues f M ∪ˡ mapValues f M') ˢ
  disjoint-∪ˡ-mapValues {M = M} {M'} f disj = begin
    proj₁ (mapValues f (M ∪ˡ M'))
    ≈⟨ map-≡ᵉ (disjoint-∪ˡ-∪ disj) ⟩
    (mapʳ f ((proj₁ M) ∪ (proj₁ M')))
    ≈⟨ map-∪ _ ⟩
    (mapʳ f (proj₁ M) ∪ mapʳ f (proj₁ M'))
    ≈˘⟨ disjoint-∪ˡ-∪ (λ x₁ x₂ → disj (dom-mapʳ⊆ x₁) (dom-mapʳ⊆ x₂)) ⟩
    proj₁ (mapValues f M ∪ˡ mapValues f M')
    ∎
   where open SetoidReasoning ≡ᵉ-Setoid

map⦅×-dup⦆-uniq : ∀ {x : Set A} → left-unique (mapˢ ×-dup x)
map⦅×-dup⦆-uniq x y with ∈-map⁻' x | ∈-map⁻' y
... | fst , refl , _ | .fst , refl , _ = refl

mapˡ∘map⦅×-dup⦆-uniq : ∀ {S : Set A} {f : A → B}
  → {@(tactic by-eq) inj : Injective _≡_ _≡_ f}
  → left-unique $ mapˡ f (mapˢ ×-dup S)
mapˡ∘map⦅×-dup⦆-uniq {inj = inj} = mapˡ-uniq {inj = λ _ _ → inj} map⦅×-dup⦆-uniq

idMap : Set A → Map A A
idMap s = -, map⦅×-dup⦆-uniq {x = s}

mapFromFun : (A → B) → Set A → Map A B
mapFromFun f s = mapValues f (idMap s)

mapWithKey-uniq : {f : A → B → B'}
  → left-unique R
  → left-unique (mapˢ (λ { (x , y) → x , f x y }) R)
mapWithKey-uniq {f = f} uniq p q with from ∈-map p | from ∈-map q
... | (x , y) , refl , xy∈r | (x' , y') , refl , xy'∈r = cong (f x) (uniq xy∈r xy'∈r)

mapWithKey : (A → B → B') → Map A B → Map A B'
mapWithKey f m@(r , p) = mapˢ (λ { (x , y) → x , f x y}) r , mapWithKey-uniq p

mapValues-dom : {f : B → C} → dom (m ˢ) ≡ᵉ dom (mapValues f m ˢ)
mapValues-dom {m = _ , _} = mapʳ-dom

_∣'_ : {P : A → Type} → Map A B → specProperty P → Map A B
m ∣' P? = filterᵐ (sp-∘ P? proj₁) m

_∣^'_ : {P : B → Type} → Map A B → specProperty P → Map A B
m ∣^' P? = filterᵐ (sp-∘ P? proj₂) m

constMap : Set A → B → Map A B
constMap X b = mapˢ (_, b) X , λ x x₁ →
  trans (proj₂ $ ×-≡,≡←≡ $ proj₁ $ proj₂ (∈⇔P x))
        (sym $ proj₂ $ ×-≡,≡←≡ $ proj₁ $ proj₂ (∈⇔P x₁))

mapPartialLiftKey-just-uniq : ∀ {f : A → B → Maybe B'}
  → left-unique R
  → just (a , b)  ∈ mapˢ (mapPartialLiftKey f) R
  → just (a , b') ∈ mapˢ (mapPartialLiftKey f) R
  → b ≡ b'
mapPartialLiftKey-just-uniq {f = f} prop a∈ a'∈ =
  let _ , eq  , ax∈r  = mapPartialLiftKey-map {f = f} a∈
      _ , eq' , ax'∈r = mapPartialLiftKey-map {f = f} a'∈
  in
    just-injective $ trans eq (trans (cong (f _) (prop ax∈r ax'∈r)) (sym eq'))

mapPartial-uniq : ∀ {r : Rel A B} {f : A → B → Maybe B' }
  → left-unique r
  → left-unique (mapPartial (mapPartialLiftKey f) r)
mapPartial-uniq {f = f} prop {a} {b} {b'} p q =
  let p = ∈-map′ p
      q = ∈-map′ q
  in mapPartialLiftKey-just-uniq {f = f} prop (⊆-mapPartial p) (⊆-mapPartial q)

mapMaybeWithKeyᵐ : (A → B → Maybe B') → Map A B → Map A B'
mapMaybeWithKeyᵐ f (rel , prop) = mapMaybeWithKey f rel , mapPartial-uniq {f = f} prop

mapFromPartialFun : (A → Maybe B) → Set A → Map A B
mapFromPartialFun f s = mapMaybeWithKeyᵐ (λ _ → f) (idMap s)

module Restrictionᵐ (sp-∈ : spec-∈ A) where
  private module R = Restriction sp-∈
  open Unionᵐ sp-∈

  _∣_ : Map A B → Set A → Map A B
  m ∣ X = ⊆-map (R._∣ X) R.res-⊆ m

  _∣_ᶜ : Map A B → Set A → Map A B
  m ∣ X ᶜ = ⊆-map (R._∣ X ᶜ) R.ex-⊆ m

  resᵐ-∅ᶜ : {M : Map A B} → (M ∣ ∅ ᶜ) ˢ ≡ᵉ M ˢ
  resᵐ-∅ᶜ = R.res-∅ᶜ

  -- map only values in X
  mapValueRestricted : (B → B) → Map A B → Set A → Map A B
  mapValueRestricted f m X = mapValues f (m ∣ X) ∪ˡ m

  -- map only value at a
  mapSingleValue : (B → B) → Map A B → A → Map A B
  mapSingleValue f m a = mapValueRestricted f m ❴ a ❵

  curryᵐ : Map (A × B) C → A → Map B C
  curryᵐ m a = R.curryʳ (m ˢ) a , λ h h' → proj₂ m (R.∈-curryʳ h) (R.∈-curryʳ h')

  res-singleton : ∀ {k} → k ∈ dom (m ˢ) → ∃[ v ] m ∣ ❴ k ❵ ≡ᵉᵐ ❴ k , v ❵ᵐ
  res-singleton {m = m@(_ , uniq)} k∈domm
    with (k , v) , (refl , h) ← ∈⇔P k∈domm
    = v
    , (λ a∈m∣k → to ∈-singleton $ case ∈⇔P a∈m∣k of λ (mem₁ , mem₂) →
         let eq = from ∈-singleton mem₁
         in  ×-≡,≡→≡ (eq , (uniq mem₂ (subst _ (sym eq) h))))
    , λ a∈❴k,v❵ → subst (_∈ ((m ∣ ❴ k ❵) ˢ))
                        (sym $ from ∈-singleton a∈❴k,v❵)
                        (∈⇔P (to ∈-singleton refl , h))

  res-singleton' : ∀ {k v} → (k , v) ∈ m ˢ → m ∣ ❴ k ❵ ≡ᵉᵐ ❴ k , v ❵ᵐ
  res-singleton' {m = m} kv∈m
    with _ , h ← res-singleton {m = m} (∈⇔P (-, (refl , kv∈m)))
    = subst _ (sym $ proj₂ m kv∈m (R.res-⊆ $ proj₂ h $ to ∈-singleton refl)) h

  res-singleton⁺ : {k : A}{v : B} → (k , v) ∈ m ˢ → (k , v) ∈ (m ∣ ❴ k ❵)ˢ
  res-singleton⁺ kv∈m = to ∈-filter ((to ∈-singleton refl) , kv∈m)

  res-singleton-inhabited : ∀ {k a} → a ∈ (m ∣ ❴ k ❵) ˢ → k ∈ dom (m ˢ)
  res-singleton-inhabited {m = m} {k} {a} a∈ =
    to dom∈ ( proj₂ a , subst (λ x → (x , proj₂ a) ∈ (m ˢ))
                                (from ∈-singleton (R.res-dom (∈-dom a∈)))
                                (R.res-⊆ a∈) )

  res-singleton'' : ∀ {k a} → a ∈ (m ∣ ❴ k ❵)ˢ → ∃[ v ] a ≡ (k , v)
  res-singleton'' {m = m}{k}{a} a∈m =
    let (v , m|≡) = res-singleton {m = m} (res-singleton-inhabited{m = m} a∈m) in
    v , from ∈-singleton (proj₁ m|≡ a∈m)

  -- f(x,-)
  infix 30 _⦅_,-⦆
  _⦅_,-⦆ = curryᵐ

  update : A → Maybe B → Map A B → Map A B
  update x (just y) m = insert m x y
  update x nothing  m = m ∣ ❴ x ❵ ᶜ

module Lookupᵐ (sp-∈ : spec-∈ A) where
  open import Relation.Nullary.Decidable
  private module R = Restriction sp-∈
  open Unionᵐ sp-∈
  open Restriction sp-∈

  module _ (m : Map A B) (x : A) where
    lookupᵐ : {@(tactic initTac assumption') _ : x ∈ dom (m ˢ)} → B
    lookupᵐ {h} = proj₁ (from dom∈ h)

    lookupᵐ? : ⦃ (x ∈ dom (m ˢ)) ⁇ ⦄ → Maybe B
    lookupᵐ? ⦃ ⁇ no  _ ⦄ = nothing
    lookupᵐ? ⦃ ⁇ yes _ ⦄ = just lookupᵐ

  pullbackMap : (m : Map A B) → ⦃ ∀ {x} → (x ∈ dom (m ˢ)) ⁇ ⦄ → (A' → A) → Set A' → Map A' B
  pullbackMap m f s = mapMaybeWithKeyᵐ (λ a _ → lookupᵐ? m (f a)) (idMap s)

module Corestrictionᵐ (sp-∈ : spec-∈ B) where
  private module R = Corestriction sp-∈

  _∣^_ : Map A B → Set B → Map A B
  m ∣^ X = ⊆-map (R._∣^ X) R.cores-⊆ m

  _∣^_ᶜ : Map A B → Set B → Map A B
  m ∣^ X ᶜ = ⊆-map (R._∣^ X ᶜ) R.coex-⊆ m

  -- f⁻¹(x)
  infix 25 _⁻¹_
  _⁻¹_ : Map A B → B → Set A
  m ⁻¹ a = dom ((m ∣^ ❴ a ❵) ˢ)
