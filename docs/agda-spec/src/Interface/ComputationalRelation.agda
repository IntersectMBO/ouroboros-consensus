{-# OPTIONS --safe #-}

module Interface.ComputationalRelation where

open import Prelude
open import Interface.STS public

private variable
  a : Level
  C S Sig : Type
  Err Err₁ Err₂ : Type
  c : C
  s s' s'' : S
  sig : Sig

data ComputationResult {a : Level} (Err : Type) (R : Type a) : Type a where
  success : R → ComputationResult Err R
  failure : Err → ComputationResult Err R

isFailure : ∀ {A : Type a} → ComputationResult Err A → Type a
isFailure x = ∃[ e ] x ≡ failure e

module _ {a b} {E : Type} {A : Type a} {B : Type b} where
  caseCR_∣_∣_ : (ma : ComputationResult E A) → (∀ {a} → ma ≡ success a → B) → (isFailure ma → B) → B
  caseCR ma ∣ f ∣ g with ma
  ... | success _ = f refl
  ... | failure e = g (e , refl)

  caseCR-success : ∀ {a} {ma : ComputationResult E A} {f : ∀ {a} → ma ≡ success a → B} {g : isFailure ma → B}
    → (eq : ma ≡ success a)
    → caseCR ma ∣ f ∣ g ≡ f eq
  caseCR-success refl = refl

  caseCR-failure : {ma : ComputationResult E A} {f : ∀ {a} → ma ≡ success a → B} {g : isFailure ma → B}
    → (eq : isFailure ma)
    → caseCR ma ∣ f ∣ g ≡ g eq
  caseCR-failure (_ , refl) = refl

instance
  Bifunctor-ComputationResult : ∀ {a : Level} → Bifunctor {_} {a} ComputationResult
  Bifunctor-ComputationResult .bimap _ f (success x) = success $ f x
  Bifunctor-ComputationResult .bimap f _ (failure x) = failure $ f x
  
  Functor-ComputationResult : ∀ {E : Type} → Functor (ComputationResult E)
  Functor-ComputationResult ._<$>_ f (success x) = success $ f x
  Functor-ComputationResult ._<$>_ _ (failure x) = failure x

  Applicative-ComputationResult : ∀ {E : Type} → Applicative (ComputationResult E)
  Applicative-ComputationResult .pure = success
  Applicative-ComputationResult ._<*>_ (success f) x = f <$> x
  Applicative-ComputationResult ._<*>_ (failure e) _ = failure e

  Monad-ComputationResult : ∀ {E : Type} → Monad (ComputationResult E)
  Monad-ComputationResult .return = success
  Monad-ComputationResult ._>>=_ (success a) m = m a
  Monad-ComputationResult ._>>=_ (failure e) _ = failure e

map-failure : ∀ {A B C : Type} {f : A → B} {x : C} {ma} → ma ≡ failure x → map f ma ≡ failure x
map-failure refl = refl

success-maybe : ∀ {R : Type} → ComputationResult Err R → Maybe R
success-maybe (success x) = just x
success-maybe (failure _) = nothing

failure-maybe : ∀ {R : Type} → ComputationResult Err R → Maybe Err
failure-maybe (success _) = nothing
failure-maybe (failure x) = just x

_≈ᶜʳ_ : ∀ {A} → ComputationResult Err A → ComputationResult Err A → Type
x ≈ᶜʳ y = success-maybe x ≡ success-maybe y

module _ (STS : C → S → Sig → S → Type) where

  ExtendedRel : C → S → Sig → ComputationResult Err S → Type
  ExtendedRel c s sig (success s') = STS c s sig s'
  ExtendedRel c s sig (failure _ ) = ∀ s' → ¬ STS c s sig s'

  record Computational Err : Type₁ where
    constructor MkComputational
    field
      computeProof : (c : C) (s : S) (sig : Sig) → ComputationResult Err (∃[ s' ] STS c s sig s')

    compute : C → S → Sig → ComputationResult Err S
    compute c s sig = map proj₁ $ computeProof c s sig

    field
      completeness : (c : C) (s : S) (sig : Sig) (s' : S)
        → STS c s sig s' → compute c s sig ≡ success s'

    open ≡-Reasoning

    computeFail : C → S → Sig → Type
    computeFail c s sig = isFailure $ compute c s sig

    ≡-success⇔STS : compute c s sig ≡ success s' ⇔ STS c s sig s'
    ≡-success⇔STS {c} {s} {sig} {s'} with computeProof c s sig in eq
    ... | success (s'' , h) = mk⇔ (λ where refl → h) λ h' →
      begin success s''     ≡˘⟨ completeness _ _ _ _ h ⟩
            compute c s sig ≡⟨ completeness _ _ _ _ h' ⟩
            success s'      ∎
    ... | failure y = mk⇔ (λ ()) λ h →
      begin failure _       ≡˘⟨ map-failure eq ⟩
            compute c s sig ≡⟨ completeness _ _ _ _ h ⟩
            success s'      ∎

    failure⇒∀¬STS : computeFail c s sig → ∀ s' → ¬ STS c s sig s'
    failure⇒∀¬STS comp≡nothing s' h rewrite ≡-success⇔STS .Equivalence.from h =
      case comp≡nothing of λ ()

    ∀¬STS⇒failure : (∀ s' → ¬ STS c s sig s') → computeFail c s sig
    ∀¬STS⇒failure {c = c} {s} {sig} ¬sts with computeProof c s sig
    ... | success (x , y) = ⊥-elim (¬sts x y)
    ... | failure y = (y , refl)

    failure⇔∀¬STS : computeFail c s sig ⇔ ∀ s' → ¬ STS c s sig s'
    failure⇔∀¬STS = mk⇔ failure⇒∀¬STS ∀¬STS⇒failure

    recomputeProof : ∀ {Γ s sig s'} → STS Γ s sig s' → ComputationResult Err (∃[ s'' ] STS Γ s sig s'')
    recomputeProof _ = computeProof _ _ _

module _ {STS : C → S → Sig → S → Type} (comp : Computational STS Err) where

  open Computational comp

  ExtendedRelSTS = ExtendedRel STS

  ExtendedRel-compute : ExtendedRelSTS c s sig (compute c s sig)
  ExtendedRel-compute {c} {s} {sig} with compute c s sig in eq
  ... | success s' = Equivalence.to ≡-success⇔STS eq
  ... | failure _  = λ s' h → case trans (sym $ Equivalence.from ≡-success⇔STS h) eq of λ ()

  open ≡-Reasoning

  ExtendedRel-rightUnique : ExtendedRelSTS c s sig s' → ExtendedRelSTS c s sig s'' → _≈ᶜʳ_ {Err = Err} s' s''
  ExtendedRel-rightUnique {s' = success x}  {success x'} h h'
    with trans (sym $ Equivalence.from ≡-success⇔STS h) (Equivalence.from ≡-success⇔STS h')
  ... | refl = refl
    
  ExtendedRel-rightUnique {s' = success x} {failure _}  h h' = ⊥-elim $ h' x h
  ExtendedRel-rightUnique {s' = failure _} {success x'} h h' = ⊥-elim $ h x' h'
  ExtendedRel-rightUnique {s' = failure a} {failure b}  h h' = refl

  computational⇒rightUnique : STS c s sig s' → STS c s sig s'' → s' ≡ s''
  computational⇒rightUnique h h' with ExtendedRel-rightUnique h h'
  ... | refl = refl

  Computational⇒Dec : ⦃ _ : DecEq S ⦄ → Dec (STS c s sig s')
  Computational⇒Dec {c} {s} {sig} {s'}
    with compute c s sig | ExtendedRel-compute {c} {s} {sig}
  ... | failure _ | ExSTS = no (ExSTS s')
  ... | success x  | ExSTS with x ≟ s'
  ... | no ¬p    = no  λ h → ¬p $ sym $ computational⇒rightUnique h ExSTS
  ... | yes refl = yes ExSTS

module _ {STS : C → S → Sig → S → Type} (comp comp' : Computational STS Err) where

  open Computational comp  renaming (compute to compute₁)
  open Computational comp' renaming (compute to compute₂)

  compute-ext≡ : compute₁ c s sig ≈ᶜʳ compute₂ c s sig
  compute-ext≡ = ExtendedRel-rightUnique comp
    (ExtendedRel-compute comp) (ExtendedRel-compute comp')

open Computational ⦃...⦄

Computational⇒Dec' :
  {STS : C → S → Sig → S → Type} ⦃ comp : Computational STS Err ⦄ → Dec (∃[ s' ] STS c s sig s')
Computational⇒Dec' with computeProof _ _ _ in eq
... | success x = yes x
... | failure x = no λ (_ , h) → failure⇒∀¬STS (-, cong (map proj₁) eq) _ h

record InjectError Err₁ Err₂ : Type where
  field injectError : Err₁ → Err₂

open InjectError

instance
  InjectError-⊥ : InjectError ⊥ Err
  InjectError-⊥ = λ where
    .injectError ()

  InjectError-Id : InjectError Err Err
  InjectError-Id = λ where
    .injectError → id

  Computational-Id : {C S : Type} → Computational (IdSTS {C} {S}) ⊥
  Computational-Id .computeProof _ s _ = success (s , Id-nop)
  Computational-Id .completeness _ _ _ _ Id-nop = refl

module _ {BSTS : C → S → ⊤ → S → Type} ⦃ _ : Computational BSTS Err₁ ⦄ where
  module _ {STS : C → S → Sig → S → Type} ⦃ _ : Computational STS Err₂ ⦄
     ⦃ _ : InjectError Err₁ Err ⦄ ⦃ _ : InjectError Err₂ Err ⦄ where instance
    Computational-ReflexiveTransitiveClosureᵇ : Computational (ReflexiveTransitiveClosureᵇ BSTS STS) (Err)
    Computational-ReflexiveTransitiveClosureᵇ .computeProof c s [] = bimap (injectError it) (map₂′ BS-base) (computeProof c s tt)
    Computational-ReflexiveTransitiveClosureᵇ .computeProof c s (sig ∷ sigs) with computeProof c s sig 
    ... | success (s₁ , h) with computeProof c s₁ sigs
    ...   | success (s₂ , hs) = success (s₂ , BS-ind h hs)
    ...   | failure a = failure (injectError it a)
    Computational-ReflexiveTransitiveClosureᵇ .computeProof c s (sig ∷ sigs) | failure a = failure (injectError it a)
    Computational-ReflexiveTransitiveClosureᵇ .completeness c s [] s' (BS-base p)
      with computeProof {STS = BSTS} c s tt | completeness _ _ _ _ p
    ... | success x | refl = refl
    Computational-ReflexiveTransitiveClosureᵇ .completeness c s (sig ∷ sigs) s' (BS-ind h hs)
      with computeProof c s sig | completeness _ _ _ _ h
    ... | success (s₁ , _) | refl
      with computeProof ⦃ Computational-ReflexiveTransitiveClosureᵇ ⦄ c s₁ sigs | completeness _ _ _ _ hs
    ... | success (s₂ , _) | p = p

  module _ {STS : C × ℕ → S → Sig → S → Type} ⦃ Computational-STS : Computational STS Err₂ ⦄
    ⦃ InjectError-Err₁ : InjectError Err₁ Err ⦄ ⦃ InjectError-Err₂ : InjectError Err₂ Err ⦄
    where instance
    Computational-ReflexiveTransitiveClosureᵢᵇ' : Computational (_⊢_⇀⟦_⟧ᵢ*'_ BSTS STS) Err
    Computational-ReflexiveTransitiveClosureᵢᵇ' .computeProof c s [] =
      bimap (injectError it) (map₂′ BS-base) (computeProof (proj₁ c) s tt)
    Computational-ReflexiveTransitiveClosureᵢᵇ' .computeProof c s (sig ∷ sigs) with computeProof c s sig
    ... | success (s₁ , h) with computeProof (proj₁ c , suc (proj₂ c)) s₁ sigs
    ... | success (s₂ , hs) = success (s₂ , BS-ind h hs)
    ... | failure a = failure a
    Computational-ReflexiveTransitiveClosureᵢᵇ' .computeProof c s (sig ∷ sigs) | failure a = failure (injectError it a)
    Computational-ReflexiveTransitiveClosureᵢᵇ' .completeness c s [] s' (BS-base p)
      with computeProof {STS = BSTS} (proj₁ c) s tt | completeness _ _ _ _ p
    ... | success x | refl = refl
    Computational-ReflexiveTransitiveClosureᵢᵇ' .completeness c s (sig ∷ sigs) s' (BS-ind h hs)
      with computeProof {STS = STS} c s sig | completeness _ _ _ _ h
    ... | success (s₁ , _) | refl
      with computeProof (proj₁ c , suc (proj₂ c)) s₁ sigs | completeness _ _ _ _ hs
    ...   | success (s₂ , _) | p = p

    Computational-ReflexiveTransitiveClosureᵢᵇ : Computational (ReflexiveTransitiveClosureᵢᵇ BSTS STS) Err
    Computational-ReflexiveTransitiveClosureᵢᵇ .computeProof c =
      Computational-ReflexiveTransitiveClosureᵢᵇ' .computeProof (c , 0)
    Computational-ReflexiveTransitiveClosureᵢᵇ .completeness c =
      Computational-ReflexiveTransitiveClosureᵢᵇ' .completeness (c , 0)

Computational-ReflexiveTransitiveClosure : {STS : C → S → Sig → S → Type} → ⦃ Computational STS Err ⦄
  → Computational (ReflexiveTransitiveClosure STS) Err
Computational-ReflexiveTransitiveClosure = it

Computational-ReflexiveTransitiveClosureᵢ : {STS : C × ℕ → S → Sig → S → Type} → ⦃ Computational STS Err ⦄
  → Computational (ReflexiveTransitiveClosureᵢ STS) Err
Computational-ReflexiveTransitiveClosureᵢ = it
