{-# OPTIONS --safe #-}

module Interface.STS where

open import Prelude

infix -150 ∙_
infixr -100 _∙_

∙_ : Type → Type
∙ A = A

_∙_ : Type → Type → Type
A ∙ B = A × B

infix -501 ────────────────────────────────_
────────────────────────────────_ : Type → Type
──────────────────────────────── A = A

infix -500 _────────────────────────────────_
_────────────────────────────────_ : Type → Type → Type
A ──────────────────────────────── B = A → B

infix -501 ───────────────────────────────────────_
───────────────────────────────────────_ : Type → Type
─────────────────────────────────────── A = A

infix -500 _───────────────────────────────────────_
_───────────────────────────────────────_ : Type → Type → Type
A ─────────────────────────────────────── B = A → B

private
  variable C S Sig : Type
           Γ : C
           s s' s'' : S
           sig : Sig
           sigs : List Sig
           n : ℕ

-- small-step to big-step transformer

module _ (_⊢_⇀⟦_⟧ᵇ_ : C → S → ⊤ → S → Type) (_⊢_⇀⟦_⟧_ : C → S → Sig → S → Type) where
  data _⊢_⇀⟦_⟧*_ : C → S → List Sig → S → Type where

    BS-base :
      Γ ⊢ s ⇀⟦ _ ⟧ᵇ s'
      ───────────────────────────────────────
      Γ ⊢ s ⇀⟦ [] ⟧* s'

    BS-ind :
        Γ ⊢ s  ⇀⟦ sig  ⟧  s'
      → Γ ⊢ s' ⇀⟦ sigs ⟧* s''
        ───────────────────────────────────────
        Γ ⊢ s  ⇀⟦ sig ∷ sigs ⟧* s''

module _ (_⊢_⇀⟦_⟧ᵇ_ : C → S → ⊤ → S → Type) (_⊢_⇀⟦_⟧_ : C × ℕ → S → Sig → S → Type) where
  data _⊢_⇀⟦_⟧ᵢ*'_ : C × ℕ → S → List Sig → S → Type where

    BS-base :
      Γ ⊢ s ⇀⟦ _ ⟧ᵇ s'
      ───────────────────────────────────────
      (Γ , n) ⊢ s ⇀⟦ [] ⟧ᵢ*' s'

    BS-ind :
        (Γ , n)     ⊢ s  ⇀⟦ sig  ⟧  s'
      → (Γ , suc n) ⊢ s' ⇀⟦ sigs ⟧ᵢ*' s''
        ───────────────────────────────────────
        (Γ , n)     ⊢ s  ⇀⟦ sig ∷ sigs ⟧ᵢ*' s''

  _⊢_⇀⟦_⟧ᵢ*_ : C → S → List Sig → S → Type
  _⊢_⇀⟦_⟧ᵢ*_ Γ = _⊢_⇀⟦_⟧ᵢ*'_ (Γ , 0)

data IdSTS {C S} : C → S → ⊤ → S → Type where
  Id-nop : IdSTS Γ s _ s

-- with a trivial base case
ReflexiveTransitiveClosure : (C → S → Sig → S → Type) → C → S → List Sig → S → Type
ReflexiveTransitiveClosure = _⊢_⇀⟦_⟧*_ IdSTS

STS-total : (C → S → Sig → S → Type) → Type
STS-total _⊢_⇀⟦_⟧_ = ∀ {Γ s sig} → ∃[ s' ] Γ ⊢ s ⇀⟦ sig ⟧ s'

ReflexiveTransitiveClosure-total : {_⊢_⇀⟦_⟧_ : C → S → Sig → S → Type}
  → STS-total _⊢_⇀⟦_⟧_ → STS-total (ReflexiveTransitiveClosure _⊢_⇀⟦_⟧_)
ReflexiveTransitiveClosure-total SS-total {Γ} {s} {[]} = s , BS-base Id-nop
ReflexiveTransitiveClosure-total SS-total {Γ} {s} {x ∷ sig} =
  case SS-total of λ where
    (s' , Ps') → map₂′ (BS-ind Ps') $ ReflexiveTransitiveClosure-total SS-total

ReflexiveTransitiveClosureᵢ : (C × ℕ → S → Sig → S → Type) → C → S → List Sig → S → Type
ReflexiveTransitiveClosureᵢ = _⊢_⇀⟦_⟧ᵢ*_ IdSTS

ReflexiveTransitiveClosureᵢ-total : {_⊢_⇀⟦_⟧_ : C × ℕ → S → Sig → S → Type}
  → STS-total _⊢_⇀⟦_⟧_ → STS-total (ReflexiveTransitiveClosureᵢ _⊢_⇀⟦_⟧_)
ReflexiveTransitiveClosureᵢ-total SS-total = helper SS-total
  where
    helper : {_⊢_⇀⟦_⟧_ : C × ℕ → S → Sig → S → Type}
      → STS-total _⊢_⇀⟦_⟧_ → STS-total (_⊢_⇀⟦_⟧ᵢ*'_ IdSTS _⊢_⇀⟦_⟧_)
    helper SS-total {s = s} {[]} = s , BS-base Id-nop
    helper SS-total {s = s} {x ∷ sig} =
      case SS-total of λ where
        (s' , Ps') → map₂′ (BS-ind Ps') $ helper SS-total

-- with a given base case
ReflexiveTransitiveClosureᵢᵇ = _⊢_⇀⟦_⟧ᵢ*_
ReflexiveTransitiveClosureᵇ  = _⊢_⇀⟦_⟧*_

LedgerInvariant : (C → S → Sig → S → Type) → (S → Type) → Type
LedgerInvariant STS P = ∀ {c s sig s'} → STS c s sig s' → P s → P s'

RTC-preserves-inv : ∀ {STS : C → S → Sig → S → Type} {P}
                  → LedgerInvariant STS P → LedgerInvariant (ReflexiveTransitiveClosure STS) P
RTC-preserves-inv inv (BS-base Id-nop) = id
RTC-preserves-inv inv (BS-ind p₁ p₂)   = RTC-preserves-inv inv p₂ ∘ inv p₁
