{-# OPTIONS --safe #-}

open import Axiom.Set using (Theory)

module Interface.HasSingleton (th : Theory) where

open Theory th renaming (Set to ℙ_; ❴_❵ to ❴_❵ˢ)
open import Axiom.Set.Map th
open import Data.Product using (_×_)

open import Agda.Primitive using () renaming (Set to Type)

record HasSingleton (A B : Type) : Type where
  field
    ❴_❵ : A → B

instance
  HasSingletonSet-Set : {A : Type} → HasSingleton A (ℙ A)
  HasSingletonSet-Set = record { ❴_❵ = ❴_❵ˢ }

  HasSingletonSet-Map : {A B : Type} → HasSingleton (A × B) (Map A B)
  HasSingletonSet-Map = record { ❴_❵ = ❴_❵ᵐ }

open HasSingleton ⦃...⦄ public
