
module Foreign.HaskellTypes where

open import Level using (Level)
open import Data.Bool.Base using (Bool)
open import Data.Nat.Base using (ℕ)
open import Data.String.Base using (String)
open import Data.List.Base using (List)
open import Data.Maybe.Base using (Maybe)
open import Data.Sum.Base using (_⊎_)
open import Data.Product.Base using (_×_)
open import Data.Unit using (⊤)

open import Foreign.Haskell.Pair using (Pair)
open import Foreign.Haskell.Either using (Either)

private variable
  l : Level
  A B : Set l

record HasHsType (A : Set l) : Set₁ where
  field
    HsType : Set

HsType : (A : Set l) → ⦃ HasHsType A ⦄ → Set
HsType _ ⦃ i ⦄ = i .HasHsType.HsType

MkHsType : (A : Set l) (Hs : Set) → HasHsType A
MkHsType A Hs .HasHsType.HsType = Hs

instance

  iHsTy-ℕ      = MkHsType ℕ ℕ
  iHsTy-Bool   = MkHsType Bool Bool
  iHsTy-⊤      = MkHsType ⊤ ⊤
  iHsTy-String = MkHsType String String

  -- Could make a macro for these kind of congruence instances.
  iHsTy-List : ⦃ HasHsType A ⦄ → HasHsType (List A)
  iHsTy-List {A = A} .HasHsType.HsType = List (HsType A)

  iHsTy-Maybe : ⦃ HasHsType A ⦄ → HasHsType (Maybe A)
  iHsTy-Maybe {A = A} .HasHsType.HsType = Maybe (HsType A)

  iHsTy-Fun : ⦃ HasHsType A ⦄ → ⦃ HasHsType B ⦄ → HasHsType (A → B)
  iHsTy-Fun {A = A} {B = B} .HasHsType.HsType = HsType A → HsType B

  iHsTy-Sum : ⦃ HasHsType A ⦄ → ⦃ HasHsType B ⦄ → HasHsType (A ⊎ B)
  iHsTy-Sum {A = A} {B = B} .HasHsType.HsType = Either (HsType A) (HsType B)

  iHsTy-Pair : ⦃ HasHsType A ⦄ → ⦃ HasHsType B ⦄ → HasHsType (A × B)
  iHsTy-Pair {A = A} {B = B} .HasHsType.HsType = Pair (HsType A) (HsType B)
