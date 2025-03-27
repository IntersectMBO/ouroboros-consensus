
module Foreign.Convertible.DerivingTest where

open import Level
open import MetaPrelude
open import Meta

import Data.List as L
import Data.List.NonEmpty as NE
import Data.String as S
import Data.Product as P

open import Relation.Nullary
open import Relation.Nullary.Decidable

open import Reflection.Tactic
open import Reflection.AST.Term
open import Reflection.AST.DeBruijn
import Reflection.TCM as R
open import Reflection.Utils
open import Reflection.Utils.TCI
import Function.Identity.Effectful as Identity

open import Class.DecEq
open import Class.DecEq
open import Class.Functor
open import Class.Monad
open import Class.MonadTC.Instances
open import Class.Traversable
open import Class.Show
open import Class.MonadReader

open import Foreign.Convertible
open import Foreign.Convertible.Deriving

-- Tests

open import Data.Maybe.Base
open import Data.Sum.Base

data HsMaybe (a : Set) : Set where
  just    : (x : a) → HsMaybe a
  nothing : HsMaybe a

data HsEither (a b : Set) : Set where
  left : a → HsEither a b
  right : b → HsEither a b

-- We should be able to generate this
ConvertibleMaybe : ∀ {a a'} → ⦃ Convertible a a' ⦄ → Convertible (Maybe a) (HsMaybe a')
ConvertibleMaybe .to (just x)   = just (to x)
ConvertibleMaybe .to nothing    = nothing
ConvertibleMaybe .from (just x) = just (from x)
ConvertibleMaybe .from nothing  = nothing

-- With deriveConvertible
unquoteDecl iConvertMaybe  = deriveConvertible iConvertMaybe (quote Maybe) (quote HsMaybe)

-- or with ConvertibleType and autoConvertible
instance
  iConvertEither : ConvertibleType _⊎_ HsEither
  iConvertEither = autoConvertible

instance
  ConvertibleNat = Convertible-Refl {ℕ}

test : ℕ ⊎ Maybe ℕ → HsEither ℕ (HsMaybe ℕ)
test = to

_ : test (inj₂ (just 5)) ≡ right (just 5)
_ = refl

-- There was a problem due to Agda#7182 with record types in parameterised modules.

module Param (A : Set) where
  record AgdaThing : Set where
    field theThing : A

record HsThing : Set where
  field theThing : ℕ

open Param ℕ
unquoteDecl iConvertThing = deriveConvertible iConvertThing (quote AgdaThing) (quote HsThing)

convThing : Convertible AgdaThing HsThing
convThing = autoConvertible
