{-# OPTIONS -v allTactics:100 #-}
{-# OPTIONS --safe #-}
--------------------------------------------------------------------------------
-- genError: returns the type of the most recently bound variable as a string
-- genErrors: match on a negated conjunction and return the string of that type
--------------------------------------------------------------------------------

module Tactic.GenError where

open import MetaPrelude
open import Meta

open import Class.Functor
open import Class.Monad
open import Class.MonadError.Instances
open import Class.MonadReader.Instances
open import Class.MonadTC.Instances
open import Class.Show

open import Reflection.Tactic
open import Reflection.Utils
open import Reflection.Utils.TCI
import Reflection as R
import Agda.Builtin.Reflection as R
open import Tactic.ClauseBuilder

module _ where
  open import Ledger.Prelude using (dec-de-morgan; _⁇; ¬_; Dec-⊤; Dec-⊥) public


instance
  _ = MonadTC-TCI
  _ = Functor-M
  _ = ContextMonad-MonadTC

open ClauseExprM

private
  liftTC : {A : Set} → R.TC A → TC A
  liftTC x = λ r → applyExtContext (r .TCEnv.localContext) x

genError' : ITactic
genError' = inDebugPath "genError" do
  t ← inferType (♯ 0)
  s ← liftTC (R.formatErrorParts [ R.termErr t ])
  unifyWithGoal (lit (Literal.string s))

module _ ⦃ _ : TCOptions ⦄ where
  macro
    genError = initTac genError'

genErrors' : ℕ → Term → ITactic
genErrors' 0 _ = error1 "genErrors: out of fuel"
genErrors' (suc fuel) t = inDebugPath "genErrors" $ flip catch
  (λ _ → genError') $ do
    m1 ← newMeta unknown
    m2 ← extendContext ("" , vArg m1) $ newMeta unknown
    unifyWithGoal =<< caseMatch (quote dec-de-morgan ∙⟦ t ⟧) (matchExprM $
        (([ ("" , vArg unknown) ] , vArg (quote inj₁ ◇⟦ ` 0 ⟧)) , finishMatch (withGoalHole genError'))
      ∷ (([ ("" , vArg m1) ]      , vArg (quote inj₂ ◇⟦ ` 0 ⟧)) , finishMatch (return m2)) ∷ [])
    extendContext ("" , vArg m1) $ runWithHole m2 (genErrors' fuel (♯ 0))

module _ ⦃ _ : TCOptions ⦄ where
  macro
    genErrors = initTac ∘ (genErrors' 100)

private
  open import MyDebugOptions
  module Test where
    test₁ : {A B : Set} → A → B → String
    test₁ a b = genError

    test₂ : {A : Set} → (a : A) → a ≡ a → String
    test₂ _ eq = genError

    test₃ : {A B C : Set} → ⦃ A ⁇ ⦄ → ⦃ B ⁇ ⦄ → ¬ (A × B × C) → String
    test₃ x = genErrors x

    _ : test₃ {A = ⊤} {⊥} {⊤} (λ where (_ , () , _)) ≡ "¬ B"
    _ = refl
