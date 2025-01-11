
module Tactic.Substitute where

open import MetaPrelude
open import Meta

-- There aren't any nice substitution functions (that I can find) in the standard library or
-- stdlib-meta. This one is cheating and only works for closed terms at either never gets
-- applied, or where we can safely throw away the arguments (e.g. `unknown`).

Subst : Set → Set
Subst A = ℕ → Term → A → A

substTerm : Subst Term
substArgs : Subst (Args Term)
substArg  : Subst (Arg Term)
substAbs  : Subst (Abs Term)
substSort : Subst Sort

substTerm x s (var y args) =
  case compare x y of λ where
    (less _ _)    → var (y ∸ 1) (substArgs x s args)
    (equal _)     → s  -- cheating and dropping the args!
    (greater _ _) → var y (substArgs x s args)
substTerm x s (con c args) = con c (substArgs x s args)
substTerm x s (def f args) = def f (substArgs x s args)
substTerm x s (lam v t) = lam v (substAbs x s t)
substTerm x s (pat-lam cs args₁) = unknown  -- ignoring for now
substTerm x s (pi a b) = pi (substArg x s a) (substAbs x s b)
substTerm x s (agda-sort s₁) = agda-sort (substSort x s s₁)
substTerm x s (lit l) = lit l
substTerm x s (meta m args) = meta m (substArgs x s args)
substTerm x s unknown = unknown

substArgs x s [] = []
substArgs x s (a ∷ as) = substArg x s a ∷ substArgs x s as

substArg x s (arg i t) = arg i (substTerm x s t)

substAbs x s (abs z t) = abs z (substTerm (ℕ.suc x) s t)

substSort x s (set t) = set (substTerm x s t)
substSort x s (lit n) = lit n
substSort x s (prop t) = prop (substTerm x s t)
substSort x s (propLit n) = propLit n
substSort x s (inf n) = inf n
substSort x s unknown = unknown
