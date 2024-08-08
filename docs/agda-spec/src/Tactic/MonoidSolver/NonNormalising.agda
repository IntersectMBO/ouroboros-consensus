{-# OPTIONS --safe #-}

module Tactic.MonoidSolver.NonNormalising where

open import Data.List    using ([]; _∷_)
open import Data.Product using (_,_)
open import Data.Maybe   using (nothing; just)
open import Reflection
open import Tactic.MonoidSolver hiding (solve-macro; solve)
import Tactic.MonoidSolver as Solver

-- The standard library MonoidSolver normalises the type of the hole
-- before solving.  This can be very expensive in some scenarios. This
-- version tries first without normalising and only if that fails does
-- it resort to normalisation.
solve-macro : Term → Term → TC _
solve-macro mon hole = catchTC (do
    hole′ ← inferType hole
    names ← findMonoidNames mon
    just (lhs , rhs) ← pure (getArgs hole′)
      where nothing → typeError (termErr hole′ ∷ [])
    let soln = constructSoln mon names lhs rhs
    unify hole soln
  ) (Solver.solve-macro mon hole)

macro
  solve : Term → Term → TC _
  solve = solve-macro
