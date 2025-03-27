module Foreign.Convertible.Deriving where

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

open import Tactic.Substitute
open import Foreign.Convertible
open import Foreign.HaskellTypes
open import Foreign.HaskellTypes.Deriving

private instance
  _ = Functor-M {TC}

-- TODO: move to agda-stdlib-meta
liftTC : ∀ {a} {A : Set a} → R.TC A → TC A
liftTC m _ = m

private

  open MonadReader ⦃...⦄

  variable
    A B C : Set

  TyViewTel = List (Abs (Arg Type))

  substTel : Subst TyViewTel
  substTel x s [] = []
  substTel x s (abs z t ∷ tel) = abs z (substArg x s t) ∷ (substTel (ℕ.suc x) s tel)
    -- Note: `abs` is abused in TyViewTel and doesn't actually scope over the `t`

  -- Substitute leading level parameters with lzero
  smashLevels : TyViewTel → ℕ × TyViewTel
  smashLevels (abs s (arg i (def (quote Level) [])) ∷ tel) =
    P.map ℕ.suc (substTel 0 (quote 0ℓ ∙)) $ smashLevels tel
  smashLevels tel = (0 , tel)

  tyViewToTel : TyViewTel → Telescope
  tyViewToTel = L.map λ where (abs s a) → s , a

  hideTyView : Abs (Arg A) → Abs (Arg A)
  hideTyView (abs s (arg (arg-info _ m) x)) = abs s (arg (arg-info hidden m) x)

  -- The type of a Convertible instance. For parameterised types adds the appropriate instance
  -- arguments and instantiates level arguments to lzero. For instance,
  -- instanceType _⊎_ Hs.Either = {A B : Set} {a b : Set} → ⦃ Convertible A a ⦄ → ⦃ Convertible B b ⦄
  --                              Convertible (A ⊎ B) (Hs.Either a b)
  instanceType : (agdaName hsName : Name) → TC TypeView
  instanceType agdaName hsName = do
    aLvls , agdaParams ← smashLevels <$> getParamsAndIndices agdaName
    hLvls , hsParams   ← smashLevels <$> getParamsAndIndices hsName
    true ← return (length agdaParams == length hsParams)
      where false → liftTC $ R.typeErrorFmt "%q and %q have different number of parameters" agdaName hsName
    let n = length agdaParams
        l₀ = quote 0ℓ ∙
    agdaTy ← applyWithVisibility agdaName $ L.replicate aLvls l₀ ++ L.map ♯ (take n (downFrom (n + n)))
    hsTy   ← applyWithVisibility hsName   $ L.replicate hLvls l₀ ++ L.map ♯ (downFrom n)
    let instHead = weaken n $ quote Convertible ∙⟦ agdaTy ∣ hsTy ⟧
        tel      = L.map hideTyView (agdaParams ++ hsParams) ++
                   L.replicate n (abs "_" (iArg (quote Convertible ∙⟦ ♯ (n + n ∸ 1) ∣ ♯ (n ∸ 1) ⟧)))
    return $ tel , instHead

  -- Compute one clause of the Convertible instance. For instance,
  -- conversionClause Convertible.to to ((c₁ , _) , (c₂ , _)) generates
  -- .to (c₁ x₁ .. xn) = c₂ (to x₁) .. (to xn)
  -- where the xi are the visible constructor arguments.
  conversionClause : Name → Name → (Name × Type) × (Name × Type) → TC Clause
  conversionClause prjP prjE ((fromC , fromTy) , (toC , toTy)) = do
    let isVis   = λ { (abs _ (arg (arg-info visible _) _)) → true; _ → false }
        fromTel = L.filterᵇ isVis (proj₁ (viewTy fromTy))
        toTel   = L.filterᵇ isVis (proj₁ (viewTy toTy))
        n       = length fromTel
        mkCon  c mkArg = Term.con c    $ L.map (vArg ∘ mkArg ∘ ♯)  (downFrom n)
        mkConP c mkArg = Pattern.con c $ L.map (vArg ∘ mkArg ∘ `_) (downFrom n)
    true ← return (n == length toTel)
      where false → liftTC $ R.typeErrorFmt "%q and %q have different number of arguments" fromC toC
    return $ clause (tyViewToTel $ L.map (λ where (abs x (arg i _)) → abs x (arg i unknown)) fromTel)
                    (vArg (proj prjP) ∷ vArg (mkConP fromC id) ∷ [])
                    (mkCon toC (prjE ∙⟦_⟧))

  -- Compute the clauses of a convertible instance.
  instanceClauses : (agdaName hsName : Name) → TC (List Clause)
  instanceClauses agdaName hsName = do
    agdaCons ← getConstrs agdaName
    hsCons   ← getConstrs hsName
    agdaPars ← length <$> getParamsAndIndices agdaName
    hsPars   ← length <$> getParamsAndIndices hsName
    true ← return (length agdaCons == length hsCons)
      where false → liftTC $ R.typeErrorFmt "%q and %q have different number of constructors" agdaName hsName
    toClauses   ← mapM (conversionClause (quote Convertible.to)   (quote to)  ) (L.zip agdaCons hsCons)
    fromClauses ← mapM (conversionClause (quote Convertible.from) (quote from)) (L.zip hsCons agdaCons)
    return $ toClauses ++ fromClauses

  absurdClause : Name → Clause
  absurdClause prj = absurd-clause (("x" , vArg unknown) ∷ []) (vArg (proj prj) ∷ vArg (absurd 0) ∷ [])

  -- Compute conversion clauses for the current goal and wrap them in a pattern lambda.
  patternLambda : TC Term
  patternLambda = do
    quote Convertible ∙⟦ `A ∣ `B ⟧ ← reduce =<< goalTy
      where t → liftTC $ R.typeErrorFmt "Expected Convertible A B, got %t" t
    agdaCons ← getConstrsForType `A
    hsCons   ← getConstrsForType `B
    toClauses   ← mapM (conversionClause (quote Convertible.to)   (quote to)  ) (L.zip agdaCons hsCons)
    fromClauses ← mapM (conversionClause (quote Convertible.from) (quote from)) (L.zip hsCons agdaCons)
    case toClauses ++ fromClauses of λ where
      []  → return $ pat-lam (absurdClause (quote Convertible.to) ∷ absurdClause (quote Convertible.from) ∷ []) []
      cls → return $ pat-lam cls []

doPatternLambda : Term → R.TC Term
doPatternLambda hole = patternLambda =<< initTCEnvWithGoal hole

-- Deriving a Convertible instance. Usage
--   unquoteDecl iName = deriveConvertible iName (quote AgdaTy) (quote HsTy)
deriveConvertible : Name → Name → Name → R.TC ⊤
deriveConvertible instName agdaName hsName = initUnquoteWithGoal ⦃ defaultTCOptions ⦄ (agda-sort (lit 0)) do
  agdaDef ← getDefinition agdaName
  hsDef   ← getDefinition hsName
  -- instName ← freshName $ "Convertible" S.++ show hsName
  instTel , instTy ← instanceType agdaName hsName
  inst    ← declareDef (iArg instName) (tyView (instTel , instTy))
  clauses ← instanceClauses agdaName hsName
  defineFun instName clauses
  return _

-- Macros providing an alternative interface. Usage
--   iName : ConvertibleType AgdaTy HsTy
--   iName = autoConvertible
-- or
--   iName = autoConvert AgdaTy
macro
  ConvertibleType : Name → Name → Tactic
  ConvertibleType agdaName hsName = initTac ⦃ defaultTCOptions ⦄ $
    unifyWithGoal ∘ tyView =<< instanceType agdaName hsName

  autoConvertible : Tactic
  autoConvertible = initTac ⦃ defaultTCOptions ⦄ $
    unifyWithGoal =<< patternLambda

  autoConvert : Name → Tactic
  autoConvert d hole = do
    hsTyMeta ← R.newMeta `Set
    R.checkType hole $ quote Convertible ∙⟦ d ∙ ∣ hsTyMeta ⟧
    hsTy ← solveHsType (d ∙)
    R.unify hsTyMeta hsTy
    R.unify hole =<< doPatternLambda hole
