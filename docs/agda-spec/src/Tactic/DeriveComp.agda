{-# OPTIONS -v allTactics:100 #-}
{-# OPTIONS --safe #-}
module Tactic.DeriveComp where

open import Prelude hiding (Type)
open import PreludeMeta hiding (TC) renaming (TCI to TC)
open import MetaPrelude using (zipWithIndex)
open import Class.Traversable

import Data.List.NonEmpty as NE
open import Data.Maybe.Properties using (just-injective)

open import Interface.ComputationalRelation
open import Interface.HasAdd; open import Interface.HasAdd.Instance
open import Interface.HasSubtract; open import Interface.HasSubtract.Instance

open import Reflection.Ext using (extendContextTel′)
open import Tactic.ClauseBuilder
open import Tactic.ReduceDec
open import MyDebugOptions

pattern ``yes x = quote _because_ ◇⟦ quote true ◇  ∣ quote ofʸ ◇⟦ x ⟧ ⟧
pattern ``no x  = quote _because_ ◇⟦ quote false ◇ ∣ quote ofⁿ ◇⟦ x ⟧ ⟧

instance _ = ContextMonad-MonadTC

record STSConstr : Set where
  field
    name    : Name
    params  : List $ Abs $ Arg Type
    clauses : List Type
    context : Pattern
    state   : Pattern
    signal  : Pattern
    result  : Term

conOrVarToPattern : ℕ → Term → Maybe Pattern
conOrVarToPattern k (♯ v) = just (Pattern.var (v - k))
conOrVarToPattern k (con c args) =
  Pattern.con c <$> (sequence $ conOrVarToPattern′ k args)
  where
    conOrVarToPattern′ : ℕ → List (Arg Term) → List (Maybe (Arg Pattern))
    conOrVarToPattern′ k = λ where
      [] → []
      ((arg i x) ∷ l) → (arg i <$> conOrVarToPattern k x) ∷ conOrVarToPattern′ k l
conOrVarToPattern _ _ = nothing

isArg : (a : Abs (Arg Term)) → Dec _
isArg a = ¬? (getVisibility (unAbs a) ≟ visible)

toSTSConstr : Name × TypeView → TC STSConstr
toSTSConstr (n , (cs , def _ args))
  with args | mapMaybe (conOrVarToPattern (length $ dropWhile isArg cs) ∘ unArg)
                       (take 3 args)
... | _ ∷ _ ∷ _ ∷ r ∷ [] | c ∷ s ∷ sig ∷ [] =
  return record
    { name = n
    ; params = takeWhile isArg cs
    ; clauses = zipWithIndex (λ i → mapVars (_- i))
              $ (unArg ∘ unAbs) <$> dropWhile isArg cs
    ; context = c
    ; state = s
    ; signal = sig
    ; result = mapVars (_- (length $ dropWhile isArg cs)) $ unArg r }
... | l | l' =
  error1 ("toSTSConstr: wrong number of arguments:"
    <+> show (length l) <+> "," <+> show (length l'))
toSTSConstr _ = error1 "toSTSConstr: wrong constructor"

errorIfNothing : ∀ {a} {A : Set a} → Maybe A → String → TC A
errorIfNothing (just x) s = return x
errorIfNothing nothing s = error1 s

getSTSConstrs : Name → TC (List STSConstr)
getSTSConstrs n = inDebugPath "getSTSConstrs" do
  dataDef ← getDataDef n
  res ← traverse toSTSConstr (DataDef.constructors dataDef)
  debugLogᵐ (res ᵛⁿ ∷ᵈᵐ []ᵐ)
  return res

generatePred : List Term → Term
generatePred clauses = quote ¿_¿ ∙⟦ helper clauses ⟧
  where
    helper : List Term → Term
    helper [] = quoteTerm ⊤
    helper (x ∷ []) = x
    helper (x ∷ x₁ ∷ l) = quote _×_ ∙⟦ x ∣ helper (x₁ ∷ l) ⟧

predWitness : ℕ → Term
predWitness 0 = quoteTerm tt
predWitness (suc n) = helper n
  where
    helper : ℕ → Term
    helper 0 = ♯ 0
    helper (suc n) = quote _,_ ◆⟦ ♯ (suc n) ∣ helper n ⟧

curryPredProof : ℕ → Term → List (Arg Term)
curryPredProof 0 t = []
curryPredProof 1 t = [ vArg t ]
curryPredProof 2 t = quote proj₁ ∙⟦ t ⟧ ⟨∷⟩ quote proj₂ ∙⟦ t ⟧ ⟨∷⟩ []
curryPredProof (suc (suc (suc k))) t =
  quote proj₁ ∙⟦ t ⟧ ⟨∷⟩ curryPredProof (suc (suc k)) (quote proj₂ ∙⟦ t ⟧)

computeFunctionBody : Term → Term → Term
computeFunctionBody g result =
  quote if_then_else_ ∙⟦ g ∣ quote just ◆⟦ result ⟧ ∣ quote nothing ◆ ⟧
  where open import Data.Bool

generateFunctionClause : (List Term → Term) → STSConstr → Clause
generateFunctionClause genPred c = let open STSConstr c in
  ⟦ context ∣ state ∣ signal ⦅ (λ { (abs s x) → (s , x) }) <$> params ⦆⇒
    computeFunctionBody (quote ⌊_⌋ ∙⟦ genPred clauses ⟧) result ⟧

generateFunction : List STSConstr → Term
generateFunction c = pat-lam (generateFunctionClause generatePred <$> c) []

rdOpts : ReductionOptions
rdOpts = onlyReduce [ quote ⌊_⌋ ]

open ClauseExprM

derive⇐ : List STSConstr → ITactic
derive⇐ (record { clauses = clauses; result = result } ∷ []) = inDebugPath "derive⇐" do
  (constrPat ∷ []) ← currentTyConstrPatterns
    where _ → error1 "TODO: Support more than one constructor!"
  expr ← singleMatchExpr constrPat $ finishMatch $ withGoalHole $ do
    let n = 1 + length clauses
        pred = weaken n $ generatePred clauses
        scheme = `λ "g" ⇒ (quote _≡_ ∙⟦ computeFunctionBody (♯ 0) (weaken (1 + n) result)
                                      ∣ quote just ◆⟦ weaken (1 + n) result ⟧ ⟧)
        eq = quote fromWitness' ∙⟦ pred ∣ predWitness (length clauses) ⟧
    unifyWithGoal $ quote subst ∙⟦ scheme ∣ quote sym ∙⟦ eq ⟧ ∣ quote refl ◆ ⟧
  unifyWithGoal $ clauseExprToPatLam expr
derive⇐ _ = error1 "TODO: support multiple constructors"

derive⇒ : Name → List STSConstr → ITactic
derive⇒ n
  (record { name = stsConstrName ; clauses = clauses; result = result } ∷ [])
  = inDebugPath "derive⇒" do
    let pred = weaken 3 (generatePred clauses)
    expr ← introsExpr (from-just $ NE.fromList ("h" ⟨∷⟩ [])) $ finishMatch $
      caseMatch (mapVars (_+ 2) $ generatePred clauses) $ matchExprM
        ((multiSinglePattern [ "" ] (vArg (``no  (` 0))) , finishMatch do
          let reducedHyp = quote subst
                ∙⟦ `λ "g" ⇒ (quote _≡_ ∙⟦ computeFunctionBody (♯ 0) (weaken 4 result)
                                        ∣ quote just ◆⟦ ♯ 3 ⟧ ⟧)
                 ∣ quote fromWitnessFalse' ∙⟦ pred ∣ ♯ 0 ⟧
                 ∣ ♯ 1
                 ⟧
          return $ quote case_of_ ∙⟦ reducedHyp ∣ `λ∅ ⟧) ∷
        (multiSinglePattern [ "" ] (vArg (``yes (` 0))) , finishMatch do
          let reducedHyp = quote subst
                ∙⟦ `λ "g" ⇒ (quote _≡_ ∙⟦ computeFunctionBody (♯ 0) (weaken 4 result)
                                        ∣ quote just ◆⟦ ♯ 3 ⟧ ⟧)
                 ∣ quote fromWitness' ∙⟦ pred ∣ ♯ 0 ⟧
                 ∣ ♯ 1
                 ⟧
          ty ∙⟦ c ∣ s ∣ sig ∣ s' ⟧ ← goalTy
            where ty → error ("BUG: Unexpected type" ∷ᵈ ty ∷ᵈ [])
          return $ quote subst
            ∙⟦ ty ∙⟦ c ∣ s ∣ sig ⟧
             ∣ quote just-injective ∙⟦ reducedHyp ⟧
             ∣ con stsConstrName (curryPredProof (length clauses) (♯ 0))
             ⟧) ∷ [])
    unifyWithGoal $ clauseExprToPatLam expr
derive⇒ _ _ = error1 "TODO: support multiple constructors"

module _ ⦃ _ : DebugOptions ⦄ where
  derive⇔ : Name → List STSConstr → Tactic
  derive⇔ n stsConstrs = initTac $ inDebugPath "derive⇔" do
    hole⇒ ← newMeta unknown
    hole⇐ ← newMeta unknown
    unifyWithGoal $ quote mk⇔ ∙⟦ hole⇒ ∣ hole⇐ ⟧
    runWithHole hole⇐ $ derive⇐ stsConstrs
    runWithHole hole⇒ $ derive⇒ n stsConstrs

  deriveComp : Name → List STSConstr → TC ⊤
  deriveComp definedType stsConstrs = do
    debugLog ("\nDerive computation function for: " ∷ᵈ definedType ∷ᵈ [])
    unifyWithGoal (generateFunction stsConstrs)

  deriveComputational : Name → Name → UnquoteDecl
  deriveComputational n compName =
    initUnquoteWithGoal (quoteTerm Name) $ inDebugPath "deriveComputational" do
      let goalTy = quote Computational ∙⟦ n ∙ ⟧
      debugLog1 goalTy
      declareDef (vArg compName) goalTy
      stsConstrs ← getSTSConstrs n
      compRes ← withSafeReset $ do
        compHole ← newMeta unknown
        equivHole ← newMeta unknown
        definition ← mkRecord (quote Computational) (compHole ⟨∷⟩ equivHole ⟨∷⟩ [])
        _ ← checkType definition goalTy
        debugLog1ᵐ (compHole ᵗ)
        runWithHole compHole $ deriveComp n stsConstrs
        reduce compHole
      debugLog ("compRes: " ∷ᵈ compRes ∷ᵈ [])
      cTy   ← newMeta unknown
      sTy   ← newMeta unknown
      sigTy ← newMeta unknown
      let isoCxt = Telescope ∋ ("c" , hArg cTy)
                 ∷ ("s" , hArg (weaken 1 sTy))
                 ∷ ("sig" , hArg (weaken 2 sigTy))
                 ∷ ("s'" , hArg (weaken 3 sTy))
                 ∷ []
      iso ← extendContextTel′ isoCxt $ newMeta unknown
      let isolam = `λ⟅ "c" ⟆⇒ `λ⟅ "s" ⟆⇒ `λ⟅ "sig" ⟆⇒ `λ⟅ "s'" ⟆⇒ iso
      definition ← mkRecord (quote Computational) (compRes ⟨∷⟩ isolam ⟨∷⟩ [])
      defineFun compName [ nonBindingClause definition ]
      extendContextTel′ isoCxt $ λ _ → derive⇔ n stsConstrs iso

-- private module _ {A B : Set} ⦃ _ : DecEq A ⦄ ⦃ _ : DecEq B ⦄ where
--   variable
--     c : A × B
--     s s' : A
--     sig : B

--   data Test : A × B → A → B → A → Set where
--     test : proj₁ c ≡ s
--          → proj₂ c ≡ sig
--          ------------------------
--          → Test c s sig (proj₁ c)

--   unquoteDecl Computational-Test = deriveComputational (quote Test) Computational-Test

--   -- Sanity checks
--   testFun : A × B → A → B → Maybe A
--   testFun c s sig =
--     if ⌊ ¿ proj₁ c ≡ s × proj₂ c ≡ sig ¿ ⌋ then just (proj₁ c) else nothing

--   testFunPf⇒ : testFun c s sig ≡ just s' → Test c s sig s'
--   testFunPf⇒ {c} {s} {sig} h = case ¿ proj₁ c ≡ s × proj₂ c ≡ sig ¿ of λ where
--     (no ¬p) → case by-reduceDec h of λ ()
--     (yes p) → subst (Test c s sig) (just-injective $ by-reduceDec h)
--             $ test (proj₁ p) (proj₂ p)

--   testFunPf⇐ : Test c s sig s' → testFun c s sig ≡ just s'
--   testFunPf⇐ {s = s} (test x x₁) = by-reduceDecInGoal (refl {x = just s})

--   Computational-Test-Manual : Computational Test
--   Computational-Test-Manual = record
--     { compute = testFun ; ≡-just⇔STS = mk⇔ testFunPf⇒ testFunPf⇐ }

--   _ : ∀ {c s sig} → testFun c s sig ≡ Computational.compute Computational-Test c s sig
--   _ = refl
