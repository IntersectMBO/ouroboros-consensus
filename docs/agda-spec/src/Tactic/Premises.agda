{-# OPTIONS --safe #-}
-- {-# OPTIONS -v tactic.premises:100 #-}
module Tactic.Premises where

open import Prelude hiding (Type)
open import PreludeMeta
open import MetaPrelude using (enumerate)

import Data.List as L
open import Data.Fin using (toℕ)

open import Class.Show
open import Class.Foldable

open import Interface.STS

open Debug ("tactic.premises", 100)
open import Reflection.Ext

{-
** Extracting the hypotheses of an STS rule.

Constructors should be formulated in the STS syntax for inference rules, i.e.
```
  rule : ∀ <IMPLICIT_ARGUMENTS + LET_EXPRESSIONS>
    ∙ <HYPOTHESIS#1>
    ∙ <HYPOTHESIS#2>
    ⋮
    ∙ <HYPOTHESIS#n-1>
    ∙ <HYPOTHESIS#n>
      ────────────────────────────────
      <CONCLUSION>
```
This meta-program then generates the type of the (decidable) hypotheses of the rule,
as well as a proof that it is in fact decidable, i.e.
```
rule-premises : ∀ ⋯ → Set
rule-premises ⋯ =
  ∙ <HYPOTHESIS#1>
  ∙ <HYPOTHESIS#3>
  ⋮
  ∙ <HYPOTHESIS#n-1>

rule-premises? : ∀ ⋯ → Dec (rule-premises ⋯)
rule-premises? = it
```
where ⋯ stands for the subset of implicit arguments that are relevant for the
selected subset of decidable hypotheses.

To be precise, we generate both definitions above under a single name, as well as
wrapping the decidability proof in the _⁇ typeclass from `Class.Decidable`, i.e.
```
unquoteDecl rule-premises = genPremises rule-premises (quote rule)
-- rule-premises        : ∀ ⋯ → ∃ _⁇
-- rule-premises .proj₁ : ∀ ⋯ → Set
-- rule-premises .proj₂ : ∀ ⋯ → (rule-premises .proj₁ ⋯) ⁇
```
-}
genPremises : Name → Name → TC ⊤
genPremises f n = do
  print $ "*** Generating premises for constructor:" <+> show n <+> "***"
  ty ← reduceRuleSyntax =<< getType n             -- (1)
  let is , hs = breakImplicits $ viewTy ty .proj₁ -- (2)
  hs ← map unbundleHypotheses                     -- (3)
     $ extendContextTel (absTelescope is)
     $ mapM reduceRuleSyntax hs -- reducing again to work around Agda issue #6951
  dhs ← filterM (isDecidable? is) hs              -- (4)
  let dhs = bundleHypotheses dhs                  -- (5)
  is , premise ← removeUnusedImplicits is dhs     -- (6)
  let premiseTy  = tyView (is , quote ∃⁇ ∙)
      premiseTel = absTelescope is
      premisePs  = map (λ (i , x) → arg (argInfo $ unAbs x) (` i))
                       (uncurry L.zip $ map₁ reverse $ unzip $ enumerate is)
      ∃premise?  = quote _,_ ◆⟦ premise ∣ quote it ∙ ⟧
      cl         = clause premiseTel premisePs ∃premise?
  print $ "```\n" <> show f <+> ":" <+> show premiseTy <> "\n"
       -- <> show f <+> "⋯ =" <+> show premise <> " , it\n```\n"
       -- TOO EXPENSIVE FOR LARGE RULES (even without `-v` flag)
  declareDef (vArg f) premiseTy
  defineFun f [ cl ]
  where
  -- (1) unfold syntactic sugar from Interface.STS
  reduceRuleSyntax : Type → TC Type
  reduceRuleSyntax ty = do
    let whitelist = Interface.STS._∙_
                 `∷ Interface.STS.∙_
                 `∷ Interface.STS._────────────────────────────────_
                 `∷ Interface.STS.────────────────────────────────_
                 `∷ Interface.STS._───────────────────────────────────────_
                 `∷ Interface.STS.───────────────────────────────────────_
                 -- ^ include all syntactic sugar for inference rules here
                 `∷ []
    withReduceDefs (true , whitelist) $ normalise ty

  -- (2) separate implicit arguments from visible hypotheses
  breakImplicits : AbsTelescope → AbsTelescope × List Type
  breakImplicits = map₂  (unArgs ∘ map unAbs)
                 ∘ break (isVisible? ∘ unAbs)

  -- (3) unbundle hypotheses (always a single product due to `∙`)
  --     * TODO: generalise to curried rules (which use `→`)
  unbundleHypotheses : List Type → List Type
  unbundleHypotheses = concatMap go where go = λ where
    (def (quote _×_) (hArg _ ∷ hArg _ ∷ vArg x ∷ vArg ys ∷ [])) →
      x ∷ go ys
    t → [ t ]

  -- (4) select only the hypotheses that are decidable
  isDecidable? : AbsTelescope → Type → TC Bool
  isDecidable? tel ty =
    isSuccessful $ checkType (quote it ∙) (tyView (tel , quote _⁇ ∙⟦ ty ⟧))

  -- (5) bundle the (decidable) hypotheses back together in a single product
  bundleHypotheses : List Type → Type
  bundleHypotheses = fold where instance _ = Monoid-Term-×; _ = Semigroup-Term-×

  -- (6) minimize the implicit arguments to only the ones that are actually used in (5)
  removeUnusedImplicits : AbsTelescope → Type → TC (AbsTelescope × Type)
  removeUnusedImplicits is ty = go (pred $ length is) is ty
    where
    fvs = freeVars ty

    go : ℕ → AbsTelescope → Type → TC (AbsTelescope × Type)
    go _ []       t = return ([] , t)
    go n (i ∷ is) t =
      ifᵈ n ∈ˡ fvs then -- relevant hypothesis
        map₁ (i ∷_) <$> go (pred n) is t
      else -- irrelevant hypothesis
        go (pred n) is (mapFreeVars pred n t)

-- ** Tests.
private

  -- * The tactics works for one or zero hypotheses.
  data ℝ : ℕ → ℕ → Set where
    base :
      ────────────────────────────────
      ℝ 0 0

    step : ∀ {n m} →
      ∙ ℝ n m
      ────────────────────────────────
      ℝ (suc n) (suc m)

  module _ ⦃ _ : ℝ ⁇² ⦄ where
    unquoteDecl ℝ-base-premises = genPremises ℝ-base-premises (quote ℝ.base)
    unquoteDecl ℝ-step-premises = genPremises ℝ-step-premises (quote ℝ.step)

    _ : ℝ-base-premises .proj₁
    _ = tt

    _ : ℝ-base-premises .proj₂ .dec ≡ yes tt
    _ = refl

    module _ (n m : ℕ) (𝕣 : ℝ n m) where
      _ : ℝ-step-premises .proj₁
      _ = 𝕣

      _ : (ℝ n m) ⁇
      _ = ℝ-step-premises .proj₂

  open import Class.Monoid

  -- * The tactic works for multiple *decidable* hypotheses.
  data ℚ {A : Set} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ : A → A → Set where
    base :
      ────────────────────────────────
      ℚ ε ε

    step : ∀ {n m n′ m′} →
      ∙ ℚ n m
      ∙ ℚ n′ m′
      ────────────────────────────────
      ℚ (n ◇ n′) (m ◇ m′)

  module _ ⦃ _ : ∀ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ → ℚ {A} ⁇² ⦄ where
    unquoteDecl ℚ-base-premises = genPremises ℚ-base-premises (quote ℚ.base)
    unquoteDecl ℚ-step-premises = genPremises ℚ-step-premises (quote ℚ.step)

    _ : ℚ-base-premises .proj₁
    _ = tt

    module _ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ (n m n′ m′ : A) (𝕢ˡ : ℚ n m) (𝕢ʳ : ℚ n′ m′) where
      _ : ℚ-step-premises .proj₁
      _ = 𝕢ˡ , 𝕢ʳ

  -- * The tactic omits *undecidable* hypotheses.
  data ℝ′ : ℕ → ℕ → Set where
    base :
      ────────────────────────────────
      ℝ′ 0 0

    step : ∀ {n m} {T T′ : ℕ → ℕ} →
      ∙ ℝ′ n m
      ∙ T ≡ T′
      ────────────────────────────────
      ℝ′ (suc n) (suc m)

  module _ ⦃ _ : ℝ′ ⁇² ⦄ where
    unquoteDecl ℝ′-base-premises = genPremises ℝ′-base-premises (quote ℝ′.base)
    unquoteDecl ℝ′-step-premises = genPremises ℝ′-step-premises (quote ℝ′.step)

    _ : ℝ′-base-premises .proj₁
    _ = tt

    module _ (n m : ℕ) (𝕣 : ℝ′ n m) where
      _ : ℝ′-step-premises .proj₁
      _ = 𝕣

  -- * The tactic works under module contexts.
  module _ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ ⦃ _ : ℚ {A} ⁇² ⦄ ⦃ _ : ℝ ⁇² ⦄ where

    data 𝕎 : A → ℕ → Set where
      base :
        ────────────────────────────────
        𝕎 ε 0

      step : ∀ {x y n n′} →
        ∙ ℝ n n′
        ∙ ℚ x y
        ∙ 𝕎 (x ◇ y) n
        ────────────────────────────────
        𝕎 (x ◇ y) n′

    unquoteDecl 𝕎-base-premises = genPremises 𝕎-base-premises (quote 𝕎.base)

    _ : 𝕎-base-premises .proj₁
    _ = tt

    unquoteDecl 𝕎-step-premises = genPremises 𝕎-step-premises (quote 𝕎.step)

    module _ (n n′ : ℕ) (x y : A) (𝕣 : ℝ n n′) (𝕢 : ℚ x y) where
      _ : 𝕎-step-premises .proj₁
      _ = 𝕣 , 𝕢

  -- * Irrelevant free variable `z` is not included in the premise type,
  --   since it only appears in the conclusion.
  data 𝕍 {A : Set} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ : A → A → Set where
    base :
      ────────────────────────────────
      𝕍 ε ε

    step : ∀ {n m n′ m′ z} ⦃ _ : Show A ⦄ →
      ∙ 𝕍 n m
      ∙ 𝕍 n′ m′
      ────────────────────────────────
      𝕍 (n ◇ n′ ◇ m ◇ m′) z

  module _ ⦃ _ : ∀ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ → 𝕍 {A} ⁇² ⦄ where
    unquoteDecl 𝕍-base-premises = genPremises 𝕍-base-premises (quote 𝕍.base)
    unquoteDecl 𝕍-step-premises = genPremises 𝕍-step-premises (quote 𝕍.step)

    _ : 𝕍-base-premises .proj₁
    _ = tt

    module _ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄
             (n m n′ m′ : A) (𝕢ˡ : 𝕍 n m) (𝕢ʳ : 𝕍 n′ m′) where
      _ : 𝕍-step-premises .proj₁
      _ = 𝕢ˡ , 𝕢ʳ

  -- * Irrelevant free variable `T` is not included in the premise type,
  --   since it only appears in an undecidable hypothesis.
  data 𝕍′ {A : Set} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ : A → A → Set where
    base :
      ────────────────────────────────
      𝕍′ ε ε

    step : ∀ {n m n′ m′} ⦃ _ : Show A ⦄ {T : Type} →
      ∙ 𝕍′ n m
      ∙ 𝕍′ n′ m′
      ∙ (∀ T′ → T ≡ T′)
      ────────────────────────────────
      𝕍′ (n ◇ n′) (m ◇ m′)

  module _ ⦃ _ : ∀ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ → 𝕍′ {A} ⁇² ⦄ where
    unquoteDecl 𝕍′-base-premises = genPremises 𝕍′-base-premises (quote 𝕍′.base)
    unquoteDecl 𝕍′-step-premises = genPremises 𝕍′-step-premises (quote 𝕍′.step)

    _ : 𝕍′-base-premises .proj₁
    _ = tt

    module _ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄
             (n m n′ m′ : A) (𝕢ˡ : 𝕍′ n m) (𝕢ʳ : 𝕍′ n′ m′) where
      _ : 𝕍′-step-premises .proj₁
      _ = 𝕢ˡ , 𝕢ʳ

  -- * Irrelevant instance `⦃ Show A ⦄` is not included in the premise type.
  data ℚ′ {A : Set} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ : A → A → Set where
    base :
      ────────────────────────────────
      ℚ′ ε ε

    step : ∀ {n m n′ m′} ⦃ _ : Show A ⦄ →
      ∙ ℚ′ n m
      ∙ ℚ′ n′ m′
      ────────────────────────────────
      ℚ′ (n ◇ n′) (m ◇ m′)

  module _ ⦃ _ : ∀ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄ → ℚ′ {A} ⁇² ⦄ where
    unquoteDecl ℚ′-base-premises = genPremises ℚ′-base-premises (quote ℚ′.base)
    unquoteDecl ℚ′-step-premises = genPremises ℚ′-step-premises (quote ℚ′.step)

    _ : ℚ′-base-premises .proj₁
    _ = tt

    module _ {A} ⦃ _ : Semigroup A ⦄ ⦃ _ : Monoid A ⦄
             (n m n′ m′ : A) (𝕢ˡ : ℚ′ n m) (𝕢ʳ : ℚ′ n′ m′) where
      _ : ℚ′-step-premises .proj₁
      _ = 𝕢ˡ , 𝕢ʳ
