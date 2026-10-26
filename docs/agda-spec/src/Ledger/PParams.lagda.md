# Protocol Parameters

<a id="sec:protocol-parameters"></a>
This section defines the adjustable protocol parameters of the Cardano consensus.
These parameters are used in block header validation and can affect various features
of the system, such as maximum and minimum sizes of certain components, and more.

<!--
```agda
{-# OPTIONS --safe #-}

open import Data.Product.Properties
open import Data.Nat.Properties using (m+1+n≢m)

open import Tactic.Derive.DecEq
open import Tactic.Derive.Show

open import Ledger.Prelude

module Ledger.PParams where

private variable
  m n : ℕ
```
-->

```agda
ProtVer : Type
ProtVer = ℕ × ℕ
```

<!--
```agda
instance
  Show-ProtVer : Show ProtVer
  Show-ProtVer = Show-×
```
-->

```agda
data pvCanFollow : ProtVer → ProtVer → Type where
  canFollowMajor : pvCanFollow (m , n) (m + 1 , 0)
  canFollowMinor : pvCanFollow (m , n) (m , n + 1)
```

**Definitions related to protocol parameters**
<a id="fig:protocol-parameter-defs"></a>

```agda
data PParamGroup : Type where
  NetworkGroup : PParamGroup
record PParams : Type where

  field
```

*Network group*

```agda
    maxHeaderSize        : ℕ
    maxBlockSize         : ℕ
    pv                   : ProtVer
```

*Network group (Leios)*

```agda
    leiosHeaderPeriod    : Milliseconds
    leiosVotingPeriod    : Milliseconds
    leiosDiffusionPeriod : Milliseconds
```

**Protocol parameter definitions**
<a id="fig:protocol-parameter-declarations"></a>

```agda
positivePParams : PParams → List ℕ
positivePParams pp = maxHeaderSize ∷ maxBlockSize ∷ []
```

<!--
```agda
  where open PParams pp
```
-->

```agda

paramsWellFormed : PParams → Type
paramsWellFormed pp = 0 ∉ fromList (positivePParams pp)
```

<!--
```agda
paramsWF-elim : (pp : PParams) → paramsWellFormed pp → (n : ℕ) → n ∈ˡ (positivePParams pp) → n > 0
paramsWF-elim pp pwf (suc n) x = z<s
paramsWF-elim pp pwf 0 0∈ = ⊥-elim (pwf (to ∈-fromList 0∈))
  where open Equivalence
```
-->

**Protocol parameter well-formedness**
<a id="fig:protocol-parameter-well-formedness"></a>

<!--
```agda
instance
  unquoteDecl DecEq-PParams        = derive-DecEq
    ((quote PParams , DecEq-PParams) ∷ [])
  unquoteDecl Show-PParams        = derive-Show
    ((quote PParams , Show-PParams) ∷ [])
```
-->

`PParams` contains parameters used in the Cardano consensus, which we group according
to the general purpose that each parameter serves.

- `NetworkGroup`: parameters related to the network settings;

The Leios parameters governing the timing of endorser block announcement, voting
and diffusion belong to the `NetworkGroup`. They are wall-clock durations rather
than slot counts, so that sub-second delivery periods can be expressed.

They are deliberately absent from `positivePParams`: zero values are the
protocol's disabled state, and governance must be able to reach it.

[Protocol parameter definitions](PParams.lagda.md#fig:protocol-parameter-declarations) also defines the
function `paramsWellFormed`. It performs some sanity checks on protocol
parameters.

<!--
```agda
instance
  pvCanFollow? : ∀ {pv} {pv'} → Dec (pvCanFollow pv pv')
  pvCanFollow? {m , n} {pv} with pv ≟ (m + 1 , 0) | pv ≟ (m , n + 1)
  ... | no ¬p    | no ¬p₁   = no $ λ where canFollowMajor → ¬p  refl
                                           canFollowMinor → ¬p₁ refl
  ... | no ¬p    | yes refl = yes canFollowMinor
  ... | yes refl | no ¬p    = yes canFollowMajor
  ... | yes refl | yes p    = ⊥-elim $ m+1+n≢m m $ ×-≡,≡←≡ p .proj₁
```
-->

[Specification contents](../../SPECIFICATION.md)
