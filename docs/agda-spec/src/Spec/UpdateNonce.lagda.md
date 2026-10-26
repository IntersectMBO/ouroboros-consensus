# UPDN Transition

<a id="sec:update-nonces-trans"></a>

The Update Nonce Transition ($\mathsf{UPDN}$) updates the nonces until the randomness gets fixed.
The environment consists of the block nonce `η`.
The state consists of the candidate nonce `ηc` and the evolving nonce `ηv`.

<!--
```agda
{-# OPTIONS --safe #-}
open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)
module Spec.UpdateNonce
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  where
open import Ledger.Prelude
```
-->

*Update Nonce environments*

```agda
record UpdateNonceEnv : Type where
  constructor ⟦_⟧ᵘᵉ
  field
    η : Nonce -- new nonce
```

*Update Nonce states*

```agda
record UpdateNonceState : Type where
  constructor ⟦_,_⟧ᵘˢ
  field
    ηv : Nonce -- evolving nonce
    ηc : Nonce -- candidate nonce
```

*Update Nonce transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonceState → Slot → UpdateNonceState → Type
```

<a id="fig:ts-types:updnonce"></a>

The transition rule $\mathsf{UPDN}$ takes the slot `s` as signal and is shown below.
There are two different cases for $\mathsf{UPDN}$: one where more than
`RandomnessStabilisationWindow`[^pre-conway-stability-window] slots remain until
the start of the next epoch, and one where at most `RandomnessStabilisationWindow`
slots remain.

Note that in the first rule, the candidate nonce `ηc` transitions to
`ηv` `⋆` `η`, not `ηc` `⋆` `η`. The reason for this is that even
though the candidate nonce is frozen sometime during the epoch, we want the two
nonces to again be equal at the start of a new epoch.

<!--
```agda
private variable
  s       : Slot
  η ηv ηc : Nonce
data _⊢_⇀⦇_,UPDN⦈_ where
```
-->

```agda
  Update-Both :
    ∙ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηv ⋆ η ⟧ᵘˢ

  Only-Evolve :
    ∙ s + RandomnessStabilisationWindow ≥ firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηc ⟧ᵘˢ
```

<a id="fig:ts-rules:updatenonce"></a>

[^pre-conway-stability-window]: Note that in pre-Conway eras `StabilityWindow` was used instead of `RandomnessStabilisationWindow`.

[Specification contents](../../SPECIFICATION.md)
