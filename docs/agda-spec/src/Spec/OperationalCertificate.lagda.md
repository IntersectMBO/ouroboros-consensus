# OCERT Transition

<a id="sec:oper-cert-trans"></a>

The Operational Certificate Transition ($\mathsf{OCERT}$) validates
the operational certificate and signature in the block header and updates the mapping
of operational certificate issue numbers. The environment consists of the set of stake pools
and the state consists of the mapping of operational certificate issue numbers. Its signal is a block header.

<!--
```agda
{-# OPTIONS --safe #-}
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions

module Spec.OperationalCertificate
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (bs     : BlockStructure crypto nonces es) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  where

open import Spec.BaseTypes crypto using (OCertCounters)
open import Ledger.Prelude

```
-->

*Operational Certificate environments*

```agda
OCertEnv = ℙ KeyHashˢ
```

*Operational Certificate states*

```agda
OCertState = OCertCounters
```

*Operational Certificate transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,OCERT⦈_ : OCertEnv → OCertState → BHeader → OCertState → Type
```

*Operational Certificate helper function*

```agda
currentIssueNo : OCertEnv → OCertState → KeyHashˢ → Maybe ℕ
currentIssueNo stpools cs hk =
  if hk ∈ dom (cs ˢ) then
    just (lookupᵐ cs hk)
  else
  if hk ∈ stpools then
    just 0
  else
    nothing
```

<a id="fig:ts-types:ocert"></a>

The transition rule $\mathsf{OCERT}$ is shown below. From the block
header body `bhb` we first extract the following:

- The operational certificate `oc`, consisting of the hot key `vkₕ`,
    the certificate issue number `n`, the KES period start `c₀` and the cold key
  signature `τ`.
- The cold key `issuerVk`.
- The slot `slot` for the block.
- The number `t` of KES periods that have elapsed since the start period on the certificate.

Using this we verify the preconditions of the operational certificate state
transition which are the following:

- The KES period `kp` of the slot in the block header body must be greater than or equal to
  the start value `c₀` listed in the operational certificate,
  and less than `MaxKESEvo`-many KES periods after `c₀`.
  The value of `MaxKESEvo` is the agreed-upon lifetime of an operational certificate,
  see [Delegation and incentives design](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf).
- `currentIssueNo stpools cs hk` returns `just m`, where `m` is the stored certificate issue number, 
  or zero if `hk` has no stored counter but belongs to a registered pool. Also, `n` must equal either
  `m` or `m` + 1.
- The signature `τ` can be verified with the cold verification key `issuerVk`.
- The KES signature `σ` can be verified with the hot verification key `vkₕ`.

After this, the transition system updates the operational certificate issue-number
map, setting the entry for `hk` to `n` and inserting it if necessary.

<!--
```agda
private variable
  stpools : OCertEnv
  cs      : OCertState
  bh      : BHeader

data _⊢_⇀⦇_,OCERT⦈_ where
```
-->

```agda
  Update-OCert :
    let 〖 bhb , σ 〗 = bh; open BHBody bhb
        ⟦ vkₕ , n , c₀ , τ ⟧ᵒᶜ = oc
        hk = hash issuerVk
        kp = kesPeriod slot
        t = kp -ᵏ c₀
    in
    ∙ c₀ ≤ kp
    ∙ kp < c₀ +ᵏ MaxKESEvo
    ∙ ∃[ m ] (just m ≡ currentIssueNo stpools cs hk × (n ≡ m ⊎ n ≡ suc m))
    ∙ isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ
    ∙ isSignedᵏ vkₕ t (encode bhb) σ
    ────────────────────────────────
    stpools ⊢ cs ⇀⦇ bh ,OCERT⦈ (❴ hk , n ❵ ∪ˡ cs)
```

<a id="fig:ts-rules:ocert"></a>

The $\mathsf{OCERT}$ rule has the following predicate failures:

- If the KES period is less than the KES period start in the certificate,
  there is a *KESBeforeStart* failure.
- If the KES period is greater than or equal to the KES period end (start + `MaxKESEvo`)
  in the certificate, there is a *KESAfterEnd* failure.
- If the stored certificate issue number in the original key hash counter mapping is larger
  than the certificate issue number in the certificate, there is a *CounterTooSmall* failure.
- If the certificate issue number `n` is larger than the successor of the current
  certificate issue number `m` returned by `currentIssueNo` (which defaults to zero
  for a registered pool without a stored counter), there is a *CounterOverIncremented* failure.
- If the signature of the hot key, certificate issue number and period start is incorrect,
  there is an *InvalidSignature* failure.
- If the KES signature using the hot key of the block header body is incorrect,
  there is an *InvalidKesSignature* failure.
- If the cold key hash is absent from both the counter map and the registered pools,
  there is a *NoCounterForKeyHash* failure.

[Specification contents](../../SPECIFICATION.md)
