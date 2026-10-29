# TICKN Transition

<a id="sec:tick-nonce-trans"></a>

The Tick Nonce Transition ($\mathsf{TICKN}$) is responsible for updating the epoch nonce and the
previous epoch's hash nonce at the start of an epoch. Its environment consists of the candidate nonce `ηc`
and the previous epoch's last block header hash as a nonce `ηph`.
Its state consists of the epoch nonce `η₀` and the previous epoch's last block header hash nonce `ηh`.

<!--
```agda
{-# OPTIONS --safe #-}
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)
module Spec.TickNonce
  (crypto : _) (open Crypto crypto)
  (es     : _) (open EpochStructure es)
  (nonces : Nonces crypto) (open Nonces nonces)
  where
open import Ledger.Prelude
```
-->

<a id="fig:ts-types:ticknonce"></a>

*Tick Nonce environments*

```agda
record TickNonceEnv : Type where
  constructor ⟦_,_⟧ᵗᵉ
  field
    ηc  : Nonce     -- candidate nonce
    ηph : Nonce     -- previous header hash as nonce
```

*Tick Nonce states*

```agda
record TickNonceState : Type where
  constructor ⟦_,_⟧ᵗˢ
  field
    η₀ : Nonce -- epoch nonce
    ηh : Nonce -- nonce from hash of previous epoch's last block header
```

*Tick Nonce transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,TICKN⦈_ : TickNonceEnv → TickNonceState → Bool → TickNonceState → Type
```

The signal to the transition rule $\mathsf{TICKN}$ is a marker indicating
whether we are in a new epoch. If we are in a new epoch, we update the epoch
nonce and the previous hash. Otherwise, we do nothing:

<!--
```agda
private variable
  ηc ηph η₀ ηh : Nonce
data _⊢_⇀⦇_,TICKN⦈_ where 
```
-->

<a id="fig:ts-rules:ticknonce"></a>

```agda
  Not-New-Epoch :
    ────────────────────────────────
    ⟦ ηc , ηph ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ false ,TICKN⦈ ⟦ η₀ , ηh ⟧ᵗˢ
  New-Epoch :
    ────────────────────────────────
    ⟦ ηc , ηph ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ true ,TICKN⦈ ⟦ ηc ⋆ ηh , ηph ⟧ᵗˢ
```

[Specification contents](../../SPECIFICATION.md)
