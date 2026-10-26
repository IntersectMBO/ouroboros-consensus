# Cryptographic Primitives

<!--
```agda
{-# OPTIONS --safe #-}
module Ledger.Crypto where

open import Ledger.Prelude hiding (T)

module _ (M : Type↑) where
  _⁴ : ∀ {ℓ} {A B C D : Type ℓ} → (A → B → C → D → Type ℓ) → Type _
  _⁴ _~_~_~_ = ∀ {x y z w} → M (x ~ y ~ z ~ w)

_⁇⁴ = _⁇ ⁴

```
-->

## Serialization

-- TODO: Add paragraph

<!--
```agda
record Serializer : Type₁ where
  field
```
-->

*Types & functions*

```agda
        Ser             : Type
        encode          : {T : Type} → T → Ser
        decode          : {T : Type} → Ser → Maybe T
        _∥_             : Ser → Ser → Ser
```

*Properties*

<!--
```agda
        ⦃ DecEq-Ser  ⦄ : DecEq Ser
        ⦃ Show-Ser   ⦄ : Show Ser
        enc-dec-correct :
```
-->

```agda
          ∀ {T : Type} (x : T) → decode (encode x) ≡ just x
```

<a id="fig:defs:serialization"></a>

## Cryptographic Hashes

-- TODO: Add paragraph and show only the relevant bits of the code below.

```agda
record isHashableSet (T : Type) : Set₁ where
  constructor mkIsHashableSet
  field THash : Type
        ⦃ DecEq-THash ⦄ : DecEq      THash
        ⦃ Show-THash  ⦄ : Show       THash
        ⦃ DecEq-T     ⦄ : DecEq    T
        ⦃ T-Hashable  ⦄ : Hashable T THash
open isHashableSet

record HashableSet : Type₁ where
  constructor mkHashableSet
  field T : Type; ⦃ T-isHashable ⦄ : isHashableSet T
  open isHashableSet T-isHashable public
```

## Public-Key Cryptography

The Cardano blockchain system is based on a public-key cryptographic system.

<!--
```agda
record PKScheme : Type₁ where
  field
```
-->

*Types & functions*

<!--
The checked declarations retain Agda's record indentation. Keep the display below
in sync with this block.
```agda
   SKey VKey : Type
   isKeyPair : SKey → VKey → Type

  KeyPair = Σ[ sk ∈ SKey ] Σ[ vk ∈ VKey ] isKeyPair sk vk

  field
        ⦃ DecEq-SKey  ⦄ : DecEq SKey
        ⦃ DecEq-VKey  ⦄ : DecEq VKey
        ⦃ Show-SKey   ⦄ : Show SKey
        ⦃ Show-VKey   ⦄ : Show VKey
```
-->

<pre><code>SKey VKey : Type
isKeyPair : SKey → VKey → Type

KeyPair = Σ[ sk ∈ SKey ] Σ[ vk ∈ VKey ] isKeyPair sk vk
</code></pre>

<a id="fig:defs:pkscheme"></a>

## Digital Signatures

-- TODO: Add paragraph.

<!--
```agda
record DSigScheme (srl : Serializer) : Type₁ where
  open Serializer srl
  
  field
        pks : PKScheme

  open PKScheme pks public

  field
```
-->

*Types & functions*

```agda
        Sig      : Type
        isSigned : VKey → Ser → Sig → Type
        sign     : SKey → Ser → Sig
```

*Properties*

<!--
```agda
  field ⦃ Dec-isSigned ⦄ : isSigned ⁇³
        isSigned-correct :
```
-->

```agda
          ∀ ((sk , vk , _) : KeyPair) (d : Ser) (σ : Sig) → sign sk d ≡ σ → isSigned vk d σ
```

<!--
```agda
        ⦃ DecEq-Sig  ⦄ : DecEq Sig
        ⦃ Show-Sig   ⦄ : Show Sig
```
-->

<a id="fig:defs:dsigscheme"></a>

## Key-Evolving Signatures

-- TODO: Add paragraph.

<!--
```agda
record KESScheme (srl : Serializer) : Type₁ where
  open Serializer srl
  
  field
        pks : PKScheme

  open PKScheme pks public

  field
```
-->

*Types & functions*

```agda
        Sig      : Type
        isSigned : VKey → ℕ → Ser → Sig → Type
        sign     : (ℕ → SKey) → ℕ → Ser → Sig
```

*Properties*

<!--
```agda
  field ⦃ Dec-isSigned ⦄ : isSigned ⁇⁴
        isSigned-correct :
```
-->

```agda
          ∀ (n : ℕ) (sk : ℕ → SKey) ((skₙ , vk , _) : KeyPair) (d : Ser) (σ : Sig)
            → skₙ ≡ sk n → sign sk n d ≡ σ → isSigned vk n d σ
```

<!--
```agda
        ⦃ DecEq-Sig  ⦄ : DecEq Sig
        ⦃ Show-Sig   ⦄ : Show Sig
```
-->

<a id="fig:defs:kesscheme"></a>

## Verifiable Random Functions

-- TODO: Add paragraph.

<!--
```agda
record VRFScheme : Type₁ where
  field
        pks : PKScheme

  open PKScheme pks public

  field
```
-->

*Types & functions*

```agda
        Seed Proof : Type
        verify     : {T : Type} → VKey → Seed → Proof × T → Type
        evaluate   : {T : Type} → SKey → Seed → Proof × T
        _XOR_      : Seed → Seed → Seed
```

*Properties*

<!--
```agda
  field ⦃ Dec-verify ⦄ : {T : Type} → verify {T} ⁇³
        verify-correct :
```
-->

```agda
          ∀ {T : Type} ((sk , vk , _) : KeyPair) (seed : Seed)
            → verify {T = T} vk seed (evaluate sk seed)
```

<!--
```agda
        ⦃ DecEq-Seed  ⦄ : DecEq Seed
        ⦃ DecEq-Proof ⦄ : DecEq Proof
        ⦃ Show-Seed   ⦄ : Show Seed
        ⦃ Show-Proof  ⦄ : Show Proof
```
-->

<a id="fig:defs:vrfscheme"></a>

<!--
```agda
record Crypto : Type₁ where
  field srl  : Serializer
        dsig : DSigScheme srl
        kes  : KESScheme srl
        vrf  : VRFScheme

  open Serializer srl public
  open DSigScheme dsig renaming (VKey to VKeyˢ; Sig to Sigˢ; isSigned to isSignedˢ) public
  open KESScheme kes renaming (KeyPair to KeyPairᵏ; VKey to VKeyᵏ; Sig to Sigᵏ; isSigned to isSignedᵏ) public
  open VRFScheme vrf renaming (KeyPair to KeyPairᵛ; VKey to VKeyᵛ) public

  field ⦃ khsˢ ⦄    : isHashableSet VKeyˢ
        ⦃ khsᵛ ⦄    : isHashableSet VKeyᵛ
        ⦃ shs  ⦄    : isHashableSet Ser

  open isHashableSet khsˢ renaming (THash to KeyHashˢ) hiding (DecEq-T) public
  open isHashableSet khsᵛ renaming (THash to KeyHashᵛ) hiding (DecEq-T) public
  open isHashableSet shs  renaming (THash to SerHash)  hiding (DecEq-T) public
```
-->

[Specification contents](../../SPECIFICATION.md)
