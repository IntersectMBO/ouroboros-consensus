# Block Definitions

<a id="sec:defs-blocks"></a>

<!--
```agda
{-# OPTIONS --safe #-}

open import Spec.BaseTypes using (Nonces)
open import Ledger.Prelude
open import Ledger.Crypto
open import Ledger.Types.Epoch

module Spec.BlockDefinitions
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)  
  (es     : _) (open EpochStructure es)
  where

open import Ledger.PParams using (ProtVer)

record BlockStructure : Type₁ where
  field
```
-->

*Abstract types*

```agda
    HashHeader : Type -- hash of a block header
    HashBBody  : Type -- hash of a block body
    VRFRes     : Type -- VRF result value
    HashEB     : Type -- hash of endorser block
```

<!--
```agda
    ⦃ DecEq-HashHeader ⦄ : DecEq HashHeader
    ⦃ DecEq-HashBBody  ⦄ : DecEq HashBBody
    ⦃ DecEq-VRFRes     ⦄ : DecEq VRFRes
    ⦃ DecEq-HashEB     ⦄ : DecEq HashEB
    ⦃ Show-HashHeader  ⦄ : Show HashHeader
    ⦃ Show-HashBBody   ⦄ : Show HashBBody
    ⦃ Show-VRFRes      ⦄ : Show VRFRes
    ⦃ Show-HashEB      ⦄ : Show HashEB
```
-->

*Concrete types*

```agda
  BlockNo = ℕ -- block number
  Certifiedℕ = ∃[ n ] n < 2 ^ 512 -- [0, 2^512) (64-byte VRF output)
```

*Announced Endorser Block*

```agda
  record AnnouncedEB : Type where
    field
      hash : HashEB
      size : ℕ
```

*Operational Certificate*

  ```agda
  record OCert : Type where
    constructor ⟦_,_,_,_⟧ᵒᶜ
    field
      vkₕ : VKeyᵏ     -- operational (hot) key
      n   : ℕ         -- certificate issue number
      c₀  : KESPeriod -- start KES period
      σ   : Sigˢ      -- cold key signature
  ```

<a id="fig:defs:blocks"></a>

*Block Header Body*

```agda
  record BHBody : Type where
    field
      prevHeader  : Maybe HashHeader  -- hash of previous block header
      issuerVk    : VKeyˢ             -- block issuer
      vrfVk       : VKeyᵛ             -- VRF verification key
      blockNo     : BlockNo           -- block number
      slot        : Slot              -- block slot
      vrfRes      : VRFRes            -- VRF result value
      vrfPrf      : Proof             -- VRF proof
      bodySize    : ℕ                 -- size of the block body
      bodyHash    : HashBBody         -- block body hash
      oc          : OCert             -- operational certificate
      pv          : ProtVer           -- protocol version
      announcedEB : Maybe AnnouncedEB -- announced endorser block
      certifiesEB : Bool              -- claims to certify the preceding block's EB
```

*Block Types*

```agda
  record BHeader : Type where
    constructor 〖_,_〗
    field
      body : BHBody
      sig  : Sigᵏ
```

*Abstract functions*

<!--
```agda
  record AbstractFunctions : Type where
    field
```
-->

```agda

      headerHash      : BHeader → HashHeader -- hash of a block header
      headerSize      : BHeader → ℕ -- size of a block header
      slotToSeed      : Slot → Seed -- big-endian encoding of the slot number in 8 bytes
      prevHashToNonce : Maybe HashHeader → Nonce
      serHashToℕ      : SerHash → Certifiedℕ
      serHashToNonce  : SerHash → Nonce
```

<a id="fig:defs:block-header"></a>

<!--
```agda
open BlockStructure
open import Tactic.Derive.Show

instance
  _ = Show-Maybe

  Show-Certifiedℕ : ∀ {bs} → Show (Certifiedℕ bs)
  Show-Certifiedℕ .show = show ∘ proj₁

  unquoteDecl Show-AnnouncedEB = derive-Show
    [(quote AnnouncedEB , Show-AnnouncedEB)]

  unquoteDecl Show-OCert = derive-Show
    [(quote OCert , Show-OCert)]

  unquoteDecl Show-BHBody = derive-Show
    [(quote BHBody , Show-BHBody)]

  unquoteDecl Show-BHeader = derive-Show
    [(quote BHeader , Show-BHeader)]
```
-->

[Specification contents](../../SPECIFICATION.md)
