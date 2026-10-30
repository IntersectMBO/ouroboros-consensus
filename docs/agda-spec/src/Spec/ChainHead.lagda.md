# CHAINHEAD Transition

<a id="sec:chainhead-trans"></a>

The Chain Head Transition rule ($\mathsf{CHAINHEAD}$) is the main rule of the blockchain layer
part of the STS. It calls $\mathsf{TICKF}$, $\mathsf{TICKN}$, and $\mathsf{PRTCL}$,
as sub-rules.

Its environment is the epoch-specific ledger state `NewEpochState`, and its
signal is a block header. Its
[state](ChainHead.lagda.md#fig:ts-types:chainhead) consists of:

- The operational certificate issue number map `cs`.
- The epoch nonce `η₀`.
- The evolving nonce `ηv`.
- The candidate nonce `ηc`.
- The previous epoch hash nonce `ηh`.
- The last header hash `h`.
- The last slot `sℓ`.
- The last block number `bℓ`.
- The endorser block announced by the last block `aeb`.

<!--
```agda
{-# OPTIONS --safe #-}
open import InterfaceLibrary.Ledger
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Data.Rational.Ext
module Spec.ChainHead
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (bs     : BlockStructure crypto nonces es) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (li     : LedgerInterface crypto es) (let open LedgerInterface li)
  (rs     : _) (open RationalExtStructure rs)  
  where
open import Spec.BaseTypes crypto using (OCertCounters; PoolDistr)
open import Spec.TickForecast crypto es li
open import Spec.TickNonce crypto es nonces
open import Spec.Protocol crypto nonces es bs af rs
open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDelegatedStake)
open import Ledger.PParams using (PParams; ProtVer)
open import Ledger.Prelude
open import Data.Rational using (ℚ; normalize)
open import Data.Product renaming (map₁ to ×-map₁)
```
-->

<a id="fig:ts-types:chainhead"></a>

*Chain Head environments*

```agda
ChainHeadEnv = NewEpochState
```

*Chain Head states*

```agda
record LastAppliedBlock : Type where
  constructor ⟦_,_,_,_⟧ℓ
  field
    bℓ  : BlockNo           -- last block number
    sℓ  : Slot              -- last slot
    h   : HashHeader        -- latest header hash
    aeb : Maybe AnnouncedEB -- endorser block announced by the last block

record ChainHeadState : Type where
  constructor ⟦_,_,_,_,_,_⟧ᶜˢ
  field
    cs  : OCertCounters          -- operational certificate issue numbers
    η₀  : Nonce                  -- epoch nonce
    ηv  : Nonce                  -- evolving nonce
    ηc  : Nonce                  -- candidate nonce
    ηh  : Nonce                  -- nonce from hash of last epoch’s last header
    lab : Maybe LastAppliedBlock -- latest applied block
```

*Chain Head transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,CHAINHEAD⦈_ : ChainHeadEnv → ChainHeadState → BHeader → ChainHeadState → Type
```

<a id="fig:ts-funs:chainhead"></a>

*Chain Head helper functions*

```agda
extractPoolDistr : PoolDelegatedStake → PoolDistr
extractPoolDistr pds = mapValues (×-map₁ stakeToProportion) pds
  where
    totalStake : Coin
    totalStake = ∑[ c ← mapValues proj₁ pds ] c

    stakeToProportion : Coin → ℚ
    stakeToProportion c = case totalStake of λ where
      0         → normalize 0 1
      t@(suc n) → normalize c t

chainChecks : ℕ → ℕ × ℕ × ProtVer → BHeader → Type
chainChecks maxpv (maxBHSize , maxBBSize , protocolVersion) bh =
  m ≤ maxpv × headerSize bh ≤ maxBHSize × bodySize ≤ maxBBSize
  where
    m = proj₁ protocolVersion
    open BHeader; open BHBody (bh .body)

lastAppliedHash : Maybe LastAppliedBlock → Maybe HashHeader
lastAppliedHash nothing                   = nothing
lastAppliedHash (just ⟦ _ , _ , h , _ ⟧ℓ) = just h

prtlSeqChecks : Maybe LastAppliedBlock → BHeader → Type
prtlSeqChecks nothing                         bh = ⊤
prtlSeqChecks lab@(just ⟦ bℓ , sℓ , _ , _ ⟧ℓ) bh = sℓ < slot × bℓ + 1 ≡ blockNo × ph ≡ prevHeader
  where
    open BHeader; open BHBody (bh .body)
    ph = lastAppliedHash lab

certificationDelay : PParams → Slot
certificationDelay pp = slotsFromDuration
    (3 * leiosHeaderPeriod + leiosVotingPeriod + leiosDiffusionPeriod)
  where open PParams pp

certChecks : PParams → Maybe LastAppliedBlock → Bool → Slot → Type
certChecks _  _                                false _ = ⊤
certChecks _  nothing                          true  _ = ⊥
certChecks _  (just ⟦ _ , _  , _ , nothing ⟧ℓ) true  _ = ⊥
certChecks pp (just ⟦ _ , sℓ , _ , just _  ⟧ℓ) true  s =
  sℓ + certificationDelay pp ≤ s
```

The transition checks the following things
(via the functions `chainChecks`, `prtlSeqChecks` and `certChecks`
from "Chain Head helper functions"):

- The slot in the block header body is larger than the last slot recorded.
- The block number increases by exactly one.
- The previous hash listed in the block header matches the previous
  block header hash which was recorded.
- The size of the block header is less than or equal to the maximal size that the
  protocol parameters allow for block headers.
- The size of the block body, as claimed by the block header, is less than or equal to the
  maximal size that the protocol parameters allow for block bodies.
- The node is not obsolete, meaning that the major component of the
  protocol version in the protocol parameters is not bigger than the constant `MaxMajorPV`.
- If the block header claims to certify an endorser block, then the last applied
  block announced one, and enough slots have elapsed since it did.

The last of these checks deserves comment, since it is the only one concerned with
endorser blocks. A block header carries a flag `certifiesEB` recording whether the
block certifies the endorser block announced by its predecessor; the hash of that
endorser block is not repeated in the header, as it is already available from the
predecessor's `announcedEB` field. Consequently the last applied block must
retain what it announced, which is why `LastAppliedBlock` carries an
`aeb` field.

A certificate may only be included once $3 L_\text{hdr} + L_\text{vote} +
L_\text{diff}$ has elapsed since the block that announced the endorser
block (see [CIP-0164](https://cips.cardano.org/cip/CIP-164)), so that the endorser block has had time to reach the whole
network. Here, $L_\text{hdr}$, $L_\text{vote}$, and $L_\text{diff}$ denote
`leiosHeaderPeriod`, `leiosVotingPeriod`, and `leiosDiffusionPeriod`, respectively,
as defined in [Protocol parameters](../Ledger/PParams.lagda.md#fig:protocol-parameter-declarations).
These three protocol parameters are wall-clock durations, so their sum is
converted to a number of slots by `slotsFromDuration`, using the genesis slot length
and rounding up. A header that sets `certifiesEB` requires the corresponding body
to carry a matching certificate. The ledger rules check this requirement, while
`CHAINHEAD` checks that the required certification delay has elapsed.

Note that the `size` component of `announcedEB` is deliberately *not*
checked here. As specified in [CIP-0164](https://cips.cardano.org/cip/CIP-164), an incorrect announced size invalidates
neither the header nor the block; it merely costs the block producer the votes of
honest nodes, which validate the endorser block itself.

<!--
```agda
private variable
  nes forecast                : NewEpochState
  cs cs′                      : OCertCounters
  η₀ ηv ηc ηh η₀′ ηv′ ηc′ ηh′ : Nonce
  lab                         : Maybe LastAppliedBlock
  bh                          : BHeader
data _⊢_⇀⦇_,CHAINHEAD⦈_ where
```
-->

<a id="fig:ts-rules:chainhead"></a>

```agda
  Chain-Head :
    let 〖 bhb , _ 〗 = bh; open BHBody bhb
        e₁   = getEpoch nes
        e₂   = getEpoch forecast
        ne   = (e₁ ≠ e₂)
        pp   = getPParams forecast; open PParams
        nₚₕ  = prevHashToNonce (lastAppliedHash lab)
        pd   = extractPoolDistr (getPoolDelegatedStake forecast)
        lab′ = just ⟦ blockNo , slot , headerHash bh , announcedEB ⟧ℓ
    in
    ∙ prtlSeqChecks lab bh
    ∙ _ ⊢ nes ⇀⦇ slot ,TICKF⦈ forecast
    ∙ chainChecks MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh
    ∙ certChecks pp lab certifiesEB slot
    ∙ ⟦ ηc , nₚₕ ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ ne ,TICKN⦈ ⟦ η₀′ , ηh′ ⟧ᵗˢ
    ∙ ⟦ pd , η₀′ ⟧ᵖᵉ ⊢ ⟦ cs , ηv , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , ηv′ , ηc′ ⟧ᵖˢ
    ────────────────────────────────
    nes ⊢ ⟦ cs  , η₀  , ηv  , ηc  , ηh  , lab  ⟧ᶜˢ ⇀⦇ bh ,CHAINHEAD⦈
          ⟦ cs′ , η₀′ , ηv′ , ηc′ , ηh′ , lab′ ⟧ᶜˢ
```

The transition rule $\mathsf{CHAINHEAD}$ is shown above and
has the following predicate failures:

- If the slot of the block header body is not larger than the last slot,
  there is a *WrongSlotInterval* failure.
- If the block number does not increase by exactly one, there is a *WrongBlockNo* failure.
- If the hash of the previous header of the block header body is not equal
  to the last header hash, there is a *WrongBlockSequence* failure.
- If the size of the block header is larger than the maximally allowed size,
  there is a *HeaderSizeTooLarge* failure.
- If the size of the block body is larger than the maximally allowed size,
  there is a *BlockSizeTooLarge* failure.
- If the major component of the protocol version is larger than `MaxMajorPV`,
  there is an *ObsoleteNode* failure.
- If the block header certifies an endorser block but there is no last applied
  block, or the last applied block announced no endorser block, there is a
  *NoEndorserBlockToCertify* failure.
- If the block header certifies an endorser block but less than
  $3 L_\text{hdr} + L_\text{vote} + L_\text{diff}$ has elapsed since the last
  applied block, there is a *CertifiedTooEarly* failure.

[Specification contents](../../SPECIFICATION.md)
