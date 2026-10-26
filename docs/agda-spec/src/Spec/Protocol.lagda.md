# PRTCL Transition

<a id="sec:protocol-trans"></a>

The Protocol Transition ($\mathsf{PRTCL}$) calls the transition $\mathsf{UPDN}$ to update the evolving
and candidate nonces, and checks the operational certificate with $\mathsf{OCERT}$. Its environment consists of:

- The stake pool stake distribution `pd`.
- The epoch nonce `η₀`.

Its state consists of

- The operational certificate issue number mapping `cs`.
- The evolving nonce `ηv`.
- The candidate nonce for the next epoch `ηc`.

<!--
```agda
{-# OPTIONS --safe #-}
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Data.Rational.Ext
module Spec.Protocol
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (bs     : BlockStructure crypto nonces es) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (rs     : _) (open RationalExtStructure rs)
  where
open import Spec.OperationalCertificate crypto nonces es bs af
open import Spec.UpdateNonce crypto nonces es
open import Spec.BaseTypes crypto using (OCertCounters; PoolDistr)
open import Data.Rational as ℚ using (ℚ; 0ℚ; 1ℚ)
open import Ledger.Prelude
open Ledger.Prelude.ℤ using (pos)
```
-->

*Protocol environments*

```agda
record PrtclEnv : Type where
  constructor ⟦_,_⟧ᵖᵉ
  field
    pd  : PoolDistr -- pool stake distribution
    η₀  : Nonce     -- epoch nonce
```

*Protocol states*

```agda
record PrtclState : Type where
  constructor ⟦_,_,_⟧ᵖˢ
  field
    cs : OCertCounters -- operational certificate issue numbers
    ηv : Nonce         -- evolving nonce
    ηc : Nonce         -- candidate nonce
```

*Protocol transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,PRTCL⦈_ : PrtclEnv → PrtclState → BHeader → PrtclState → Type
```

<a id="fig:ts-types:prtcl"></a>

*Protocol helper functions*

<!--
Keep the display below in sync with these checked declarations. Hidden proof code
requires separate indentation but should not interrupt the displayed functions.
```agda
hBLeader : BHBody → Certifiedℕ
hBLeader bhb = serHashToℕ (hash (encode "L" ∥ encode vrfRes))
  where open BHBody bhb

hBNonce : BHBody → Nonce
hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode vrfRes))
  where open BHBody bhb

lookupPoolDistr : PoolDistr → KeyHashˢ → Maybe (ℚ × KeyHashᵛ)
lookupPoolDistr pd hk =
  if hk ∈ dom (pd ˢ) then
    just (lookupᵐ pd hk)
  else
    nothing

checkLeaderVal : Certifiedℕ → InPosUnitInterval → ℚ → Type
checkLeaderVal (certℕ , certℕprf) (f , posf , f≤1) σ =
   if f ≡ 1ℚ then ⊤ else
     λ{f≢1ℚ} →
       let
         p = pos certℕ ℚ./ (2 ^ 512)
         q = 1ℚ ℚ.- p
         p≢1ℚ  = ↥p<↧p⇒p≢1 {p} (n<m⇒↥[n/m]<↧[n/m] certℕprf)
         1-f≥0 = p≤1⇒1-p≥0 f≤1
         1-f≢0 = p≢1⇒1-p≢0 f≢1ℚ
         instance
           q≢0ℚ  = ℚ.≢-nonZero (p≢1⇒1-p≢0 p≢1ℚ)
           1-f>0 = ℚ.positive (≤∧≢⇒< 1-f≥0 $ ≢-sym 1-f≢0)
         c = ln (1ℚ ℚ.- f)
       in
         ℚ.1/ q ℚ.< exp ((ℚ.- σ) ℚ.* c)

vrfChecks : Nonce → PoolDistr → InPosUnitInterval → BHBody → Type
vrfChecks η₀ pd f bhb =
  case lookupPoolDistr pd hk of
    λ where
      nothing            → ⊥
      (just (σ , vrfHK)) →
           vrfHK ≡ hash vrfVk
         × verify vrfVk seed (vrfPrf , vrfRes)
         × checkLeaderVal (hBLeader bhb) f σ
  where
    open BHBody bhb
    hk = hash issuerVk
    seed = slotToSeed slot XOR nonceToSeed η₀
```
-->

<pre><code>hBLeader : BHBody → Certifiedℕ
hBLeader bhb = serHashToℕ (hash (encode "L" ∥ encode vrfRes))
  where open BHBody bhb

hBNonce : BHBody → Nonce
hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode vrfRes))
  where open BHBody bhb

lookupPoolDistr : PoolDistr → KeyHashˢ → Maybe (ℚ × KeyHashᵛ)
lookupPoolDistr pd hk =
  if hk ∈ dom (pd ˢ) then
    just (lookupᵐ pd hk)
  else
    nothing

checkLeaderVal : Certifiedℕ → InPosUnitInterval → ℚ → Type
checkLeaderVal (certℕ , certℕprf) (f , posf , f≤1) σ =
   if f ≡ 1ℚ then ⊤ else
       let
         p = pos certℕ ℚ./ (2 ^ 512)
         q = 1ℚ ℚ.- p
         c = ln (1ℚ ℚ.- f)
       in
         ℚ.1/ q ℚ.&lt; exp ((ℚ.- σ) ℚ.* c)

vrfChecks : Nonce → PoolDistr → InPosUnitInterval → BHBody → Type
vrfChecks η₀ pd f bhb =
  case lookupPoolDistr pd hk of
    λ where
      nothing            → ⊥
      (just (σ , vrfHK)) →
           vrfHK ≡ hash vrfVk
         × verify vrfVk seed (vrfPrf , vrfRes)
         × checkLeaderVal (hBLeader bhb) f σ
  where
    open BHBody bhb
    hk = hash issuerVk
    seed = slotToSeed slot XOR nonceToSeed η₀
</code></pre>

<a id="fig:ts-funs:prtcl"></a>

In "Protocol helper functions" we define a function `vrfChecks` which performs all the VRF related checks
on a given block header body.
In addition to the block header body `bhb`, the function requires the epoch nonce `η₀`,
the stake distribution `pd` (aggregated by pool), and the active slots coefficient `f` from the protocol
parameters. The function checks:

- The validity of the proof `vrfPrf` for the leader value and the new nonce.
- The verification key hash `vrfHK` is associated with relative stake `σ` in the stake distribution.
- The `hBLeader` value of `bhb` indicates a possible leader for
  this slot. The function `checkLeaderVal` performs this check.

The function `vrfChecks` has the following predicate failures:

- If the issuer's key hash is not in the pool distribution, there is a *VRFKeyUnknown* failure.
- If the stored VRF verification-key hash `vrfHK` does not match `hash vrfVk`,
  where `vrfVk` is the VRF verification key in the block header, there is a *VRFKeyWrongVRFKey* failure.
- If the VRF generated value in the block header does not validate
  against the VRF certificate, there is a *VRFKeyBadProof* failure.
- If the VRF generated leader value in the block header is too large
  compared to the relative stake of the pool, there is a *VRFLeaderValueTooBig* failure.

The transition rule $\mathsf{PRTCL}$ is shown below.

<!--
```agda
private variable
  pd               : PoolDistr
  cs cs′           : OCertCounters
  ηv ηc ηv′ ηc′ η₀ : Nonce
  bh               : BHeader
data _⊢_⇀⦇_,PRTCL⦈_ where
```
-->

```agda
  Evolve-Prtcl :
    let 〖 bhb , σ 〗 = bh; open BHBody bhb
        η = hBNonce bhb
    in
    ∙ ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ slot ,UPDN⦈ ⟦ ηv′ , ηc′ ⟧ᵘˢ
    ∙ dom (pd ˢ) ⊢ cs ⇀⦇ bh ,OCERT⦈ cs′
    ∙ vrfChecks η₀ pd ActiveSlotCoeff bhb
    ────────────────────────────────
    ⟦ pd , η₀ ⟧ᵖᵉ ⊢ ⟦ cs , ηv , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , ηv′ , ηc′ ⟧ᵖˢ
```

<a id="fig:ts-rules:prtcl"></a>

[Specification contents](../../SPECIFICATION.md)
