{-# OPTIONS --safe #-}

open import InterfaceLibrary.Ledger
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Data.Rational.Ext

module Spec.ChainHead.Properties
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (li     : LedgerInterface crypto es ss) (let open LedgerInterface li)
  (rs     : _) (open RationalExtStructure rs)
  where

open import Tactic.GenError
open import Ledger.Prelude
open import Ledger.PParams crypto es ss using (PParams; ProtVer)
open import Spec.TickForecast crypto es ss li
open import Spec.TickForecast.Properties crypto es ss li
open import Spec.TickNonce crypto es nonces
open import Spec.TickNonce.Properties crypto es nonces
open import Spec.Protocol crypto nonces es ss bs af rs
open import Spec.Protocol.Properties crypto nonces es ss bs af rs
open import Spec.ChainHead crypto nonces es ss bs af li rs

instance

  prtlSeqChecks⁇ : prtlSeqChecks ⁇²
  prtlSeqChecks⁇ {nothing}                    {_}  .dec = yes tt
  prtlSeqChecks⁇ {lab@(just ⟦ bℓ , sℓ , _ ⟧ℓ)} {bh} .dec =
    sℓ     <? slot       ×-dec
    bℓ + 1 ≟  blockNo    ×-dec
    ph     ≟  prevHeader
    where
      open BHBody (proj₁ bh)
      ph = lastAppliedHash lab

chainChecks? : ∀ maxpv ps bh → Dec (chainChecks maxpv ps bh)
chainChecks? maxpv (maxBHSize , maxBBSize , protocolVersion) bh =
  m             ≤? maxpv     ×-dec
  headerSize bh ≤? maxBHSize ×-dec
  bodySize      ≤? maxBBSize
  where
    m = proj₁ protocolVersion
    open BHBody (proj₁ bh)

instance

  _ = Monad-ComputationResult

  Computational-CHAINHEAD : Computational _⊢_⇀⦇_,CHAINHEAD⦈_ String
  Computational-CHAINHEAD = record {Go} where
    open Computational ⦃...⦄ renaming (computeProof to comp; completeness to complete)
    computeTICKF = comp {STS = _⊢_⇀⦇_,TICKF⦈_}
    computeTICKN = comp {STS = _⊢_⇀⦇_,TICKN⦈_}
    computePRTCL = comp {STS = _⊢_⇀⦇_,PRTCL⦈_}
    module Go
      (nes : NewEpochState)
      (s   : ChainHeadState) (let ⟦ cs , η₀ , ηv , ηc , ηh , lab ⟧ᶜˢ = s)
      (bh  : BHeader)        (let (bhb , σ) = bh; open BHBody bhb)
      where

      e₁      = getEpoch nes
      nₚₕ     = prevHashToNonce (lastAppliedHash lab)
      lab′    = just ⟦ blockNo , slot , headerHash bh ⟧ℓ
      ticknΓ  = ⟦ ηc , nₚₕ ⟧ᵗᵉ
      ticknSt = ⟦ η₀ , ηh ⟧ᵗˢ
      prtclSt = ⟦ cs , ηv , ηc ⟧ᵖˢ

      computeProof : ComputationResult String (∃[ s′ ] nes ⊢ s ⇀⦇ bh ,CHAINHEAD⦈ s′)
      computeProof = case ¿ prtlSeqChecks ¿² lab bh of λ where
        (no ¬psc) → failure (genErrors ¬psc)
        (yes psc) → do
          (forecast , tickfStep) ← computeTICKF _ nes slot
          let
            e₂ = getEpoch forecast
            ne = (e₁ ≠ e₂)
            pp = getPParams forecast; open PParams
            pd = getPoolDistr forecast
          case chainChecks? MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh of λ where
            (no ¬cc) → failure (genErrors ¬cc)
            (yes cc) → do
              (⟦ η₀′ , _ ⟧ᵗˢ , ticknStep) ← computeTICKN ticknΓ ticknSt ne
              (_             , prtclStep) ← computePRTCL ⟦ pd , η₀′ ⟧ᵖᵉ prtclSt bh
              success (-, Chain-Head (psc , tickfStep , cc , ticknStep , prtclStep))

      completeness : ∀ s′ → nes ⊢ s ⇀⦇ bh ,CHAINHEAD⦈ s′ → (proj₁ <$> computeProof) ≡ success s′
      completeness ⟦ cs′ , η₀′ , ηv′ , ηc′ , ηh′ , lab′ ⟧ᶜˢ (Chain-Head (psc , tickfStep , cc , ticknStep , prtclStep))
        with ¿ prtlSeqChecks ¿² lab bh
      ... | no ¬psc = contradiction psc ¬psc
      ... | yes _
        with computeTICKF _ nes slot | complete _ nes _ _ tickfStep
      ... | success (forecast , _) | refl
        with
          (let pp = getPParams forecast; open PParams
           in chainChecks? MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh)
      ... | no ¬cc = contradiction cc ¬cc
      ... | yes _
        with
          (let e₂ = getEpoch forecast; ne = (e₁ ≠ e₂)
           in computeTICKN ticknΓ ticknSt ne) | complete ticknΓ ticknSt _ _ ticknStep
      ... | success (⟦ η₀′ , _ ⟧ᵗˢ , _) | refl
        with computePRTCL ⟦ getPoolDistr forecast , η₀′ ⟧ᵖᵉ prtclSt bh | complete ⟦ getPoolDistr forecast , η₀′ ⟧ᵖᵉ prtclSt _ _ prtclStep
      ... | success _ | refl = refl
