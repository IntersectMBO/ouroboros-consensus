{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Types.Epoch
open import InterfaceLibrary.Ledger

module Spec.TickForecast.Properties
  (crypto : Crypto)
  (es     : _) (open EpochStructure es)
  (li     : LedgerInterface crypto es) (let open LedgerInterface li)
  where

open import Ledger.Prelude
open import Spec.TickForecast crypto es li

instance

  _ = Monad-ComputationResult

  Computational-TICKF : Computational _⊢_⇀⦇_,TICKF⦈_ String
  Computational-TICKF = record {Go} where
    open Computational ⦃...⦄ renaming (computeProof to comp; completeness to complete)
    computeNEWEPOCH  = comp {STS = _⊢_⇀⦇_,NEWEPOCH⦈_}
    module Go
      (Γ   : ⊤)
      (nes : NewEpochState)
      (s   : Slot)
      where

      computeProof : ComputationResult String (∃[ nes′ ] Γ ⊢ nes ⇀⦇ s ,TICKF⦈ nes′)
      computeProof = do
        (_ , newEpochStep) ← computeNEWEPOCH Γ nes (epoch s)
        success (-, Tick-Forecast newEpochStep)

      completeness : ∀ nes′ → Γ ⊢ nes ⇀⦇ s ,TICKF⦈ nes′ → (proj₁ <$> computeProof) ≡ success nes′
      completeness forecast (Tick-Forecast newEpochStep)
        with computeNEWEPOCH Γ nes (epoch s) | complete Γ nes _ _ newEpochStep
      ... | success _ | refl = refl
