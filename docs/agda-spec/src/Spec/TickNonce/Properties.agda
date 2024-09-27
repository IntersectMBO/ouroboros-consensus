{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)

module Spec.TickNonce.Properties
  (crypto : _) (open Crypto crypto)
  (es     : _) (open EpochStructure es)
  (nonces : Nonces crypto) (open Nonces nonces)
  where

open import Ledger.Prelude
open Computational ⦃...⦄
open import Spec.TickNonce crypto es nonces

instance
  Computational-TICKN : Computational _⊢_⇀⦇_,TICKN⦈_ String
  Computational-TICKN .computeProof _ _ false                 = success (-, Not-New-Epoch)
  Computational-TICKN .computeProof _ _ true                  = success (-, New-Epoch)
  Computational-TICKN .completeness _ _ false _ Not-New-Epoch = refl
  Computational-TICKN .completeness _ _ true  _ New-Epoch     = refl
