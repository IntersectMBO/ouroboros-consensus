{-# OPTIONS --allow-unsolved-metas #-} -- TODO: Remove when structs are fully implemented

open import Spec.Foreign.ExternalFunctions

module Spec.Foreign.HSConsensus.Core where

open import Ledger.Prelude hiding (ε) renaming (fromList to fromListˢ) public
open Computational public

open import Foreign.Convertible           public
open import Foreign.Convertible.Deriving  public
open import Foreign.HaskellTypes          public
open import Foreign.HaskellTypes.Deriving public

open import Ledger.Crypto
open import Ledger.Types.Epoch

module _ {A : Type} ⦃ _ : DecEq A ⦄ ⦃ _ : Show A ⦄ where instance
  ∀Hashable : Hashable A A
  ∀Hashable = λ where .hash → id

  ∀isHashableSet : isHashableSet A
  ∀isHashableSet = mkIsHashableSet A

instance
  Hashable-⊤ : Hashable ⊤ ℕ
  Hashable-⊤ = λ where .hash tt → 0

module Implementation where

  -- TODO: Complete

module ExternalStructures (externalFunctions : ExternalFunctions) where
  HSGlobalConstants = GlobalConstants ∋ record {Implementation}
  instance
    HSEpochStructure = EpochStructure ∋ ℕEpochStructure HSGlobalConstants

    -- TODO: Complete
    HSCrypto : Crypto
    HSCrypto = record {}

    open import Spec.BaseTypes HSCrypto

    -- TODO: Complete
    HSNonces : Nonces
    HSNonces = record
      { Nonce = ℕ
      ; _⋆_ = _+_
      }

    open EpochStructure HSEpochStructure public
    open Crypto HSCrypto public
    open Nonces HSNonces public
