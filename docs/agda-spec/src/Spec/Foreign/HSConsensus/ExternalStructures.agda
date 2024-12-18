open import Spec.Foreign.ExternalFunctions

module Spec.Foreign.HSConsensus.ExternalStructures (externalFunctions : ExternalFunctions) where

open import Data.String.Base renaming (_++_ to _+ˢ_) hiding (show; length)
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Spec.Foreign.HSConsensus.Core

HSGlobalConstants = GlobalConstants ∋ record {Implementation}
instance
  HSEpochStructure = EpochStructure ∋ ℕEpochStructure HSGlobalConstants

  -- NOTE: Dummy for now.
  HSSerializer : Serializer
  HSSerializer = record
    { Ser             = String
    ; encode          = const ""
    ; decode          = const nothing
    ; _∥_             = _+ˢ_
    ; enc-dec-correct = error "enc-dec-correct evaluated"
    }

  -- NOTE: Dummy for now.
  HSPKScheme : PKScheme
  HSPKScheme = record
    { SKey      = ℕ
    ; VKey      = ℕ
    ; isKeyPair = _≡_
    }

  -- Note: Dummy for now.
  HSDSigScheme : DSigScheme HSSerializer
  HSDSigScheme = record
    { pks              = HSPKScheme
    ; isSigned         = λ _ _ _ → ⊤
    ; sign             = λ _ _ → 0
    ; isSigned-correct = error "isSigned-correct evaluated"
    ; Dec-isSigned     = λ {_} {_} {_} → ⁇ (true because ofʸ tt)
    }

  -- NOTE: Dummy for now.
  HSKESScheme : KESScheme HSSerializer
  HSKESScheme = record
    { pks              = HSPKScheme
    ; Sig              = ℕ
    ; isSigned         = λ _ _ _ _ → ⊤
    ; sign             = λ _ _ _ → 0
    ; isSigned-correct = error "isSigned-correct evaluated"
    ; Dec-isSigned     = λ {_} {_} {_} {_} → ⁇ (true because ofʸ tt)
    }

  -- NOTE: Dummy for now.
  HSVRFScheme : VRFScheme
  HSVRFScheme = record
    { Implementation
    ; pks            = HSPKScheme
    ; Proof          = ℕ
    ; verify         = λ _ _ _ → ⊤
    ; evaluate       = error "evaluate evaluated"
    ; _XOR_          = _+_
    ; verify-correct = error "verify-correct evaluated"
    ; Dec-verify     = λ {T = _} {_} {_} {_} → ⁇ (true because ofʸ tt)
    }

  HSCrypto : Crypto
  HSCrypto = record
    { srl        = HSSerializer
    ; dsig       = HSDSigScheme
    ; kes        = HSKESScheme
    ; vrf        = HSVRFScheme
    ; ScriptHash = ℕ
    }

  open import Spec.BaseTypes HSCrypto

  -- NOTE: Dummy for now.
  HSNonces : Nonces
  HSNonces = record
    { Implementation
    ; Nonce       = ℕ
    ; _⋆_         = _+_
    ; nonceToSeed = id
    }

  open EpochStructure HSEpochStructure public
  open Crypto HSCrypto public
  open Nonces HSNonces public
