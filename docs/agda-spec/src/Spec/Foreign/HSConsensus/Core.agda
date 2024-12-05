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

open import Spec.Foreign.Util public

open import Data.Rational using (1ℚ; ½; Positive; positive; _<_) renaming (_≤_ to _≤ℚ_)
open import Data.Rational.Ext using (PosUnitInterval)
open import Data.Integer using (_≤_; _<_)
open import Data.String.Base renaming (_++_ to _+ˢ_) hiding (show; length)

½-positive : Positive ½
½-positive = positive {½} (_<_.*<* (_<_.+<+ (s≤s z≤n)))

½≤1 : ½ ≤ℚ 1ℚ
½≤1 = _≤ℚ_.*≤* (_≤_.+≤+ (s≤s z≤n))

module _ {A : Type} ⦃ _ : DecEq A ⦄ ⦃ _ : Show A ⦄ where instance
  ∀Hashable : Hashable A A
  ∀Hashable = λ where .hash → id

  ∀isHashableSet : isHashableSet A
  ∀isHashableSet = mkIsHashableSet A

instance
  Hashable-⊤ : Hashable ⊤ ℕ
  Hashable-⊤ = λ where .hash tt → 0

module Implementation where
  -- Global constants
  Network                        = ℕ
  SlotsPerEpochᶜ                 = 100
  StabilityWindowᶜ               = 10
  RandomnessStabilisationWindowᶜ = 20
  Quorum                         = 1
  NetworkId                      = 0
  SlotsPerKESPeriodᶜ             = 5
  MaxKESEvoᶜ                     = 30
  ActiveSlotCoeffᶜ : PosUnitInterval
  ActiveSlotCoeffᶜ               = ½ , ½-positive , ½≤1
  MaxMajorPVᶜ                    = 1

  -- VRFs and nonces
  Seed = ℕ

module ExternalStructures (externalFunctions : ExternalFunctions) where
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
