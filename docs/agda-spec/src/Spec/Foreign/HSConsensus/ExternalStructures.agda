open import Spec.Foreign.ExternalFunctions

module Spec.Foreign.HSConsensus.ExternalStructures (externalFunctions : ExternalFunctions) where

open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Spec.Foreign.HSConsensus.Core

HSGlobalConstants = GlobalConstants ∋ record {Implementation}
instance
  HSEpochStructure = EpochStructure ∋ ℕEpochStructure HSGlobalConstants

  -- NOTE: Dummy for now.
  HSSerializer : Serializer
  HSSerializer = record
    { Ser             = ℕ
    ; encode          = const 0
    ; decode          = const nothing
    ; _∥_             = _+_
    ; enc-dec-correct = error "enc-dec-correct evaluated"
    }

  -- NOTE: Dummy for now.
  HSPKScheme : PKScheme
  HSPKScheme = record
    { SKey      = ℕ
    ; VKey      = ℕ
    ; isKeyPair = _≡_
    }

  HSDSigScheme : DSigScheme HSSerializer
  HSDSigScheme = record
    { pks              = HSPKScheme
    ; Sig              = ℕ
    ; isSigned         = λ vk d σ → extIsSignedˢ vk d σ ≡ true
    ; sign             = λ _ _ → 0 -- NOTE: Dummy for now
    ; isSigned-correct = error "isSigned-correct evaluated"
    ; Dec-isSigned     = ⁇ (_ ≟ _)
    }
    where
      open ExternalFunctions externalFunctions

  HSKESScheme : KESScheme HSSerializer
  HSKESScheme = record
    { pks              = HSPKScheme
    ; Sig              = ℕ
    ; isSigned         = λ vk n d σ → extIsSignedᵏ vk n d σ ≡ true
    ; sign             = λ _ _ _ → 0 -- NOTE: Dummy for now
    ; isSigned-correct = error "isSigned-correct evaluated"
    ; Dec-isSigned     = ⁇ (_ ≟ _)
    }
    where
      open ExternalFunctions externalFunctions

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
    { srl  = HSSerializer
    ; dsig = HSDSigScheme
    ; kes  = HSKESScheme
    ; vrf  = HSVRFScheme
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

open import Spec.BlockDefinitions it it it

instance
  -- NOTE: Dummy for now
  HSBlockStructure : BlockStructure
  HSBlockStructure = record
    { HashHeader       = ℕ
    ; HashBBody        = ℕ
    ; VRFRes           = ℕ
    ; DecEq-HashHeader = DecEq-ℕ
    }

open BlockStructure HSBlockStructure public

instance
  -- NOTE: Dummy for now
  HSAbstractFunctions : AbstractFunctions
  HSAbstractFunctions = record
    { headerHash      = const 2
    ; headerSize      = const 1
    ; slotToSeed      = id
    ; prevHashToNonce = const 0
    ; serHashToℕ      = const (0 , s≤s z≤n)
    ; serHashToNonce  = id
    }

open import Data.Rational.Ext using (RationalExtStructure)

instance
  HSRationalExtStructure : RationalExtStructure
  HSRationalExtStructure = record
    { exp = λ p → from (extExp (to p))
    ; ln  = λ p ⦃ p>0 ⦄ → from (extLn (to p))
    }
    where
      open ExternalFunctions externalFunctions

-- TODO: Move to its own file Spec.Foreign.HSConsensus.InterfaceLibrary.Ledger

open import InterfaceLibrary.Ledger HSCrypto HSEpochStructure
open import Ledger.PParams using (PParams)

-- NOTE: Dummy for now
DummyNEWEPOCH : ⊤ → ℕ → ℕ → ℕ → Type
DummyNEWEPOCH _ n _ n′ = n′ ≡ suc n

instance
  Computational-DummyNEWEPOCH : Computational DummyNEWEPOCH String
  Computational-DummyNEWEPOCH .computeProof _ n _ = success (suc n , refl)
  Computational-DummyNEWEPOCH .completeness _ n _ n′ eq
    with n′ ≟ suc n
  ... | yes p rewrite p = refl
  ... | no ¬p = contradiction eq ¬p

  HSLedgerInterface : LedgerInterface
  HSLedgerInterface = record
    { NewEpochState      = ℕ
    ; getPParams         = const $ record { maxHeaderSize = 1; maxBlockSize = 2; pv = 1 , 0 }
    ; getEpoch           = const 1
    ; getPoolDistr       = const ❴ 457 , (ℤ.+ 1 ℚ./ 3 , 568) ❵
    ; adoptGenesisDelegs = _+_
    ; _⊢_⇀⦇_,NEWEPOCH⦈_  = DummyNEWEPOCH
    }
    where
      import Data.Integer.Base as ℤ using (+_)
      import Data.Rational.Base as ℚ using (_/_)

open EpochStructure HSEpochStructure public
open Crypto HSCrypto public
open Nonces HSNonces public
open AbstractFunctions HSAbstractFunctions public
open RationalExtStructure HSRationalExtStructure public
open LedgerInterface HSLedgerInterface public
