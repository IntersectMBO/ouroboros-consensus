open import Spec.Foreign.ExternalFunctions

module Spec.Foreign.HSConsensus.Core where

open import Ledger.Prelude hiding (ε) renaming (fromList to fromListˢ) public
open Computational public

open import Foreign.Convertible           public
open import Foreign.Convertible.Deriving  public
open import Foreign.HaskellTypes          public
open import Foreign.HaskellTypes.Deriving public

open import Ledger.Crypto

open import Spec.Foreign.Util public

open import Data.Rational using (1ℚ; ½; Positive; positive; _<_) renaming (_≤_ to _≤ℚ_)
open import Data.Rational.Ext using (PosUnitInterval)
open import Data.Integer using (_≤_; _<_)

½-positive : Positive ½
½-positive = positive {½} (_<_.*<* (_<_.+<+ (s≤s z≤n)))

½≤1 : ½ ≤ℚ 1ℚ
½≤1 = _≤ℚ_.*≤* (_≤_.+≤+ (s≤s z≤n))

instance
  Hashable-⊤ : Hashable ⊤ ℕ
  Hashable-⊤ = λ where .hash tt → 0

  Hashable-ℕ : Hashable ℕ ℕ
  Hashable-ℕ = λ where .hash → suc

  isHashableSet-ℕ : isHashableSet ℕ
  isHashableSet-ℕ = mkIsHashableSet ℕ

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
