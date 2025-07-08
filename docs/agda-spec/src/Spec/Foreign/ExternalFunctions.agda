module Spec.Foreign.ExternalFunctions where -- TODO: Complete

open import Ledger.Prelude
import Spec.Foreign.HSTypes as F using (Rational)
open import Spec.Foreign.HSConsensus.Core

record ExternalFunctions : Type where
  field
    extSignˢ     : ℕ → ℕ → ℕ
    extIsSignedˢ : ℕ → ℕ → ℕ → Bool
    extIsSignedᵏ : ℕ → ℕ → ℕ → ℕ → Bool
    extExp       : F.Rational → F.Rational
    extLn        : F.Rational → F.Rational
{-# FOREIGN GHC
  data ExternalFunctions = MkExternalFunctions
    { extSignDSIG     :: Integer -> Integer -> Integer
    , extIsSignedDSIG :: Integer -> Integer -> Integer -> Bool
    , extIsSignedKES  :: Integer -> Integer -> Integer -> Integer -> Bool
    , extExp          :: Rational -> Rational
    , extLn           :: Rational -> Rational
    }
#-}
{-# COMPILE GHC ExternalFunctions = data ExternalFunctions (MkExternalFunctions) #-}

dummyExternalFunctions : ExternalFunctions
dummyExternalFunctions = record
  { extSignˢ     = λ _ _ → 0
  ; extIsSignedˢ = λ _ _ _ → true
  ; extIsSignedᵏ = λ _ _ _ _ → true
  ; extExp       = λ p → to (rationalExtStructure≈ .exp (from p))
  ; extLn        = λ p → to (rationalExtStructure≈ .ln (from p) ⦃ if (from p) ℚ.> 0ℚ then (λ{p>0} → positive p>0) else error "Not a positive rational number" ⦄)
  }
  where
    open import Data.Rational.Ext using (RationalExtStructure; rationalExtStructure≈)
    open RationalExtStructure
    open import Data.Rational as ℚ
{-# COMPILE GHC dummyExternalFunctions as dummyExternalFunctions #-}
