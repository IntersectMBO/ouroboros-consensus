module Spec.Foreign.ExternalFunctions where -- TODO: Complete

open import Ledger.Prelude
open import Foreign.HaskellTypes.Deriving

record ExternalFunctions : Set where
  field
    extIsSignedˢ : ℕ → ℕ → ℕ → Bool
    extIsSignedᵏ : ℕ → ℕ → ℕ → ℕ → Bool
{-# FOREIGN GHC
  data ExternalFunctions = MkExternalFunctions
    { extIsSignedDSIG :: Integer -> Integer -> Integer -> Bool
    , extIsSignedKES  :: Integer -> Integer -> Integer -> Integer -> Bool
    }
#-}
{-# COMPILE GHC ExternalFunctions = data ExternalFunctions (MkExternalFunctions) #-}

dummyExternalFunctions : ExternalFunctions
dummyExternalFunctions = record
  { extIsSignedˢ = λ _ _ _ → true
  ; extIsSignedᵏ = λ _ _ _ _ → true
  }
{-# COMPILE GHC dummyExternalFunctions as dummyExternalFunctions #-}
