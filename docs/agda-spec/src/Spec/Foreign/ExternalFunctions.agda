module Spec.Foreign.ExternalFunctions where -- TODO: Complete

open import Ledger.Prelude
open import Foreign.HaskellTypes.Deriving

record ExternalFunctions : Set where
  field extDummy : ℕ → Bool
{-# FOREIGN GHC
  data ExternalFunctions = MkExternalFunctions { extDummy :: Integer -> Bool }
#-}
{-# COMPILE GHC ExternalFunctions = data ExternalFunctions (MkExternalFunctions) #-}

dummyExternalFunctions : ExternalFunctions
dummyExternalFunctions = record { extDummy = λ x → true }
{-# COMPILE GHC dummyExternalFunctions as dummyExternalFunctions #-}
