module Spec.Foreign.HSConsensus.TickForecast where

open import Spec.Foreign.ExternalFunctions

open import Foreign.Haskell.Coerce

open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.TickForecast DummyCrypto DummyEpochStructure DummyLedgerInterface

-- TODO: Move to its own file Spec.Foreign.HSConsensus.InterfaceLibrary.Ledger
unquoteDecl = do
  hsTypeAlias NewEpochState

module _ (ext : ExternalFunctions) where
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (NewEpochState; Epoch)
  open import Spec.TickForecast.Properties HSCrypto HSEpochStructure HSLedgerInterface

  tickf-step : HsType (⊤ → NewEpochState → Epoch → ComputationResult String NewEpochState)
  tickf-step = to (coerce ⦃ TrustMe ⦄ $ compute Computational-TICKF)

  {-# COMPILE GHC tickf-step as tickfStep #-}
