module Spec.Foreign.HSConsensus.ChainHead where

open import Spec.Foreign.ExternalFunctions

open import Foreign.Haskell.Coerce

open import Spec.Foreign.HSConsensus.BaseTypes
open import Spec.ChainHead DummyCrypto DummyNonces DummyEpochStructure DummyBlockStructure DummyAbstractFunctions DummyLedgerInterface DummyRationalExtStructure

unquoteDecl = do
  hsTypeAlias ChainHeadEnv

instance
  HsTy-LastAppliedBlock = autoHsType LastAppliedBlock ⊣ withConstructor "MkLastAppliedBlock"
                                                      • fieldPrefix "lab"
  Conv-LastAppliedBlock = autoConvert LastAppliedBlock

  HsTy-ChainHeadState = autoHsType ChainHeadState ⊣ withConstructor "MkChainHeadState"
                                                  • fieldPrefix "chs"
  Conv-ChainHeadState = autoConvert ChainHeadState

module _ (ext : ExternalFunctions) where
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (BHeader)
  open import Spec.Foreign.HSConsensus.BlockDefinitions
  open import Spec.ChainHead.Properties HSCrypto HSNonces HSEpochStructure HSBlockStructure HSAbstractFunctions HSLedgerInterface HSRationalExtStructure

  chainhead-step : HsType (ChainHeadEnv → ChainHeadState → BHeader → ComputationResult String ChainHeadState)
  -- FIXME: Investigate why `A` needs to be manually supplied.
  chainhead-step = to {ChainHeadEnv → ChainHeadState → BHeader → ComputationResult String ChainHeadState} (coerce ⦃ TrustMe ⦄ $ compute Computational-CHAINHEAD)

  {-# COMPILE GHC chainhead-step as chainheadStep #-}
