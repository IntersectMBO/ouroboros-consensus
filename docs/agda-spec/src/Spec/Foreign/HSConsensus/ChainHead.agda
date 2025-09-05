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
  open import Spec.Foreign.HSConsensus.ExternalStructures ext hiding (BHeader; getPoolDelegatedStake)
  open import Spec.Foreign.HSConsensus.BlockDefinitions
  open import Spec.ChainHead.Properties HSCrypto HSNonces HSEpochStructure HSBlockStructure HSAbstractFunctions HSLedgerInterface HSRationalExtStructure

  chainhead-step : HsType (ChainHeadEnv → ChainHeadState → BHeader → ComputationResult String ChainHeadState)
  -- FIXME: Investigate why `A` needs to be manually supplied.
  chainhead-step = to {ChainHeadEnv → ChainHeadState → BHeader → ComputationResult String ChainHeadState} (coerce ⦃ TrustMe ⦄ $ compute Computational-CHAINHEAD)

  {-# COMPILE GHC chainhead-step as chainheadStep #-}

  open import Data.String.Base renaming (_++_ to _+ˢ_) hiding (show; length; _≤_)
  open import Spec.Foreign.HSTypes using (Show-HSMap)

  chainhead-debug : HsType (ChainHeadEnv → ChainHeadState → BHeader → String)
  chainhead-debug che chs bh =
    let
       pds = getPoolDelegatedStake 126
       pd  = extractPoolDistr pds
    in
      unlines
        $ ("\npd  \t\t\t: " +ˢ Show-HSMap .show (to pd))
        ∷ ("\npds \t\t\t: " +ˢ Show-HSMap .show (to pds))
        ∷ []

  {-# COMPILE GHC chainhead-debug as chainheadDebug #-}
