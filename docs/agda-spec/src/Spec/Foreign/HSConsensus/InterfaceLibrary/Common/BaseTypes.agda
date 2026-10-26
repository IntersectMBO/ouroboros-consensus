module Spec.Foreign.HSConsensus.InterfaceLibrary.Common.BaseTypes where

open import Spec.Foreign.HSTypes
open import Spec.Foreign.HSConsensus.BaseTypes
open import InterfaceLibrary.Common.BaseTypes DummyCrypto using (PoolDistr)

unquoteDecl = do
  hsTypeAlias PoolDistr
