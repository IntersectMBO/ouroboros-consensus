{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node.SanityCheck () where

import Data.List.NonEmpty (NonEmpty (..))
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Strict
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.History.EpochInfo
import Ouroboros.Consensus.Protocol.Abstract

instance CanHardFork xs => BlockSupportsSanityCheck (HardForkBlock xs) where
  configAllSecurityParams tlc =
    let configProtocol = topLevelConfigProtocol tlc
     in hardForkConsensusConfigK configProtocol
          :| perEraConsensusConfigSecurityParams (hardForkConsensusConfigPerEra configProtocol)

perEraConsensusConfigSecurityParams ::
  All SingleEraBlock xs =>
  PerEraConsensusConfig xs -> [SecurityParam]
perEraConsensusConfigSecurityParams (PerEraConsensusConfig xs) =
  unK $ hctraverse_ (Proxy @SingleEraBlock) go xs
 where
  go ::
    forall a.
    SingleEraBlock a =>
    WrapPartialConsensusConfig a -> K [SecurityParam] ()
  go (WrapPartialConsensusConfig c) =
    K [protocolSecurityParam (completeConsensusConfig (Proxy @(BlockProtocol a)) dummyEpochInfo c)]
