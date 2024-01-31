{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Node.SanityCheck
  () where

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.Block.SupportsSanityCheck
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Protocol.Abstract
import           Data.List.NonEmpty (NonEmpty(..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Data.SOP.Constraint
import           Data.SOP.BasicFunctors
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Data.SOP.Strict
import           Ouroboros.Consensus.HardFork.History.EpochInfo

instance CanHardFork xs => BlockSupportsSanityCheck (HardForkBlock xs) where
  checkSecurityParamConsistency tlc = do
    let configProtocol = topLevelConfigProtocol tlc
    hardForkConsensusConfigK configProtocol :|
      perEraConsensusConfigSecurityParams (hardForkConsensusConfigPerEra configProtocol)

perEraConsensusConfigSecurityParams :: All SingleEraBlock xs
                                    => PerEraConsensusConfig xs -> [SecurityParam]
perEraConsensusConfigSecurityParams (PerEraConsensusConfig xs) =
  unK $ hctraverse_ (Proxy @SingleEraBlock) go xs
    where
      go :: forall a . SingleEraBlock a
         => WrapPartialConsensusConfig a -> K [SecurityParam] ()
      go (WrapPartialConsensusConfig c) =
        K [ protocolSecurityParam (completeConsensusConfig (Proxy @(BlockProtocol a)) dummyEpochInfo c) ]
