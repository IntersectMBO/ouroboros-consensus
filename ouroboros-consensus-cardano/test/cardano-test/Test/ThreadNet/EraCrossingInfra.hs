{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.EraCrossingInfra (TestSetup (..), DualBlock) where

import Cardano.Ledger.BaseTypes (nonZero)
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Slotting.Slot (EpochSize (..))
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
  ( isHardForkNodeToNodeEnabled
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Consensus.Shelley.MockCrypto (MockCrypto)
import Test.QuickCheck
import Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.ThreadNet.Infra.ShelleyBasedHardFork
import Test.ThreadNet.Infra.TwoEras
import Test.ThreadNet.TxGen.Allegra ()
import Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import Test.Util.Orphans.Arbitrary ()

-- | A hard-fork block for two Shelley-based eras
type DualBlock era1 era2 =
  ShelleyBasedHardForkBlock (TPraos MockCrypto) era1 (TPraos MockCrypto) era2

-- | The varying data of the tests crossing between Shelley-based eras
--
-- Note: The Shelley nodes in this test all join, propose an update, and endorse
-- it literally as soon as possible. Therefore, if the test reaches the end of
-- the first epoch, the proposal will be adopted.
data TestSetup era1 era2 = TestSetup
  { setupD :: Shelley.DecentralizationParam
  , setupHardFork :: Bool
  -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce :: SL.Nonce
  -- ^ the initial Shelley 'SL.ticknStateEpochNonce'
  --
  -- We vary it to ensure we explore different leader schedules.
  , setupK :: SecurityParam
  , setupPartition :: Partition
  , setupSlotLength :: SlotLength
  , setupTestConfig :: TestConfig
  , setupVersion :: (NodeToNodeVersion, BlockNodeToNodeVersion (DualBlock era1 era2))
  }

deriving instance Show (TestSetup era1 era2)

instance
  SupportedNetworkProtocolVersion (DualBlock era1 era2) =>
  Arbitrary (TestSetup era1 era2)
  where
  arbitrary = do
    setupD <-
      arbitrary
        -- The decentralization parameter cannot be 0 in the first
        -- Shelley epoch, since stake pools can only be created and
        -- delegated to via Shelley transactions.
        `suchThat` ((/= 0) . Shelley.decentralizationParamToRational)
    setupK <- SecurityParam <$> choose (8, 10) `suchThatMap` nonZero
    -- If k < 8, common prefix violations become too likely in
    -- Praos mode for thin overlay schedules (ie low d), even for
    -- f=0.2.

    setupInitialNonce <- genNonce

    setupSlotLength <- arbitrary

    let epochSize = EpochSize $ shelleyEpochSize setupK
    setupTestConfig <-
      genTestConfig
        setupK
        (epochSize, epochSize)
    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupHardFork <- frequency [(49, pure True), (1, pure False)]

    -- TODO How reliable is the Byron-based partition duration logic when
    -- reused for Shelley?
    setupPartition <- genPartition numCoreNodes numSlots setupK

    setupVersion <-
      genVersionFiltered
        isHardForkNodeToNodeEnabled
        (Proxy @(DualBlock era1 era2))

    pure
      TestSetup
        { setupD
        , setupHardFork
        , setupInitialNonce
        , setupK
        , setupPartition
        , setupSlotLength
        , setupTestConfig
        , setupVersion
        }
