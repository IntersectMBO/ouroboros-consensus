{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Ouroboros.Consensus.Shelley.Node.Praos (
    -- * BlockForging
    praosBlockForging
  , praosSharedBlockForging
    -- * ProtocolInfo
  , ProtocolParams (..)
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Data.Text as T
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (configConsensus)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosParams (..),
                     praosCheckCanForge)
import           Ouroboros.Consensus.Shelley.Eras (BabbageEra, ConwayEra,
                     EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible, forgeShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto,
                     ShelleyKeyBundle (..), ShelleyLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Util.IOLike (IOLike, newEmptyMVar, withMVar)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance BlockForgingCredentials (ShelleyBlock (Praos c) era) = ShelleyKeyBundle c

-- | Create a 'BlockForging' record for a single era.
praosBlockForging ::
     forall m era c.
     ( ShelleyCompatible (Praos c) era
     , c ~ EraCrypto era
     , Mempool.TxLimits (ShelleyBlock (Praos c) era)
     , IOLike m
     )
  => PraosParams
  -> Mempool.TxOverrides (ShelleyBlock (Praos c) era)
  -> ShelleyLeaderCredentials (EraCrypto era)
  -> m (BlockForging m (ShelleyBlock (Praos c) era))
praosBlockForging praosParams maxTxCapacityOverrides credentials = do
    let slotToPeriod :: SlotNo -> Absolute.KESPeriod
        slotToPeriod (SlotNo slot) =
          SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

    praosSharedBlockForging slotToPeriod credentials maxTxCapacityOverrides
  where
    PraosParams {praosSlotsPerKESPeriod} = praosParams

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
praosSharedBlockForging ::
     forall m c era.
     ( ShelleyEraWithCrypto c (Praos c) era
     , IOLike m
     )
  => (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c
  -> Mempool.TxOverrides (ShelleyBlock (Praos c) era)
  -> m (BlockForging m (ShelleyBlock (Praos c) era))
praosSharedBlockForging
  slotToPeriod
  ShelleyLeaderCredentials {
      shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    }
  maxTxCapacityOverrides = do
    keyBundleVar <- newEmptyMVar

    pure BlockForging
      { forgeLabel = label <> "_" <> T.pack (L.eraName @era),
        canBeLeader = canBeLeader,
        updateForgeState = \_ curSlot _ ->
          withMVar keyBundleVar $ \(hotKey, ocert) -> do
            forgeStateUpdateInfoFromUpdateInfo
              <$> HotKey.evolve hotKey (slotToPeriod curSlot),
        checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
          praosCheckCanForge
            (configConsensus cfg)
            curSlot,
        forgeBlock = \cfg curNo curSlot tickedLedger txs isLeader -> do
          withMVar keyBundleVar $ \(hotKey, ocert) -> do
            let startPeriod :: Absolute.KESPeriod
                startPeriod = SL.ocertKESPeriod ocert

            forgeShelleyBlock
              hotKey
              ocert
              canBeLeader
              cfg
              maxTxCapacityOverrides
              curNo
              curSlot
              tickedLedger
              txs
              isLeader
      }

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

data instance ProtocolParams (ShelleyBlock (Praos c) (BabbageEra c)) = ProtocolParamsBabbage {
    babbageProtVer                :: SL.ProtVer
    -- ^ see 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer', mutatis mutandi
  , babbageMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (Praos c) (BabbageEra c))
  }

data instance ProtocolParams (ShelleyBlock (Praos c) (ConwayEra c)) = ProtocolParamsConway {
    conwayProtVer                :: SL.ProtVer
    -- ^ see 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer', mutatis mutandi
  , conwayMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (Praos c) (ConwayEra c))
  }
