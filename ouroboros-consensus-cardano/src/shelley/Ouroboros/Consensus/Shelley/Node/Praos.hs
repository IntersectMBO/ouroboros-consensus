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
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

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
  -> ShelleyLeaderCredentials (EraCrypto era) m
  -> m (BlockForging m (ShelleyBlock (Praos c) era))
praosBlockForging praosParams maxTxCapacityOverrides credentials = do
    ShelleyKeyBundle
          { shelleyKeyBundleSignKeyKES = skSound
          , shelleyKeyBundleOpCert = ocert
          } <- shelleyLeaderCredentialsGetSignKeyBundle credentials

    let startPeriod :: Absolute.KESPeriod
        startPeriod = SL.ocertKESPeriod ocert

        slotToPeriod :: SlotNo -> Absolute.KESPeriod
        slotToPeriod (SlotNo slot) =
          SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

    hotKey <- HotKey.mkHotKey @m @c skSound startPeriod praosMaxKESEvo
    pure $ praosSharedBlockForging hotKey ocert slotToPeriod credentials maxTxCapacityOverrides
  where
    PraosParams {praosMaxKESEvo, praosSlotsPerKESPeriod} = praosParams

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
praosSharedBlockForging ::
     forall m c era.
     ( ShelleyEraWithCrypto c (Praos c) era
     , IOLike m
     )
  => HotKey.HotKey c m
  -> SL.OCert c
  -> (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c m
  -> Mempool.TxOverrides (ShelleyBlock (Praos c) era)
  -> BlockForging m     (ShelleyBlock (Praos c) era)
praosSharedBlockForging
  hotKey
  ocert
  slotToPeriod
  ShelleyLeaderCredentials {
      shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    }
  maxTxCapacityOverrides = do
    BlockForging
      { forgeLabel = label <> "_" <> T.pack (L.eraName @era),
        canBeLeader = canBeLeader,
        updateForgeState = \_ curSlot _ ->
          forgeStateUpdateInfoFromUpdateInfo
            <$> HotKey.evolve hotKey (slotToPeriod curSlot),
        checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
          praosCheckCanForge
            (configConsensus cfg)
            curSlot,
        forgeBlock = \cfg ->
          forgeShelleyBlock
            hotKey
            ocert
            canBeLeader
            cfg
            maxTxCapacityOverrides
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
