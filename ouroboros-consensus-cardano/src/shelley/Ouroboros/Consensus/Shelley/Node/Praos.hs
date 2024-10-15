{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Ouroboros.Consensus.Shelley.Node.Praos (
    -- * BlockForging
    praosBlockForging
  , praosSharedBlockForging
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Data.Text as T
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (configConsensus)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosParams (..),
                     praosCheckCanForge)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible, forgeShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto,
                     ShelleyLeaderCredentials (..))
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
  -> HotKey.HotKey c m
  -> ShelleyLeaderCredentials (EraCrypto era)
  -> BlockForging m (ShelleyBlock (Praos c) era)
praosBlockForging praosParams hotKey credentials =
    praosSharedBlockForging hotKey slotToPeriod credentials
  where
    PraosParams {praosSlotsPerKESPeriod} = praosParams

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
      SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

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
  -> (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c
  -> BlockForging m (ShelleyBlock (Praos c) era)
praosSharedBlockForging
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials {
      shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    } =
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
            canBeLeader
            cfg,
        finalize = HotKey.finalize hotKey
      }
