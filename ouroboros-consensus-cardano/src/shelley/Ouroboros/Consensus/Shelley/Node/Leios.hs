{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.Leios
  ( -- * BlockForging
    leiosBlockForging
  , leiosSharedBlockForging
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Data.Text as T
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Leios
  ( ConsensusConfig (LeiosConfig, leiosEpochInfo, leiosPraosParams)
  , Leios
  , LeiosCrypto
  )
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (PraosConfig)
  , PraosParams (..)
  , praosCheckCanForge
  )
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Shelley.Protocol.Leios ()
import Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record for a single era.
leiosBlockForging ::
  forall m era c.
  ( ShelleyCompatible (Leios c) era
  , LeiosCrypto c
  , IOLike m
  ) =>
  ConsensusConfig (Leios c) ->
  HotKey.HotKey c m ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Leios c) era)
leiosBlockForging leiosConfig hotKey credentials =
  leiosSharedBlockForging hotKey slotToPeriod credentials
 where
  PraosParams{praosSlotsPerKESPeriod} = leiosPraosParams leiosConfig

  slotToPeriod :: SlotNo -> Absolute.KESPeriod
  slotToPeriod (SlotNo slot) =
    SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

-- | Create a 'BlockForging' record safely using the given 'HotKey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
leiosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Leios c) era
  , LeiosCrypto c
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Leios c) era)
leiosSharedBlockForging
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    } =
    BlockForging
      { forgeLabel = label <> "_" <> T.pack (L.eraName @era)
      , canBeLeader = canBeLeader
      , updateForgeState = \_ curSlot _ ->
          forgeStateUpdateInfoFromUpdateInfo
            <$> HotKey.evolve hotKey (slotToPeriod curSlot)
      , checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
          praosCheckCanForge
            (toPraosConsensusConfig $ configConsensus cfg)
            curSlot
      , forgeBlock = \cfg ->
          forgeShelleyBlock
            hotKey
            canBeLeader
            cfg
      , finalize = HotKey.finalize hotKey
      }

-- | Convert a 'ConsensusConfig (Leios c)' to a 'ConsensusConfig (Praos c)'
-- for use with 'praosCheckCanForge'.
toPraosConsensusConfig ::
  ConsensusConfig (Leios c) ->
  ConsensusConfig (Praos.Praos c)
toPraosConsensusConfig LeiosConfig{leiosPraosParams, leiosEpochInfo} =
  PraosConfig
    { Praos.praosParams = leiosPraosParams
    , Praos.praosEpochInfo = leiosEpochInfo
    }
