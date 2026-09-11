{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.Praos
  ( -- * BlockForging
    praosBlockForging
  , praosSharedBlockForging
  , praosWithLeiosSharedBlockForging
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Data.Text as T
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos
  ( BasePraos
  , Praos
  , PraosParams (..)
  , PraosWithLeios
  , praosCheckCanForge
  )
import Ouroboros.Consensus.Protocol.Praos.Common (StrictMaybeLeios (..))
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoHasLeios)
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record for a single era.
praosBlockForging ::
  forall m era c.
  ( ShelleyCompatible (Praos c) era
  , IOLike m
  ) =>
  PraosParams ->
  HotKey.HotKey c m ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Praos c) era)
praosBlockForging praosParams hotKey credentials =
  praosSharedBlockForging hotKey slotToPeriod credentials
 where
  PraosParams{praosSlotsPerKESPeriod} = praosParams

  slotToPeriod :: SlotNo -> Absolute.KESPeriod
  slotToPeriod (SlotNo slot) =
    SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
-- | Shared by every Praos extension; the caller supplies the Leios token,
-- since it knows which extension it is.
basePraosSharedBlockForging ::
  forall m pext c era.
  ( ShelleyCompatible (BasePraos pext c) era
  , IOLike m
  ) =>
  StrictMaybeLeios (ProtoHasLeios (BasePraos pext c)) () ->
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (BasePraos pext c) era)
basePraosSharedBlockForging
  leiosToken
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
            (configConsensus cfg)
            curSlot
      , forgeBlock = \cfg ->
          forgeShelleyBlock
            hotKey
            canBeLeader
            leiosToken
            cfg
      , finalize = HotKey.finalize hotKey
      }

praosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Praos c) era
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Praos c) era)
praosSharedBlockForging = basePraosSharedBlockForging SNothingLeios

praosWithLeiosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (PraosWithLeios c) era
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (PraosWithLeios c) era)
praosWithLeiosSharedBlockForging = basePraosSharedBlockForging (SJustLeios ())
