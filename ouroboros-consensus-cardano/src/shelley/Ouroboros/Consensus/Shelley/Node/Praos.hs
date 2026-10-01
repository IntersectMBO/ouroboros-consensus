{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader)
import Ouroboros.Consensus.Protocol.Praos
  ( Praos
  , PraosCannotForge
  , PraosParams (..)
  , PraosWithLeios
  , praosCheckCanForge
  )
import Ouroboros.Consensus.Shelley.Ledger
  ( LeiosForge (..)
  , ShelleyBlock
  , ShelleyCompatible
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader)
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( CannotForgeError
  , ProtoCrypto
  , ProtocolHeaderSupportsKES (configSlotsPerKESPeriod)
  )
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
-- | Shared by both Praos protocols; the caller supplies the 'LeiosForge',
-- since it knows which protocol it is.
basePraosSharedBlockForging ::
  forall m proto c era.
  ( ShelleyCompatible proto era
  , ProtoCrypto proto ~ c
  , CanBeLeader proto ~ PraosCanBeLeader c
  , CannotForgeError proto ~ PraosCannotForge c
  , IOLike m
  ) =>
  LeiosForge proto ->
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock proto era)
basePraosSharedBlockForging
  leiosForge
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
            (configSlotsPerKESPeriod (configConsensus cfg))
            curSlot
      , forgeBlock = \cfg ->
          forgeShelleyBlock
            hotKey
            canBeLeader
            leiosForge
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
praosSharedBlockForging = basePraosSharedBlockForging (NoLeiosForge ())

praosWithLeiosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (PraosWithLeios c) era
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (PraosWithLeios c) era)
praosWithLeiosSharedBlockForging = basePraosSharedBlockForging (LeiosForge (,))
