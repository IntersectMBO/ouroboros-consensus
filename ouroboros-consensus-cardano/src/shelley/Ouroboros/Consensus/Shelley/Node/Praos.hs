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
  , praos2SharedBlockForging
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import Control.Exception (assert)
import qualified Data.Text as T
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus, configLedger)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (encodeLeiosEbSize)
import Ouroboros.Consensus.Protocol.Abstract (CanBeLeader, ConsensusConfig)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos
  ( Praos
  , PraosCannotForge
  , PraosParams (..)
  , praosCheckCanForge
  )
import Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader, WhenLeios)
import Ouroboros.Consensus.Protocol.Praos.Views (PolyPraosLedgerView (..))
import Ouroboros.Consensus.Protocol.Praos2 (ConsensusConfig (..), LeiosOnly (..), Praos2)
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyLeaderCredentials (..)
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract (CannotForgeError, ProtoCrypto)
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
praosSharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Praos c) era
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Praos c) era)
praosSharedBlockForging = basePraosSharedBlockForging praosParams

-- | 'praosSharedBlockForging' for every Praos.
basePraosSharedBlockForging ::
  forall m proto c era.
  ( ShelleyCompatible proto era
  , ProtoCrypto proto ~ c
  , CanBeLeader proto ~ PraosCanBeLeader c
  , CannotForgeError proto ~ PraosCannotForge c
  , Applicative (WhenLeios proto)
  , Foldable (WhenLeios proto)
  , IOLike m
  ) =>
  -- | The Praos parameters within this protocol's configuration
  (ConsensusConfig proto -> PraosParams) ->
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock proto era)
basePraosSharedBlockForging
  getPraosParams
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
            (getPraosParams (configConsensus cfg))
            curSlot
      , forgeBlock = forgeShelleyBlock hotKey canBeLeader
      , finalize = HotKey.finalize hotKey
      }

-- | As 'praosSharedBlockForging', for 'Praos2'.
praos2SharedBlockForging ::
  forall m c era.
  ( ShelleyCompatible (Praos2 c) era
  , LedgerSupportsProtocol (ShelleyBlock (Praos2 c) era)
  , IOLike m
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Praos2 c) era)
praos2SharedBlockForging hotKey slotToPeriod credentials =
  blockForging{forgeBlock = \args -> checkEbSize args <$> forgeBlock blockForging args}
 where
  blockForging :: BlockForging m (ShelleyBlock (Praos2 c) era)
  blockForging =
    basePraosSharedBlockForging (praosParams . leiosPraosConfig) hotKey slotToPeriod credentials

  checkEbSize ForgeBlockArgs{fbConfig, fbCurrentTickedLedgerState} forged@ForgeBlockResult{endorserBlock} =
    -- If a ledger change makes the 'serialize'' bytes of a transaction longer
    -- than its 'sizeTxF' plus 'perTxOverhead', the endorser block can exceed
    -- the size that 'txEbMeasureDijkstra' reserved. Header validation then
    -- rejects the block with 'LeiosEbTooBig'. The assert makes a test fail at
    -- the forge instead.
    assert (all fits endorserBlock) forged
   where
    Praos2HasLeios leiosLedgerView =
      plvLeios $ protocolLedgerView (configLedger fbConfig) fbCurrentTickedLedgerState
    fits eb = encodeLeiosEbSize eb <= llvMaxEbBodySize leiosLedgerView
