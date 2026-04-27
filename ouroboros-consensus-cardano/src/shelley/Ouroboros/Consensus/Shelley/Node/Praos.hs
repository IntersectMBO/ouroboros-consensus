{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.Praos
  ( -- * BlockForging
    praosBlockForging
  , praosSharedBlockForging
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import Control.Tracer (traceWith)
import qualified Data.Text as T
import LeiosDemoDb
  ( LeiosDbConnection (leiosDbQueryCertificateByPoint, leiosDbQueryCompletedEbByPoint)
  )
import LeiosDemoTypes
  ( EbAnnouncement (EbAnnouncement, ebAnnouncementHash)
  , LeiosPoint (..)
  , TraceLeiosKernel (MkTraceLeiosKernel)
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configConsensus)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos
  ( LeiosState (LeiosState, leiosStateCanCertify, leiosStatePreviousAnnouncement)
  , Praos
  , PraosParams (..)
  , PraosState (praosStateLastSlot, praosStateLeios)
  , praosCheckCanForge
  , withOriginToSlotNo
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosCanBeLeader (praosCanBeLeaderOpCert)
  )
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  , forgeShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Node.Common
  ( ShelleyEraWithCrypto
  , ShelleyLeaderCredentials (..)
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
  , Mempool.TxLimits (ShelleyBlock (Praos c) era)
  , IOLike m
  ) =>
  PraosParams ->
  ShelleyLeaderCredentials c ->
  m (BlockForging m (ShelleyBlock (Praos c) era))
praosBlockForging praosParams credentials = do
  hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod praosMaxKESEvo
  pure $ praosSharedBlockForging hotKey slotToPeriod credentials
 where
  PraosParams{praosMaxKESEvo, praosSlotsPerKESPeriod} = praosParams

  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsInitSignKey = initSignKey
    , shelleyLeaderCredentialsCanBeLeader = canBeLeader
    } = credentials

  startPeriod :: Absolute.KESPeriod
  startPeriod = SL.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

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
  ) =>
  HotKey.HotKey c m ->
  (SlotNo -> Absolute.KESPeriod) ->
  ShelleyLeaderCredentials c ->
  BlockForging m (ShelleyBlock (Praos c) era)
praosSharedBlockForging
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader = canBeLeader
    , shelleyLeaderCredentialsLabel = label
    } = do
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
      , forgeBlock = forgeShelleyBlock hotKey canBeLeader
      , leiosDecideForgeType = leiosDecideForgeTypePraos
      }

leiosDecideForgeTypePraos ::
  Monad m => LeiosDecideForgeTypeArgs m (ShelleyBlock (Praos c) era) -> m ForgeType
leiosDecideForgeTypePraos args = do
  let
    leiosDb = ldftaLeiosDb args
    traceLeios = traceWith (ldftaLeiosTracer args)
    praosState = ldftaChainDepState args
    LeiosState{..} = praosStateLeios praosState
  traceLeios $ MkTraceLeiosKernel $ "leiosDecideForgeTypePraos called with: " <> show praosState
  case leiosStatePreviousAnnouncement of
    Nothing -> do
      traceLeios $ MkTraceLeiosKernel $ "leiosDecideForgeTypePraos: No previous EB announcement"
      return ForgeTxsRb
    Just (EbAnnouncement{..}) ->
      if leiosStateCanCertify
        then do
          let ebPoint =
                MkLeiosPoint
                  { pointSlotNo = withOriginToSlotNo . praosStateLastSlot $ praosState -- NOTE(bladyjoker): We're using previous slot no (as in of the previous block)
                  , pointEbHash = ebAnnouncementHash
                  }
          mayEbClosure <- leiosDbQueryCompletedEbByPoint leiosDb ebPoint
          case mayEbClosure of
            Nothing -> do
              traceLeios $ MkTraceLeiosKernel $ "leiosDecideForgeTypePraos: EB not downloaded " <> show ebPoint -- TODO(bladyjoker): Structure message
              return ForgeTxsRb
            Just ebClosure -> do
              mayCert <- leiosDbQueryCertificateByPoint leiosDb ebPoint
              case mayCert of
                Nothing -> do
                  traceLeios $
                    MkTraceLeiosKernel $
                      "leiosDecideForgeTypePraos: EB downloaded but no certificate (voting not finished?) "
                        <> show ebPoint -- TODO(bladyjoker): Structure message
                  return ForgeTxsRb
                Just cert -> do
                  traceLeios $
                    MkTraceLeiosKernel $
                      "leiosDecideForgeTypePraos: EB downloaded " <> show ebPoint <> " and certified " <> show cert -- TODO(bladyjoker): Structure message
                  return $ ForgeCertRb cert ebClosure
        else do
          traceLeios $ MkTraceLeiosKernel $ "leiosDecideForgeTypePraos: Can't certify yet"
          return ForgeTxsRb
