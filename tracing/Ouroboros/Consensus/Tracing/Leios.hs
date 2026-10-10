{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tracing instances and metrics for the Leios prototype: the Leios kernel and
-- per-peer traces, the LeiosNotify and LeiosFetch mini-protocol messages, and
-- the EB announcement delay CDFs.
module Ouroboros.Consensus.Tracing.Leios
  ( -- * Leios metrics
    initialLeiosMetrics
  , calculateLeiosMetrics
  , LeiosMetrics

    -- * Severity
  , mapLeiosSeverity
  ) where

import Cardano.Logging
import Cardano.Slotting.Slot (SlotNo (..))
import LeiosDemoDb.Trace (LeiosDbStats (..), TraceLeiosDb (..))
import LeiosDemoLogic.Announcements.ElBimap (ElId (..))
import qualified LeiosDemoOnlyTestFetch as LF
import qualified LeiosDemoOnlyTestNotify as LN
import LeiosDemoTypes
  ( AnnouncementFields (..)
  , FetchArrivalBytes (..)
  , LeiosEb
  , LeiosPoint
  , LeiosTx
  , LeiosVote
  , TraceLeiosKernel (..)
  , TraceLeiosPeer (..)
  , messageLeiosFetchToObject
  , messageLeiosNotifyToObject
  , traceLeiosKernelToObject
  , traceLeiosPeerToObject
  )
import qualified LeiosDemoTypes as Leios
import Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , headerLeiosAnnouncement
  )
import qualified Ouroboros.Consensus.Tracing.Cdf as Cdf

--------------------------------------------------------------------------------
-- Leios metrics
--------------------------------------------------------------------------------

-- | Leios metrics which construct CDFs for EB announcements.
data LeiosMetrics' c = LeiosMetrics
  { lmCdfState :: Cdf.State SlotNo
  -- ^ internal state which collects all data points
  , lmCdf200msVar :: !c
  -- ^ count EB announcements which arrived within 200ms
  , lmCdf400msVar :: !c
  -- ^ count EB announcements which arrived within 400ms
  , lmCdf600msVar :: !c
  -- ^ count EB announcements which arrived within 600ms
  , lmCdf800msVar :: !c
  -- ^ count EB announcements which arrived within 800ms
  , lmCdf1000msVar :: !c
  -- ^ count EB announcements which arrived within 1000ms
  , lmCdf1200msVar :: !c
  -- ^ count EB announcements which arrived within 1200ms
  , lmCdf1400msVar :: !c
  -- ^ count EB announcements which arrived within 1400ms
  , lmCdf1600msVar :: !c
  -- ^ count EB announcements which arrived within 1600ms
  , lmCdf1800msVar :: !c
  -- ^ count EB announcements which arrived within 1800ms
  , lmCdf2000msVar :: !c
  -- ^ count EB announcements which arrived within 2000ms
  , lmDelay :: Double
  -- ^ last EB announcement delay value
  , lmTraceVars :: Bool
  , lmTraceIt :: Bool
  }
  deriving Functor

type LeiosMetrics = LeiosMetrics' Cdf.Counter

initialLeiosMetrics :: LeiosMetrics
initialLeiosMetrics =
  LeiosMetrics
    { lmCdfState = Cdf.empty
    , lmCdf200msVar = Cdf.Counter 0.2 0
    , lmCdf400msVar = Cdf.Counter 0.4 0
    , lmCdf600msVar = Cdf.Counter 0.6 0
    , lmCdf800msVar = Cdf.Counter 0.8 0
    , lmCdf1000msVar = Cdf.Counter 1.0 0
    , lmCdf1200msVar = Cdf.Counter 1.2 0
    , lmCdf1400msVar = Cdf.Counter 1.4 0
    , lmCdf1600msVar = Cdf.Counter 1.6 0
    , lmCdf1800msVar = Cdf.Counter 1.8 0
    , lmCdf2000msVar = Cdf.Counter 2.0 0
    , lmDelay = 0
    , lmTraceIt = False
    , lmTraceVars = False
    }

calculateLeiosMetrics ::
  LeiosMetrics ->
  LoggingContext ->
  TraceLeiosKernel ->
  LeiosMetrics
calculateLeiosMetrics
  lm@LeiosMetrics{..}
  _lc
  (TraceLeiosAnnouncementAccepted _ _ fields (Just annDelay)) =
    case announcementElection fields of
      MkElId slotNo@(SlotNo slotNo_) _ ->
        if Cdf.null lmCdfState && annDelay > 20 -- During startup wait until we are in sync
          then nothingToDo
          else
            case Cdf.processDataPoint
              Cdf.defaultConfig -- should come from config file
              (fromIntegral slotNo_, slotNo, annDelay)
              lmCdfState
              lm of
              Nothing -> nothingToDo
              Just (lm', lmCdfState') ->
                lm'
                  { lmCdfState = lmCdfState'
                  , lmTraceIt = True
                  , lmTraceVars = Cdf.size lmCdfState' >= 45 -- wait until we have at least 45 samples before providing cdf estimates
                  , lmDelay = realToFrac annDelay
                  }
   where
    nothingToDo = lm{lmTraceIt = False}
calculateLeiosMetrics lm _lc _ = lm

instance LogFormatting LeiosMetrics where
  forMachine _dtal _ = mempty
  asMetrics LeiosMetrics{lmTraceIt = False} = []
  asMetrics LeiosMetrics{..} =
    lateAnnouncementsAllMetric
      ++ if lmTraceVars
        then
          [ cdfMetric "leios.eb.announcement.delay.cdf200ms" lmCdf200msVar
          , cdfMetric "leios.eb.announcement.delay.cdf400ms" lmCdf400msVar
          , cdfMetric "leios.eb.announcement.delay.cdf600ms" lmCdf600msVar
          , cdfMetric "leios.eb.announcement.delay.cdf800ms" lmCdf800msVar
          , cdfMetric "leios.eb.announcement.delay.cdf1000ms" lmCdf1000msVar
          , cdfMetric "leios.eb.announcement.delay.cdf1200ms" lmCdf1200msVar
          , cdfMetric "leios.eb.announcement.delay.cdf1400ms" lmCdf1400msVar
          , cdfMetric "leios.eb.announcement.delay.cdf1600ms" lmCdf1600msVar
          , cdfMetric "leios.eb.announcement.delay.cdf1800ms" lmCdf1800msVar
          , cdfMetric "leios.eb.announcement.delay.cdf2000ms" lmCdf2000msVar
          ]
        else []
   where
    size = Cdf.size lmCdfState
    cdfMetric name var = DoubleM name (fromIntegral (Cdf.counter var) / fromIntegral size)
    lateAnnouncementsAllMetric = [CounterM "leios.eb.announcement.late" Nothing | lmDelay > 2]

instance MetaTrace LeiosMetrics where
  namespaceFor _ = Namespace [] ["LeiosMetrics"]
  severityFor _ _ = Just Debug
  documentFor _ = Just "EB announcement delay metrics, recomputed on every accepted announcement."

  metricsDocFor (Namespace _ ["LeiosMetrics"]) =
    [ ("leios.eb.announcement.delay.cdf200ms", "probability for EB announcemement to complete within 200ms")
    , ("leios.eb.announcement.delay.cdf400ms", "probability for EB announcemement to complete within 400ms")
    , ("leios.eb.announcement.delay.cdf600ms", "probability for EB announcemement to complete within 600ms")
    , ("leios.eb.announcement.delay.cdf800ms", "probability for EB announcemement to complete within 800ms")
    , ("leios.eb.announcement.delay.cdf1000ms", "probability for EB announcemement to complete within 1000ms")
    , ("leios.eb.announcement.delay.cdf1200ms", "probability for EB announcemement to complete within 1200ms")
    , ("leios.eb.announcement.delay.cdf1400ms", "probability for EB announcemement to complete within 1400ms")
    , ("leios.eb.announcement.delay.cdf1600ms", "probability for EB announcemement to complete within 1600ms")
    , ("leios.eb.announcement.delay.cdf1800ms", "probability for EB announcemement to complete within 1800ms")
    , ("leios.eb.announcement.delay.cdf2000ms", "probability for EB announcemement to complete within 2000ms")
    , ("leios.eb.announcement.late", "number of late EB announcements that took longer than 2s")
    ]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["LeiosMetrics"]
    ]

--------------------------------------------------------------------------------
-- Severity
--------------------------------------------------------------------------------

-- TODO Consensus defines 'Leios.LeiosSeverity' for now; once LeiosDemoTypes can
-- depend on trace-dispatcher, use 'SeverityS' directly and drop this.
mapLeiosSeverity :: Leios.LeiosSeverity -> SeverityS
mapLeiosSeverity = \case
  Leios.LSDebug -> Debug
  Leios.LSInfo -> Info
  Leios.LSNotice -> Notice
  Leios.LSWarning -> Warning
  Leios.LSError -> Error

--------------------------------------------------------------------------------
-- Leios kernel and peer
--------------------------------------------------------------------------------

-- 'forMachine' delegates to 'traceLeiosKernelToObject' so the JSON shape matches
-- the consensus-side tracer (per-constructor fields rather than a 'show' blob).
instance LogFormatting TraceLeiosKernel where
  forMachine _dtal = traceLeiosKernelToObject
  forHuman = Leios.traceLeiosKernelForHuman
  asMetrics = \case
    -- LeiosFetch arrival bytes, partitioned by prior LeiosTxCache state (node policy).
    TraceLeiosFetchBodyArrival fab ->
      [ CounterM "leiosFetchBodyInvalidBytes" (Just (fromIntegral (fabInvalid fab)))
      , CounterM "leiosFetchBodyEvictedBytes" (Just (fromIntegral (fabEvicted fab)))
      , CounterM "leiosFetchBodyGoodBytes" (Just (fromIntegral (fabGood fab)))
      , CounterM "leiosFetchBodyExtraBytes" (Just (fromIntegral (fabExtra fab)))
      ]
    TraceLeiosFetchTxsArrival fab ->
      [ CounterM "leiosFetchTxsInvalidBytes" (Just (fromIntegral (fabInvalid fab)))
      , CounterM "leiosFetchTxsEvictedBytes" (Just (fromIntegral (fabEvicted fab)))
      , CounterM "leiosFetchTxsGoodBytes" (Just (fromIntegral (fabGood fab)))
      , CounterM "leiosFetchTxsExtraBytes" (Just (fromIntegral (fabExtra fab)))
      ]
    -- Sampled on a timer, so these exist from node start.
    TraceLeiosDb (TraceLeiosDbStats (LeiosDbStats{volatileEbs, immutableEbs, walBytes})) ->
      [ IntM "leiosDbVolatileEbs" (fromIntegral volatileEbs)
      , IntM "leiosDbImmutableEbs" (fromIntegral immutableEbs)
      , IntM "leiosDbWalBytes" walBytes
      ]
    -- Accumulating counters, bumped per copier commit / GC pass.
    TraceLeiosDb (TraceLeiosDbCopiedToImmutable copiedEbs) ->
      [CounterM "leiosDbCopiedEbs" (Just copiedEbs)]
    TraceLeiosDb (TraceLeiosDbEvicted evictedEbs) ->
      [CounterM "leiosDbEvictedEbs" (Just evictedEbs)]
    TraceLeiosDb TraceLeiosDbGCError{} ->
      [CounterM "leiosDbSweepErrors" (Just 1)]
    TraceLeiosDb TraceLeiosDbCopyError{} ->
      [CounterM "leiosDbCopyErrors" (Just 1)]
    _ -> []

instance MetaTrace TraceLeiosKernel where
  namespaceFor = Namespace [] . Leios.nsiPath . Leios.leiosKernelNSInfo . Leios.leiosKernelNSOf
  severityFor (Namespace _ p) _ = mapLeiosSeverity . Leios.nsiSeverity <$> Leios.leiosKernelNSByPath p
  documentFor _ = Nothing
  metricsDocFor (Namespace _ p) = maybe [] Leios.nsiMetricsDoc (Leios.leiosKernelNSByPath p)
  allNamespaces = Namespace [] <$> Leios.leiosKernelNSPaths

instance LogFormatting TraceLeiosPeer where
  forMachine _dtal = traceLeiosPeerToObject
  forHuman = Leios.traceLeiosPeerForHuman
  asMetrics _ = []

instance MetaTrace TraceLeiosPeer where
  namespaceFor = Namespace [] . Leios.nsiPath . Leios.leiosPeerNSInfo . Leios.leiosPeerNSOf
  severityFor (Namespace _ p) _ = mapLeiosSeverity . Leios.nsiSeverity <$> Leios.leiosPeerNSByPath p
  documentFor _ = Nothing
  metricsDocFor (Namespace _ p) = maybe [] Leios.nsiMetricsDoc (Leios.leiosPeerNSByPath p)
  allNamespaces = Namespace [] <$> Leios.leiosPeerNSPaths

--------------------------------------------------------------------------------
-- LeiosNotify / LeiosFetch
--
-- Delegate to the compact 'messageLeiosNotifyToObject' /
-- 'messageLeiosFetchToObject' so MsgLeiosBlock(Txs) lines emit just the
-- metadata and not the full tx CBOR payload that the derived 'Show' would dump.
--------------------------------------------------------------------------------

instance
  ( Show (Header blk)
  , ResolveLeiosBlock blk
  ) =>
  LogFormatting (AnyMessage (LN.LeiosNotify LeiosPoint (Header blk) LeiosVote))
  where
  forHuman = showT

  forMachine _dtal (AnyMessageAndAgency _stok msg) =
    messageLeiosNotifyToObject headerLeiosAnnouncement msg

instance LogFormatting (AnyMessage (LF.LeiosFetch LeiosPoint LeiosEb LeiosTx)) where
  forHuman = showT

  forMachine _dtal (AnyMessageAndAgency _stok msg) =
    messageLeiosFetchToObject msg

instance MetaTrace (AnyMessage (LN.LeiosNotify LeiosPoint (Header blk) LeiosVote)) where
  namespaceFor (AnyMessageAndAgency _stok msg) = case msg of
    LN.MsgLeiosNotificationRequestNext{} -> Namespace [] ["RequestNext"]
    LN.MsgLeiosBlockAnnouncement{} -> Namespace [] ["BlockAnnouncement"]
    LN.MsgLeiosBlockOffer{} -> Namespace [] ["BlockOffer"]
    LN.MsgLeiosBlockTxsOffer{} -> Namespace [] ["BlockTxsOffer"]
    LN.MsgLeiosVotes{} -> Namespace [] ["Votes"]
    LN.MsgDone -> Namespace [] ["Done"]
    LN.MsgQuit -> Namespace [] ["Quit"]
    LN.MsgCanceled -> Namespace [] ["Canceled"]

  severityFor _ _ = Just Debug

  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["RequestNext"]
    , Namespace [] ["BlockAnnouncement"]
    , Namespace [] ["BlockOffer"]
    , Namespace [] ["BlockTxsOffer"]
    , Namespace [] ["Votes"]
    , Namespace [] ["Done"]
    , Namespace [] ["Quit"]
    , Namespace [] ["Canceled"]
    ]

instance MetaTrace (AnyMessage (LF.LeiosFetch LeiosPoint LeiosEb LeiosTx)) where
  namespaceFor (AnyMessageAndAgency _stok msg) = case msg of
    LF.MsgLeiosBlockRequest{} -> Namespace [] ["BlockRequest"]
    LF.MsgLeiosBlock{} -> Namespace [] ["Block"]
    LF.MsgLeiosBlockTxsRequest{} -> Namespace [] ["BlockTxsRequest"]
    LF.MsgLeiosBlockTxs{} -> Namespace [] ["BlockTxs"]
    LF.MsgDone -> Namespace [] ["Done"]

  severityFor _ _ = Just Debug

  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockRequest"]
    , Namespace [] ["Block"]
    , Namespace [] ["BlockTxsRequest"]
    , Namespace [] ["BlockTxs"]
    , Namespace [] ["Done"]
    ]
