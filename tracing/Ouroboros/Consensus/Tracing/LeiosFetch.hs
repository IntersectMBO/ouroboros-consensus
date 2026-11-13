{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the node's LeiosFetch client's and server's events are logged.
module Ouroboros.Consensus.Tracing.LeiosFetch () where

import Cardano.Crypto.Hash (hashToStringAsHex)
import Cardano.Logging
import Control.Exception (displayException)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..))
import Data.Aeson (Value (String), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), prettyLeiosPoint)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace

instance
  (LogFormatting peer, Show peer) =>
  LogFormatting (TraceLeiosFetchClientEvent peer)
  where
  forMachine dtal = \case
    TraceLeiosFetchClientBlockArrived peer point arrival ->
      mconcat
        [ "kind" .= String "BlockArrived"
        , "peer" .= forMachine dtal peer
        , pointFields point
        , arrivalFields arrival
        ]
    TraceLeiosFetchClientBlockFromCache point ->
      mconcat ["kind" .= String "BlockFromCache", pointFields point]
    TraceLeiosFetchClientTxsArrived peer point arrival ->
      mconcat
        [ "kind" .= String "TxsArrived"
        , "peer" .= forMachine dtal peer
        , pointFields point
        , arrivalFields arrival
        ]
    TraceLeiosFetchClientBlockAcquired point age ->
      mconcat ["kind" .= String "BlockAcquired", pointFields point, "age" .= age]
    TraceLeiosFetchClientBlockTxsAcquired point age ->
      mconcat ["kind" .= String "BlockTxsAcquired", pointFields point, "age" .= age]
    TraceLeiosFetchClientClosureSummary point summary ->
      mconcat ["kind" .= String "ClosureSummary", pointFields point, summaryFields summary]
    TraceLeiosFetchClientPolicy peer invocation event ->
      mconcat ["peer" .= forMachine dtal peer, "invocation" .= invocation, routineFields event]
   where
    arrivalFields (MkFetchArrivalBytes invalid evicted good extra) =
      mconcat
        ["invalidBytes" .= invalid, "evictedBytes" .= evicted, "goodBytes" .= good, "extraBytes" .= extra]

  forHuman = \case
    TraceLeiosFetchClientBlockArrived peer point _ ->
      "Endorser block " <> pretty point <> " arrived from " <> showT peer
    TraceLeiosFetchClientBlockFromCache point ->
      "Took endorser block " <> pretty point <> " from the LeiosTxCache"
    TraceLeiosFetchClientTxsArrived peer point _ ->
      "Transactions of endorser block " <> pretty point <> " arrived from " <> showT peer
    TraceLeiosFetchClientBlockAcquired point _ -> "Acquired endorser block " <> pretty point
    TraceLeiosFetchClientBlockTxsAcquired point _ ->
      "Acquired the transactions of endorser block " <> pretty point
    TraceLeiosFetchClientClosureSummary point summary ->
      "Closure acquisition " <> pretty point <> ": " <> showT summary
    TraceLeiosFetchClientPolicy peer invocation event ->
      "LeiosFetch invocation " <> showT invocation <> " for " <> showT peer <> ": " <> showT event
   where
    pretty = Text.pack . prettyLeiosPoint

instance MetaTrace (TraceLeiosFetchClientEvent peer) where
  namespaceFor = \case
    TraceLeiosFetchClientBlockArrived{} -> Namespace [] ["BlockArrived"]
    TraceLeiosFetchClientBlockFromCache{} -> Namespace [] ["BlockFromCache"]
    TraceLeiosFetchClientTxsArrived{} -> Namespace [] ["TxsArrived"]
    TraceLeiosFetchClientBlockAcquired{} -> Namespace [] ["BlockAcquired"]
    TraceLeiosFetchClientBlockTxsAcquired{} -> Namespace [] ["BlockTxsAcquired"]
    TraceLeiosFetchClientClosureSummary{} -> Namespace [] ["ClosureSummary"]
    TraceLeiosFetchClientPolicy _ _ event -> Namespace [] [routineName event]

  severityFor (Namespace _ ["BlockArrived"]) _ = Just Debug
  severityFor (Namespace _ ["BlockFromCache"]) _ = Just Debug
  severityFor (Namespace _ ["TxsArrived"]) _ = Just Debug
  severityFor (Namespace _ ["BlockAcquired"]) _ = Just Info
  severityFor (Namespace _ ["BlockTxsAcquired"]) _ = Just Info
  severityFor (Namespace _ [name]) _ | name `elem` routineNames || name == "ClosureSummary" = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["BlockArrived"]) =
    Just "An endorser block arrived, its bytes classified by whether the node needed them."
  documentFor (Namespace _ ["BlockFromCache"]) =
    Just
      "The node took an endorser block from the body its LeiosTxCache held for another point, rather than fetching it."
  documentFor (Namespace _ ["TxsArrived"]) =
    Just
      "Some of an endorser block's transactions arrived, their bytes classified by whether the node needed them."
  documentFor (Namespace _ ["BlockAcquired"]) =
    Just "The node durably holds an endorser block it fetched."
  documentFor (Namespace _ ["BlockTxsAcquired"]) =
    Just "The node durably holds all the transactions of an endorser block it fetched."
  documentFor (Namespace _ ["ClosureSummary"]) =
    Just
      "Distinct confirmed closure payload origins; excludes framing, retries and redundant wire traffic."
  documentFor (Namespace _ [name])
    | name `elem` routineNames =
        Just "An ordinary fetch policy transition, captured by its original request or invocation owner."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockArrived"]
    , Namespace [] ["BlockFromCache"]
    , Namespace [] ["TxsArrived"]
    , Namespace [] ["BlockAcquired"]
    , Namespace [] ["BlockTxsAcquired"]
    ]
      ++ [Namespace [] [name] | name <- "ClosureSummary" : routineNames]

pointFields :: LeiosPoint -> Aeson.Object
pointFields (MkLeiosPoint (SlotNo slot) ebHash) =
  mconcat ["slot" .= slot, "ebHash" .= String (Text.pack (hashToStringAsHex ebHash))]

-- The exceptions these events carry disconnect only the client, not the whole
-- node, so their fields are rendered for structured logging, rather than left
-- to the network layer's traces of the same exceptions, which 'show' them.

instance LogFormatting TraceLeiosFetchServerEvent where
  forMachine _dtal = \case
    TraceLeiosFetchServerBlockRequested point ->
      mconcat ["kind" .= String "BlockRequested", pointFields point]
    TraceLeiosFetchServerBlockServed point bytes ->
      mconcat ["kind" .= String "BlockServed", pointFields point, "bytes" .= bytes]
    TraceLeiosFetchServerTxsRequested point txCount ->
      mconcat ["kind" .= String "TxsRequested", pointFields point, "txCount" .= txCount]
    TraceLeiosFetchServerTxsServed point txCount bytes ->
      mconcat
        ["kind" .= String "TxsServed", pointFields point, "txCount" .= txCount, "bytes" .= bytes]
    TraceLeiosFetchServerInvalidRequest exn -> case exn of
      ExnLeiosUnknownBlockRequested point ->
        mconcat ["kind" .= String "UnknownBlock", pointFields point]
      ExnLeiosUnknownTxsRequested point offsets ->
        mconcat ["kind" .= String "UnknownTxs", pointFields point, "offsets" .= offsets]
    TraceLeiosFetchServerBlockDbFailure point err ->
      mconcat ["kind" .= String "BlockDbFailure", pointFields point, "exception" .= err]
    TraceLeiosFetchServerTxsDbFailure point bitmaps err ->
      mconcat
        [ "kind" .= String "TxsDbFailure"
        , pointFields point
        , "bitmaps" .= bitmaps
        , "exception" .= err
        ]
    TraceLeiosFetchServerOpenReaderDbFailure err ->
      mconcat ["kind" .= String "OpenReaderDbFailure", "exception" .= err]

  forHuman = \case
    TraceLeiosFetchServerBlockRequested point ->
      "Requested endorser block " <> pretty point
    TraceLeiosFetchServerBlockServed point bytes ->
      "Served endorser block " <> pretty point <> ", of " <> showT bytes <> " bytes"
    TraceLeiosFetchServerTxsRequested point txCount ->
      "Requested " <> showT txCount <> " transactions of endorser block " <> pretty point
    TraceLeiosFetchServerTxsServed point txCount bytes ->
      "Served "
        <> showT txCount
        <> " transactions of endorser block "
        <> pretty point
        <> ", "
        <> showT bytes
        <> " bytes"
    TraceLeiosFetchServerInvalidRequest exn -> case exn of
      ExnLeiosUnknownBlockRequested point ->
        "Disconnecting a client that requested endorser block " <> pretty point <> ", which is not held"
      ExnLeiosUnknownTxsRequested point offsets ->
        "Disconnecting a client that requested transactions of endorser block "
          <> pretty point
          <> " at offsets "
          <> showT offsets
          <> ", which are not held"
    TraceLeiosFetchServerBlockDbFailure point err ->
      "Disconnecting a client, since reading its requested endorser block "
        <> pretty point
        <> " from the LeiosDB failed: "
        <> Text.pack (displayException err)
    TraceLeiosFetchServerTxsDbFailure point _bitmaps err ->
      "Disconnecting a client, since reading its requested transactions of endorser block "
        <> pretty point
        <> " from the LeiosDB failed: "
        <> Text.pack (displayException err)
    TraceLeiosFetchServerOpenReaderDbFailure err ->
      "Disconnecting a client, since opening its LeiosDB reader failed: "
        <> Text.pack (displayException err)
   where
    pretty = Text.pack . prettyLeiosPoint

instance MetaTrace TraceLeiosFetchServerEvent where
  namespaceFor = \case
    TraceLeiosFetchServerBlockRequested{} -> Namespace [] ["BlockRequested"]
    TraceLeiosFetchServerBlockServed{} -> Namespace [] ["BlockServed"]
    TraceLeiosFetchServerTxsRequested{} -> Namespace [] ["TxsRequested"]
    TraceLeiosFetchServerTxsServed{} -> Namespace [] ["TxsServed"]
    TraceLeiosFetchServerInvalidRequest ExnLeiosUnknownBlockRequested{} -> Namespace [] ["UnknownBlock"]
    TraceLeiosFetchServerInvalidRequest ExnLeiosUnknownTxsRequested{} -> Namespace [] ["UnknownTxs"]
    TraceLeiosFetchServerBlockDbFailure{} -> Namespace [] ["BlockDbFailure"]
    TraceLeiosFetchServerTxsDbFailure{} -> Namespace [] ["TxsDbFailure"]
    TraceLeiosFetchServerOpenReaderDbFailure{} -> Namespace [] ["OpenReaderDbFailure"]

  severityFor (Namespace _ ["BlockRequested"]) _ = Just Debug
  severityFor (Namespace _ ["BlockServed"]) _ = Just Info
  severityFor (Namespace _ ["TxsRequested"]) _ = Just Debug
  severityFor (Namespace _ ["TxsServed"]) _ = Just Info
  severityFor (Namespace _ ["UnknownBlock"]) _ = Just Notice
  severityFor (Namespace _ ["UnknownTxs"]) _ = Just Notice
  severityFor (Namespace _ ["BlockDbFailure"]) _ = Just Error
  severityFor (Namespace _ ["TxsDbFailure"]) _ = Just Error
  severityFor (Namespace _ ["OpenReaderDbFailure"]) _ = Just Error
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["BlockRequested"]) =
    Just "A client requested an endorser block."
  documentFor (Namespace _ ["BlockServed"]) =
    Just "The server sent a client an endorser block."
  documentFor (Namespace _ ["TxsRequested"]) =
    Just "A client requested some of an endorser block's transactions."
  documentFor (Namespace _ ["TxsServed"]) =
    Just "The server sent a client some of an endorser block's transactions."
  documentFor (Namespace _ ["UnknownBlock"]) =
    Just
      "A client requested an endorser block that the node does not hold, so the server disconnects it."
  documentFor (Namespace _ ["UnknownTxs"]) =
    Just
      "A client requested transactions that the node does not hold, so the server disconnects it."
  documentFor (Namespace _ ["BlockDbFailure"]) =
    Just
      "Reading a client's requested endorser block from the LeiosDB failed, so the server disconnects the client."
  documentFor (Namespace _ ["TxsDbFailure"]) =
    Just
      "Reading a client's requested transactions from the LeiosDB failed, so the server disconnects the client."
  documentFor (Namespace _ ["OpenReaderDbFailure"]) =
    Just "Opening the server's LeiosDB reader failed, so the server disconnects the client."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockRequested"]
    , Namespace [] ["BlockServed"]
    , Namespace [] ["TxsRequested"]
    , Namespace [] ["TxsServed"]
    , Namespace [] ["UnknownBlock"]
    , Namespace [] ["UnknownTxs"]
    , Namespace [] ["BlockDbFailure"]
    , Namespace [] ["TxsDbFailure"]
    , Namespace [] ["OpenReaderDbFailure"]
    ]

-- These format only detached scalar facts; the source events contain no
-- response vectors, request commitments, candidate maps or live state readers.
instance (LogFormatting peer, Show peer) => LogFormatting (TraceLeiosFetchDetailEvent peer) where
  forMachine dtal (TraceLeiosFetchDetailEvent peer invocation event) =
    mconcat ["peer" .= forMachine dtal peer, "invocation" .= invocation, detailFields event]
  forHuman (TraceLeiosFetchDetailEvent peer invocation event) =
    "LeiosFetch invocation " <> showT invocation <> " for " <> showT peer <> ": " <> showT event

instance MetaTrace (TraceLeiosFetchDetailEvent peer) where
  namespaceFor (TraceLeiosFetchDetailEvent _ _ event) = Namespace [] [detailName event]
  severityFor (Namespace _ [name]) _ | name `elem` detailNames = Just Debug
  severityFor _ _ = Nothing
  documentFor (Namespace _ [name])
    | name `elem` detailNames =
        Just
          "A captured request lifecycle fact. Times are original monotonic observations; this independently suppressible tracer emits no packet fragments."
  documentFor _ = Nothing
  allNamespaces = [Namespace [] [name] | name <- detailNames]

routineName :: FetchRoutine -> Text.Text
routineName = \case
  FetchUpstreamChanged{} -> "UpstreamStateChanged"
  FetchHedgeOpened{} -> "HedgeOpened"
  FetchBodyWon{} -> "BodyWinner"
  FetchPenaltyCharged{} -> "PenaltyCharged"
  FetchInvocationClosed{} -> "InvocationClosed"
  FetchRegistrationRemoved{} -> "RegistrationRemoved"

routineNames :: [Text.Text]
routineNames =
  [ "UpstreamStateChanged"
  , "HedgeOpened"
  , "BodyWinner"
  , "PenaltyCharged"
  , "InvocationClosed"
  , "RegistrationRemoved"
  ]

routineFields :: FetchRoutine -> Aeson.Object
routineFields event = mconcat ["kind" .= routineName event, fields]
 where
  fields = case event of
    FetchUpstreamChanged blocked -> "blocked" .= blocked
    FetchHedgeOpened request cause -> mconcat [requestFields request, "cause" .= showT cause]
    FetchBodyWon request actual -> mconcat [requestFields request, "actualBytes" .= actual, "stage" .= String "WriterAccepted"]
    FetchPenaltyCharged request at previous effective following ->
      mconcat
        [ requestFields request
        , "eventAt" .= seconds at
        , "savedBefore" .= penaltyFields previous
        , "effectiveBefore" .= P.scoreValue effective
        , "savedAfter" .= penaltyFields following
        ]
    FetchInvocationClosed used failed -> mconcat [usageFields used, "failed" .= failed]
    FetchRegistrationRemoved used -> usageFields used

requestFields :: FetchRequest -> Aeson.Object
requestFields (FetchRequest ordinal point job bytes) =
  mconcat
    [pointFields point, "ordinal" .= ordinal, "job" .= job, "payloadBytes" .= bytes]

usageFields :: FetchUsage -> Aeson.Object
usageFields (FetchUsage bytes bodies byteLimit bodyLimit) =
  mconcat
    [ "txBytesReserved" .= bytes
    , "bodiesReserved" .= bodies
    , "txByteLimit" .= byteLimit
    , "bodyCountLimit" .= bodyLimit
    ]

penaltyFields :: P.Snapshot -> Aeson.Object
penaltyFields value = mconcat ["score" .= P.scoreValue (P.savedScore value), "at" .= fmap seconds (P.savedAt value)]

summaryFields :: ClosureSummary -> Aeson.Object
summaryFields (ClosureSummary total local network transactions missing forged) =
  mconcat
    [ "closureBytes" .= total
    , "localBytes" .= local
    , "uniqueNetworkBytes" .= network
    , "uniqueNetworkTransactions" .= transactions
    , "missingAtBodyBytes" .= missing
    , "completedByForge" .= forged
    ]

detailName :: FetchDetail -> Text.Text
detailName = \case
  FetchAdmitted{} -> "RequestAdmitted"
  FetchTransition{} -> "Transition"
  FetchHeadActivated{} -> "HeadActivated"
  FetchPressureChanged{} -> "PeerPressureChanged"

detailNames :: [Text.Text]
detailNames = ["RequestAdmitted", "Transition", "HeadActivated", "PeerPressureChanged"]

detailFields :: FetchDetail -> Aeson.Object
detailFields event = mconcat ["kind" .= detailName event, fields]
 where
  fields = case event of
    FetchAdmitted request (FetchBudget rate rtt transfer cold source) at used selected ->
      mconcat
        [ requestFields request
        , usageFields used
        , "eventAt" .= seconds at
        , "minimumBitsPerSecond" .= rate
        , "rttAllowanceSeconds" .= duration rtt
        , "transferSeconds" .= duration transfer
        , "startupSeconds" .= duration cold
        , "rttSource" .= showT source
        , maybe mempty selectionFields selected
        ]
    FetchTransition request phase at used ->
      mconcat
        [requestFields request, usageFields used, "phase" .= showT phase, "eventAt" .= seconds at]
    FetchHeadActivated request started deadline continuous ->
      mconcat
        [ requestFields request
        , "headStart" .= seconds started
        , "deadline" .= seconds deadline
        , "continuous" .= continuous
        ]
    FetchPressureChanged limits used -> mconcat [usageFields used, "limits" .= map showT limits]

selectionFields :: FetchSelection -> Aeson.Object
selectionFields (FetchSelection at score offered) =
  mconcat
    [ "selectionAt" .= seconds at
    , "selectionScore" .= P.scoreValue score
    , "firstClosureOfferAt" .= fmap seconds offered
    ]

seconds :: Time -> Double
seconds (Time value) = duration value

duration :: DiffTime -> Double
duration = realToFrac
