{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the node's LeiosFetch server's events are logged.
module Ouroboros.Consensus.Tracing.LeiosFetch () where

import Cardano.Logging
import Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), prettyEbHash, prettyLeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..))

instance LogFormatting TraceLeiosFetchServerEvent where
  forMachine _dtal = \case
    TraceLeiosFetchServerBlockRequested point ->
      mconcat ["kind" .= String "BlockRequested", pointFields point]
    TraceLeiosFetchServerBlockServed point txCount ->
      mconcat ["kind" .= String "BlockServed", pointFields point, "txCount" .= txCount]
    TraceLeiosFetchServerTxsRequested point txCount ->
      mconcat ["kind" .= String "TxsRequested", pointFields point, "txCount" .= txCount]
    TraceLeiosFetchServerTxsServed point txCount bytes ->
      mconcat
        ["kind" .= String "TxsServed", pointFields point, "txCount" .= txCount, "bytes" .= bytes]
    TraceLeiosFetchServerUnknownBlock point ->
      mconcat ["kind" .= String "UnknownBlock", pointFields point]
    TraceLeiosFetchServerUnknownTxs point offsets ->
      mconcat ["kind" .= String "UnknownTxs", pointFields point, "offsets" .= offsets]
    TraceLeiosFetchServerDbException point err ->
      mconcat ["kind" .= String "DbException", pointFields point, "error" .= String (Text.pack err)]
   where
    pointFields (MkLeiosPoint (SlotNo slot) ebHash) =
      mconcat ["slot" .= slot, "ebHash" .= String (Text.pack (prettyEbHash ebHash))]

  forHuman = \case
    TraceLeiosFetchServerBlockRequested point ->
      "Requested endorser block " <> pretty point
    TraceLeiosFetchServerBlockServed point txCount ->
      "Served endorser block " <> pretty point <> ", referencing " <> showT txCount <> " transactions"
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
    TraceLeiosFetchServerUnknownBlock point ->
      "Disconnecting a client that requested endorser block " <> pretty point <> ", which is not held"
    TraceLeiosFetchServerUnknownTxs point offsets ->
      "Disconnecting a client that requested transactions of endorser block "
        <> pretty point
        <> " at offsets "
        <> showT offsets
        <> ", which are not held"
    TraceLeiosFetchServerDbException point err ->
      "Reading endorser block " <> pretty point <> " from the LeiosDB failed: " <> Text.pack err
   where
    pretty = Text.pack . prettyLeiosPoint

instance MetaTrace TraceLeiosFetchServerEvent where
  namespaceFor = \case
    TraceLeiosFetchServerBlockRequested{} -> Namespace [] ["BlockRequested"]
    TraceLeiosFetchServerBlockServed{} -> Namespace [] ["BlockServed"]
    TraceLeiosFetchServerTxsRequested{} -> Namespace [] ["TxsRequested"]
    TraceLeiosFetchServerTxsServed{} -> Namespace [] ["TxsServed"]
    TraceLeiosFetchServerUnknownBlock{} -> Namespace [] ["UnknownBlock"]
    TraceLeiosFetchServerUnknownTxs{} -> Namespace [] ["UnknownTxs"]
    TraceLeiosFetchServerDbException{} -> Namespace [] ["DbException"]

  severityFor (Namespace _ ["BlockRequested"]) _ = Just Debug
  severityFor (Namespace _ ["BlockServed"]) _ = Just Info
  severityFor (Namespace _ ["TxsRequested"]) _ = Just Debug
  severityFor (Namespace _ ["TxsServed"]) _ = Just Info
  severityFor (Namespace _ ["UnknownBlock"]) _ = Just Notice
  severityFor (Namespace _ ["UnknownTxs"]) _ = Just Notice
  severityFor (Namespace _ ["DbException"]) _ = Just Error
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
  documentFor (Namespace _ ["DbException"]) =
    Just "Reading the LeiosDB failed, so the server disconnects the client."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockRequested"]
    , Namespace [] ["BlockServed"]
    , Namespace [] ["TxsRequested"]
    , Namespace [] ["TxsServed"]
    , Namespace [] ["UnknownBlock"]
    , Namespace [] ["UnknownTxs"]
    , Namespace [] ["DbException"]
    ]
