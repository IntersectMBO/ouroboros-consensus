{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the node's LeiosFetch server's events are logged.
module Ouroboros.Consensus.Tracing.LeiosFetch () where

import Cardano.Logging
import Control.Exception (displayException)
import Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), prettyEbHash, prettyLeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..))

-- The exceptions these events carry disconnect only the client, not the whole
-- node, so their fields are rendered for structured logging, rather than left
-- to the network layer's traces of the same exceptions, which 'show' them.
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
