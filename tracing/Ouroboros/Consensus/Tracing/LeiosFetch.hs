{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the node's LeiosFetch client's and server's events are logged.
module Ouroboros.Consensus.Tracing.LeiosFetch () where

import Cardano.Crypto.Hash (hashToStringAsHex)
import Cardano.Logging
import Control.Exception (displayException)
import Data.Aeson (Value (String), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), prettyLeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes (..)
  , TraceLeiosFetchClientEvent (..)
  , TraceLeiosFetchServerEvent (..)
  )

instance LogFormatting TraceLeiosFetchClientEvent where
  forMachine _dtal = \case
    TraceLeiosFetchClientBlockArrived point arrival ->
      mconcat ["kind" .= String "BlockArrived", pointFields point, arrivalFields arrival]
    TraceLeiosFetchClientTxsArrived point arrival ->
      mconcat ["kind" .= String "TxsArrived", pointFields point, arrivalFields arrival]
    TraceLeiosFetchClientBlockAcquired point age ->
      mconcat ["kind" .= String "BlockAcquired", pointFields point, "age" .= age]
    TraceLeiosFetchClientBlockTxsAcquired point age ->
      mconcat ["kind" .= String "BlockTxsAcquired", pointFields point, "age" .= age]
   where
    arrivalFields (MkFetchArrivalBytes invalid evicted good extra) =
      mconcat
        ["invalidBytes" .= invalid, "evictedBytes" .= evicted, "goodBytes" .= good, "extraBytes" .= extra]

  forHuman = \case
    TraceLeiosFetchClientBlockArrived point _ -> "Endorser block " <> pretty point <> " arrived"
    TraceLeiosFetchClientTxsArrived point _ -> "Transactions of endorser block " <> pretty point <> " arrived"
    TraceLeiosFetchClientBlockAcquired point _ -> "Acquired endorser block " <> pretty point
    TraceLeiosFetchClientBlockTxsAcquired point _ ->
      "Acquired the transactions of endorser block " <> pretty point
   where
    pretty = Text.pack . prettyLeiosPoint

instance MetaTrace TraceLeiosFetchClientEvent where
  namespaceFor = \case
    TraceLeiosFetchClientBlockArrived{} -> Namespace [] ["BlockArrived"]
    TraceLeiosFetchClientTxsArrived{} -> Namespace [] ["TxsArrived"]
    TraceLeiosFetchClientBlockAcquired{} -> Namespace [] ["BlockAcquired"]
    TraceLeiosFetchClientBlockTxsAcquired{} -> Namespace [] ["BlockTxsAcquired"]

  severityFor (Namespace _ ["BlockArrived"]) _ = Just Debug
  severityFor (Namespace _ ["TxsArrived"]) _ = Just Debug
  severityFor (Namespace _ ["BlockAcquired"]) _ = Just Info
  severityFor (Namespace _ ["BlockTxsAcquired"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["BlockArrived"]) =
    Just "An endorser block arrived, its bytes classified by whether the node needed them."
  documentFor (Namespace _ ["TxsArrived"]) =
    Just
      "Some of an endorser block's transactions arrived, their bytes classified by whether the node needed them."
  documentFor (Namespace _ ["BlockAcquired"]) =
    Just "The node durably holds an endorser block it fetched."
  documentFor (Namespace _ ["BlockTxsAcquired"]) =
    Just "The node durably holds all the transactions of an endorser block it fetched."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockArrived"]
    , Namespace [] ["TxsArrived"]
    , Namespace [] ["BlockAcquired"]
    , Namespace [] ["BlockTxsAcquired"]
    ]

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
