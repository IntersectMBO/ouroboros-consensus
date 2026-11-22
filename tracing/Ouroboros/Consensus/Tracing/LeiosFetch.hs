{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the node's LeiosFetch client's and server's events, and the LeiosFetch
-- messages it sends and receives, are logged.
--
-- The messages' fields follow the Leios prototype's, so that tools reading its
-- logs, like the Leios visualizer, read these too.
module Ouroboros.Consensus.Tracing.LeiosFetch () where

import Cardano.Crypto.Hash (hashToStringAsHex)
import Cardano.Logging
import Control.Exception (displayException)
import Data.Aeson (Object, Value (String), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Data.Word (Word16, Word64)
import Network.TypedProtocol.Codec (AnyMessage (..))
import Numeric (showHex)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , hashSerialisedEndorserBlock
  , prettyLeiosPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes (..)
  , LeiosBlockTxSources (..)
  , LeiosTxCacheInsertBodySummary (..)
  , TraceLeiosFetchClientEvent (..)
  , TraceLeiosFetchServerEvent (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (LeiosFetch, Message (..))
import Ouroboros.Network.Block (Serialised (..))

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
    TraceLeiosFetchClientBlockTxSources point sources summary ->
      mconcat
        [ "kind" .= String "BlockTxSources"
        , pointFields point
        , "localFills" .= btsLocalFills sources
        , "mempool" .= btsMempool sources
        , "remaining" .= btsRemaining sources
        , foldMap summaryFields summary
        ]
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
   where
    arrivalFields (MkFetchArrivalBytes invalid evicted good extra) =
      mconcat
        ["invalidBytes" .= invalid, "evictedBytes" .= evicted, "goodBytes" .= good, "extraBytes" .= extra]
    summaryFields summary =
      "txCache"
        .= Aeson.object
          [ "tracked" .= ibsTracked summary
          , "acquired" .= ibsAcquired summary
          , "validated" .= ibsValidated summary
          , "txCount" .= ibsCacheTxCount summary
          , "load" .= ibsCacheLoad summary
          ]

  forHuman = \case
    TraceLeiosFetchClientBlockArrived peer point _ ->
      "Endorser block " <> pretty point <> " arrived from " <> showT peer
    TraceLeiosFetchClientBlockTxSources point sources summary ->
      "Endorser block "
        <> pretty point
        <> ": "
        <> showT (btsLocalFills sources)
        <> " txs from other points, "
        <> showT (btsMempool sources)
        <> " from the mempool, "
        <> showT (btsRemaining sources)
        <> " remaining"
        <> foldMap summaryHuman summary
    TraceLeiosFetchClientTxsArrived peer point _ ->
      "Transactions of endorser block " <> pretty point <> " arrived from " <> showT peer
    TraceLeiosFetchClientBlockAcquired point _ -> "Acquired endorser block " <> pretty point
    TraceLeiosFetchClientBlockTxsAcquired point _ ->
      "Acquired the transactions of endorser block " <> pretty point
   where
    pretty = Text.pack . prettyLeiosPoint
    summaryHuman summary =
      "; the LeiosTxCache already tracked "
        <> showT (ibsTracked summary)
        <> " of them, had acquired "
        <> showT (ibsAcquired summary)
        <> " and validated "
        <> showT (ibsValidated summary)
        <> ", and is at load "
        <> showT (ibsCacheLoad summary)

instance MetaTrace (TraceLeiosFetchClientEvent peer) where
  namespaceFor = \case
    TraceLeiosFetchClientBlockArrived{} -> Namespace [] ["BlockArrived"]
    TraceLeiosFetchClientBlockTxSources{} -> Namespace [] ["BlockTxSources"]
    TraceLeiosFetchClientTxsArrived{} -> Namespace [] ["TxsArrived"]
    TraceLeiosFetchClientBlockAcquired{} -> Namespace [] ["BlockAcquired"]
    TraceLeiosFetchClientBlockTxsAcquired{} -> Namespace [] ["BlockTxsAcquired"]

  severityFor (Namespace _ ["BlockArrived"]) _ = Just Debug
  severityFor (Namespace _ ["BlockTxSources"]) _ = Just Info
  severityFor (Namespace _ ["TxsArrived"]) _ = Just Debug
  severityFor (Namespace _ ["BlockAcquired"]) _ = Just Info
  severityFor (Namespace _ ["BlockTxsAcquired"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["BlockArrived"]) =
    Just
      "An endorser block arrived, its bytes classified by whether the node needed them. One the node writes is traced once the write settles."
  documentFor (Namespace _ ["BlockTxSources"]) =
    Just
      "How many of an endorser block's transactions the node copies from other points, takes from its mempool, and has yet to get, as it writes the body. If the LeiosTxCache took the body in then, also how many of them it already tracked, had acquired and had validated, and its load."
  documentFor (Namespace _ ["TxsArrived"]) =
    Just
      "Some of an endorser block's transactions arrived, their bytes classified by whether the node needed them. Ones the node writes are traced once the write settles."
  documentFor (Namespace _ ["BlockAcquired"]) =
    Just
      "The node acquired an endorser block it fetched; its age is how long after its point's slot began."
  documentFor (Namespace _ ["BlockTxsAcquired"]) =
    Just
      "The node acquired all the transactions of an endorser block it fetched; its age is how long after its point's slot began."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockArrived"]
    , Namespace [] ["BlockTxSources"]
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
      "Served endorser block " <> pretty point <> ", " <> showT bytes <> " bytes"
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

instance LogFormatting (AnyMessage (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx)) where
  forMachine _dtal (AnyMessageAndAgency _stok msg) = case msg of
    MsgLeiosBlockRequest (MkLeiosPoint slot ebHash) ->
      mconcat ["kind" .= String "MsgLeiosBlockRequest", ebPoint slot ebHash]
    MsgLeiosBlock eb@(Serialised bytes) ->
      mconcat
        [ "kind" .= String "MsgLeiosBlock"
        , "ebHash" .= ebHashValue (hashSerialisedEndorserBlock eb)
        , "ebBytesSize" .= BSL.length bytes
        ]
    MsgLeiosBlockTxsRequest (MkLeiosPoint slot ebHash) bitmaps ->
      mconcat
        [ "kind" .= String "MsgLeiosBlockTxsRequest"
        , ebPoint slot ebHash
        , "numTxs" .= sum (map (Bits.popCount . snd) bitmaps)
        , "bitmaps" .= map bitmapString bitmaps
        ]
    MsgLeiosBlockTxs txs ->
      mconcat
        [ "kind" .= String "MsgLeiosBlockTxs"
        , "numTxs" .= length txs
        , "txsBytesSize" .= sum (fmap (BS.length . leiosTxBytes) txs)
        ]
    MsgDone -> "kind" .= String "MsgDone"
   where
    ebHashValue :: EndorserBlockHash -> Value
    ebHashValue = String . Text.pack . hashToStringAsHex

    ebPoint :: SlotNo -> EndorserBlockHash -> Object
    ebPoint (SlotNo slot) ebHash = mconcat ["ebSlot" .= slot, "ebHash" .= ebHashValue ebHash]

    bitmapString :: (Word16, Word64) -> String
    bitmapString (idx, bits) =
      show idx <> ":0x" <> replicate (Bits.countLeadingZeros bits `div` 4) '0' <> showHex bits ""

instance MetaTrace (AnyMessage (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx)) where
  namespaceFor (AnyMessageAndAgency _stok msg) = case msg of
    MsgLeiosBlockRequest{} -> Namespace [] ["BlockRequest"]
    MsgLeiosBlock{} -> Namespace [] ["Block"]
    MsgLeiosBlockTxsRequest{} -> Namespace [] ["BlockTxsRequest"]
    MsgLeiosBlockTxs{} -> Namespace [] ["BlockTxs"]
    MsgDone -> Namespace [] ["Done"]

  severityFor _ _ = Just Debug

  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["BlockRequest"]
    , Namespace [] ["Block"]
    , Namespace [] ["BlockTxsRequest"]
    , Namespace [] ["BlockTxs"]
    , Namespace [] ["Done"]
    ]
