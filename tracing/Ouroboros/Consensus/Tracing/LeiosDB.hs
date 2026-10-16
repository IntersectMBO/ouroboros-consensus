{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How the LeiosDB's events are logged.
module Ouroboros.Consensus.Tracing.LeiosDB () where

import Cardano.Logging
import Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..), TraceLeiosDb (..))

instance LogFormatting TraceLeiosDb where
  forMachine _dtal = \case
    TraceLeiosDbInsertCollision table key ->
      mconcat ["kind" .= String "LeiosDbInsertCollision", "table" .= table, "key" .= key]
    TraceLeiosDbWriterQueueFull job ->
      mconcat ["kind" .= String "LeiosDbWriterQueueFull", "job" .= job]
    TraceLeiosDbWriteAbandoned job ->
      mconcat ["kind" .= String "LeiosDbWriteAbandoned", "job" .= job]
    TraceLeiosDbWriteJobDone job ->
      mconcat ["kind" .= String "LeiosDbWriteJobDone", "job" .= job]
    TraceLeiosDbStats LeiosDbStats{volatileEbs, immutableEbs, walBytes} ->
      mconcat
        [ "kind" .= String "LeiosDbStats"
        , "volatileEbs" .= volatileEbs
        , "immutableEbs" .= immutableEbs
        , "walBytes" .= walBytes
        ]
    TraceLeiosDbCopiedToImmutable copiedEbs ->
      mconcat ["kind" .= String "LeiosDbCopiedToImmutable", "copiedEbs" .= copiedEbs]
    TraceLeiosDbEvicted evictedEbs ->
      mconcat ["kind" .= String "LeiosDbEvicted", "evictedEbs" .= evictedEbs]
    TraceLeiosDbGCError reason ->
      mconcat ["kind" .= String "LeiosDbSweepError", "reason" .= reason]
    TraceLeiosDbCopyError ebHash reason ->
      mconcat ["kind" .= String "LeiosDbCopyError", "ebHash" .= ebHash, "reason" .= reason]

  forHuman = \case
    TraceLeiosDbInsertCollision table key ->
      "Ignored a row already in the LeiosDB's " <> Text.pack table <> " table: " <> Text.pack key
    TraceLeiosDbWriterQueueFull job ->
      "A LeiosDB write waited for room in the writer's queue: " <> Text.pack job
    TraceLeiosDbWriteAbandoned job ->
      "A LeiosDB write was lost while waiting for room in the writer's queue: " <> Text.pack job
    TraceLeiosDbWriteJobDone job ->
      "The LeiosDB writer ran " <> Text.pack job
    TraceLeiosDbStats LeiosDbStats{volatileEbs, immutableEbs, walBytes} ->
      "The LeiosDB holds "
        <> showT volatileEbs
        <> " EB rows in its volatile partition and "
        <> showT immutableEbs
        <> " EBs in its immutable partition, with a "
        <> showT walBytes
        <> " byte write-ahead log"
    TraceLeiosDbCopiedToImmutable copiedEbs ->
      "Copied " <> showT copiedEbs <> " EBs to the LeiosDB's immutable partition"
    TraceLeiosDbEvicted evictedEbs ->
      "Evicted " <> showT evictedEbs <> " EB rows from the LeiosDB's volatile partition"
    TraceLeiosDbGCError reason ->
      "A LeiosDB sweep pass failed and will be retried: " <> Text.pack reason
    TraceLeiosDbCopyError ebHash reason ->
      "Copying EB "
        <> Text.pack ebHash
        <> " to the LeiosDB's immutable partition failed and will be retried: "
        <> Text.pack reason

  asMetrics = \case
    TraceLeiosDbStats LeiosDbStats{volatileEbs, immutableEbs, walBytes} ->
      [ IntM "leiosDbVolatileEbs" (fromIntegral volatileEbs)
      , IntM "leiosDbImmutableEbs" (fromIntegral immutableEbs)
      , IntM "leiosDbWalBytes" walBytes
      ]
    TraceLeiosDbCopiedToImmutable copiedEbs ->
      [CounterM "leiosDbCopiedEbs" (Just copiedEbs)]
    TraceLeiosDbEvicted evictedEbs ->
      [CounterM "leiosDbEvictedEbs" (Just evictedEbs)]
    TraceLeiosDbGCError{} ->
      [CounterM "leiosDbSweepErrors" (Just 1)]
    TraceLeiosDbCopyError{} ->
      [CounterM "leiosDbCopyErrors" (Just 1)]
    TraceLeiosDbWriteAbandoned{} ->
      [CounterM "leiosDbWritesAbandoned" (Just 1)]
    TraceLeiosDbInsertCollision{} -> []
    TraceLeiosDbWriterQueueFull{} -> []
    TraceLeiosDbWriteJobDone{} -> []

instance MetaTrace TraceLeiosDb where
  namespaceFor = \case
    TraceLeiosDbInsertCollision{} -> Namespace [] ["InsertCollision"]
    TraceLeiosDbWriterQueueFull{} -> Namespace [] ["WriterQueueFull"]
    TraceLeiosDbWriteAbandoned{} -> Namespace [] ["WriteAbandoned"]
    TraceLeiosDbWriteJobDone{} -> Namespace [] ["WriteJobDone"]
    TraceLeiosDbStats{} -> Namespace [] ["Stats"]
    TraceLeiosDbCopiedToImmutable{} -> Namespace [] ["Copied"]
    TraceLeiosDbEvicted{} -> Namespace [] ["Evicted"]
    TraceLeiosDbGCError{} -> Namespace [] ["SweepError"]
    TraceLeiosDbCopyError{} -> Namespace [] ["CopyError"]

  severityFor (Namespace _ ["InsertCollision"]) _ = Just Info
  severityFor (Namespace _ ["WriterQueueFull"]) _ = Just Info
  severityFor (Namespace _ ["WriteAbandoned"]) _ = Just Warning
  severityFor (Namespace _ ["WriteJobDone"]) _ = Just Debug
  severityFor (Namespace _ ["Stats"]) _ = Just Info
  severityFor (Namespace _ ["Copied"]) _ = Just Info
  severityFor (Namespace _ ["Evicted"]) _ = Just Info
  severityFor (Namespace _ ["SweepError"]) _ = Just Warning
  severityFor (Namespace _ ["CopyError"]) _ = Just Warning
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["InsertCollision"]) =
    Just "An insert found its row already present, and left it as it was."
  documentFor (Namespace _ ["WriterQueueFull"]) =
    Just
      "A write had to wait for room in the writer's queue: the writer is the bottleneck, and every producer is now behind it."
  documentFor (Namespace _ ["WriteAbandoned"]) =
    Just
      "The submitting thread died while waiting for room in the writer's queue, so its write is lost."
  documentFor (Namespace _ ["WriteJobDone"]) =
    Just "The writer ran a job."
  documentFor (Namespace _ ["Stats"]) =
    Just "The sizes of the two partitions, sampled on a timer."
  documentFor (Namespace _ ["Copied"]) =
    Just "The copier committed EBs' closures to the immutable partition."
  documentFor (Namespace _ ["Evicted"]) =
    Just "A sweep pass evicted EB announcement rows from the volatile partition."
  documentFor (Namespace _ ["SweepError"]) =
    Just "A sweep pass failed. It is retried."
  documentFor (Namespace _ ["CopyError"]) =
    Just "Copying an EB failed. It stays pinned, so it is still the next one to copy."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["Stats"]) =
    [
      ( "leiosDbVolatileEbs"
      , "EB announcement rows in the volatile partition. An EB copied but not yet evicted counts in both partitions."
      )
    , ("leiosDbImmutableEbs", "EBs in the immutable partition")
    , ("leiosDbWalBytes", "The size of the volatile partition's write-ahead log, in bytes")
    ]
  metricsDocFor (Namespace _ ["Copied"]) =
    [("leiosDbCopiedEbs", "EBs copied into the immutable partition")]
  metricsDocFor (Namespace _ ["Evicted"]) =
    [("leiosDbEvictedEbs", "EB announcement rows evicted from the volatile partition")]
  metricsDocFor (Namespace _ ["SweepError"]) =
    [("leiosDbSweepErrors", "Failed sweep passes, each retried")]
  metricsDocFor (Namespace _ ["CopyError"]) =
    [("leiosDbCopyErrors", "Failed copies, each retried")]
  metricsDocFor (Namespace _ ["WriteAbandoned"]) =
    [("leiosDbWritesAbandoned", "Writes lost while waiting for room in the writer's queue")]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["InsertCollision"]
    , Namespace [] ["WriterQueueFull"]
    , Namespace [] ["WriteAbandoned"]
    , Namespace [] ["WriteJobDone"]
    , Namespace [] ["Stats"]
    , Namespace [] ["Copied"]
    , Namespace [] ["Evicted"]
    , Namespace [] ["SweepError"]
    , Namespace [] ["CopyError"]
    ]
