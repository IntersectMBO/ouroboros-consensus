-- | Tracing configuration.
--
-- Tracing is owned by the node's tracing system (hermod / @trace-dispatcher@),
-- not by @cardano-config@. The keys below are accepted but parsed /opaquely/:
-- their contents are neither interpreted nor validated here. The authoritative
-- schema for them lives in the @trace-dispatcher@ package.
--
-- This type exists as an /informational placeholder/, so that the tracing keys
-- are visible in the configuration schema (rather than silently ignored) and
-- are preserved when round-tripping a configuration through the parser.
module Cardano.Configuration.File.Tracing
  ( TracingConfiguration (..)
  ) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A node accepts tracing configuration in two mutually-exclusive shapes:
--
--   * the /legacy flat form/, with @TraceOptions@ and the @TraceOption*@ keys
--     directly at the top level of the configuration (this is what current
--     configuration files use); or
--
--   * a single @HermodTracing@ key, whose value is either an inline object or a
--     path (a string) to a separate file holding that object.
--
-- All fields are optional and captured opaquely; see the module documentation.
data TracingConfiguration = TracingConfiguration
  { hermodTracing :: Maybe (Either Text Value)
  -- ^ The new outer form: an inline object, or a path to a file holding it.
  , traceOptions :: Maybe Value
  , traceOptionForwarder :: Maybe Value
  , traceOptionNodeName :: Maybe Value
  , traceOptionMetricsPrefix :: Maybe Value
  , traceOptionResourceFrequency :: Maybe Value
  , traceOptionLedgerMetricsFrequency :: Maybe Value
  , tracePrometheusSimpleRun :: Maybe Value
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec TracingConfiguration)

instance HasCodec TracingConfiguration where
  codec =
    object "TracingConfiguration" $
      TracingConfiguration
        <$> optionalFieldWith
          "HermodTracing"
          (eitherCodec (codec @Text) (codec @Value))
          ("Tracing configuration as an inline object, or a path to a separate file holding it. " <> ownedNote)
          .= hermodTracing
        <*> opaque "TraceOptions" "Per-namespace tracing options (severity, detail, backends, frequency)." .= traceOptions
        <*> opaque "TraceOptionForwarder" "Trace forwarder configuration." .= traceOptionForwarder
        <*> opaque "TraceOptionNodeName" "Human-readable node name used in traces." .= traceOptionNodeName
        <*> opaque "TraceOptionMetricsPrefix" "Prefix applied to emitted metrics." .= traceOptionMetricsPrefix
        <*> opaque "TraceOptionResourceFrequency" "Resource-trace frequency, in milliseconds." .= traceOptionResourceFrequency
        <*> opaque "TraceOptionLedgerMetricsFrequency" "Ledger-metrics frequency, in milliseconds." .= traceOptionLedgerMetricsFrequency
        <*> opaque "TracePrometheusSimpleRun" "Overrides for the simple Prometheus endpoint's DoS protection." .= tracePrometheusSimpleRun
   where
    -- All tracing keys are accepted but stored as opaque 'Value's: the
    -- authoritative schema lives in trace-dispatcher, so we only record that
    -- the key exists and what it is for.
    opaque :: Text -> Text -> ObjectCodec (Maybe Value) (Maybe Value)
    opaque name desc = optionalField name (desc <> " " <> ownedNote)

    ownedNote :: Text
    ownedNote = "Consumed by the node tracing system (trace-dispatcher), not parsed or validated by cardano-config."
