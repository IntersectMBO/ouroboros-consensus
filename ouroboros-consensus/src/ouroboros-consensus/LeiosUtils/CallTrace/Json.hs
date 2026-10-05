{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JSON-aware CallTrace API for production use.
--
-- Import this module in production code instead of "LeiosUtils.CallTrace"
-- whenever JSON serialisation is needed.  All names mirror the typed core
-- but work with an existential 'CallCtx' / 'CallTrace' that carries the
-- required 'Aeson.ToJSON' dictionaries.
module LeiosUtils.CallTrace.Json
  ( -- * Context
    CallCtx (..)
  , rootCallCtx
  , rootCallCtxWith
  , newCallCtx
  , newCallCtxWith

    -- * Tracing
  , CallTrace (..)
  , callTrace
  , callTraceVia

    -- * Serialisation
  , callTraceToObject

    -- * Re-exports from "LeiosUtils.CallTrace"
  , CallName
  , ThreadName
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import LeiosUtils.CallTrace
  ( CallName
  , ThreadInfo (..)
  , ThreadName
  , callId
  , callStack
  , callThreadId
  )
import qualified LeiosUtils.CallTrace as CT
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | An existential wrapper around 'CT.CallCtx t m' that retains the
-- 'Aeson.ToJSON t' dictionary.  Production code passes this around
-- instead of the polymorphic 'CT.CallCtx t m'.
data CallCtx m = forall t. Aeson.ToJSON t => CallCtx (CT.CallCtx t m)

-- | An existential wrapper around 'CT.CallTrace t a r' that retains
-- 'Aeson.ToJSON' dictionaries for all three type parameters.
data CallTrace
  = forall t a r.
    (Aeson.ToJSON t, Aeson.ToJSON a, Aeson.ToJSON r) =>
    CallTrace (CT.CallTrace t a r)

instance Eq CallTrace where
  ct1 == ct2 = callTraceToObject ct1 == callTraceToObject ct2

instance Show CallTrace where
  show = show . callTraceToObject

-- | Root context with no thread argument (thread arg is @()@).
rootCallCtx :: MonadSTM m => ThreadName -> m (CallCtx m)
rootCallCtx t = CallCtx <$> CT.rootCallCtx t ()

-- | Root context with an explicit thread argument.
rootCallCtxWith :: (MonadSTM m, Aeson.ToJSON t) => ThreadName -> t -> m (CallCtx m)
rootCallCtxWith t arg = CallCtx <$> CT.rootCallCtx t arg

-- | Branch onto a new child thread with no thread argument (@()@).
newCallCtx :: MonadSTM m => CallCtx m -> ThreadName -> m (CallCtx m)
newCallCtx (CallCtx ctx) t = CallCtx <$> CT.newCallCtx ctx t ()

-- | Like 'newCallCtx', but records an explicit thread argument.
newCallCtxWith :: (MonadSTM m, Aeson.ToJSON t) => CallCtx m -> ThreadName -> t -> m (CallCtx m)
newCallCtxWith (CallCtx ctx) t arg = CallCtx <$> CT.newCallCtx ctx t arg

-- | Like 'callTrace', but records @f r@ in the 'CallEnd' instead of @r@.
callTraceVia ::
  (IOLike m, Aeson.ToJSON a, Aeson.ToJSON r') =>
  (r -> r') ->
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> m r) ->
  m r
callTraceVia f tracer (CallCtx ctx) name arg action =
  CT.callTraceVia f (traceWith tracer . CallTrace) ctx name arg (action . CallCtx)

-- | Instrument a call: emit 'CallStart' / 'CallEnd' events via the tracer.
-- The result is not recorded (projected to @()@); use 'callTraceVia' to
-- record a projected result.
callTrace ::
  (IOLike m, Aeson.ToJSON a) =>
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> m r) ->
  m r
callTrace = callTraceVia (const ())

-- | Render a 'CallTrace' as an Aeson 'Object'.
callTraceToObject :: CallTrace -> Aeson.Object
callTraceToObject (CallTrace ct) =
  let
    eventObject = case CT.ctEvent ct of
      CT.CallStart ->
        ["event" .= Aeson.String "Start"]
      CT.CallEnd result measure ->
        [ "event" .= Aeson.String "End"
        , "result" .= Aeson.toJSON result
        , "duration" .= Aeson.toJSON (CT.cmDuration measure)
        , "allocations" .= Aeson.toJSON (CT.cmAllocations measure)
        ]
    ci = CT.ctCallInfo ct
   in
    mconcat $
      [ "kind" .= Aeson.String "Call"
      , "thread" .= tiName (NE.head (CT.ciThreadStack ci))
      , "thread_id" .= formatThreadIdPath ci
      , "thread_argument" .= Aeson.toJSON (CT.ctThreadArgument ct)
      , "name" .= CT.ciCallName ci
      , "stack" .= formatCallStack ci
      , "id" .= formatCallId ci
      , "child_id" .= CT.ciChildCallId ci
      , "parent_id" .= maybe "" formatCallId (CT.ciCallParent ci)
      , "argument" .= Aeson.toJSON (CT.ctCallArgument ct)
      ]
        <> eventObject
 where
  formatCallId = intercalate "." . fmap show . callId
  formatCallStack = intercalate " -> " . reverse . fmap CT.ciCallName . callStack
  formatThreadIdPath = intercalate "." . fmap show . callThreadId
