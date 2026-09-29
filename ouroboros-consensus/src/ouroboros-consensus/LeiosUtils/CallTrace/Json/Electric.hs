{-# LANGUAGE ExistentialQuantification #-}

-- | JSON-aware 'callTrace' / 'callTraceVia' variants for
-- 'Ouroboros.Consensus.Util.Electric'.
module LeiosUtils.CallTrace.Json.Electric
  ( callTrace
  , callTraceVia
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import LeiosUtils.CallTrace (CallName, MonadAllocationCounter)
import qualified LeiosUtils.CallTrace as CT
import LeiosUtils.CallTrace.Json (CallCtx (..), CallTrace (..))
import Ouroboros.Consensus.Util (Electric, electric, runElectric)

-- | Like 'LeiosUtils.CallTrace.Json.callTraceVia', but for actions in
-- 'Electric'.
callTraceVia ::
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m, Aeson.ToJSON a, Aeson.ToJSON r') =>
  (r -> r') ->
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> Electric m r) ->
  Electric m r
callTraceVia f tracer (CallCtx ctx) name arg action =
  electric $
    CT.callTraceVia f (traceWith tracer . CallTrace) ctx name arg (runElectric . action . CallCtx)

-- | Like 'LeiosUtils.CallTrace.Json.callTrace', but for actions in
-- 'Electric'.
callTrace ::
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m, Aeson.ToJSON a) =>
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> Electric m r) ->
  Electric m r
callTrace = callTraceVia (const ())
