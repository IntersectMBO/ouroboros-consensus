{-# LANGUAGE ExistentialQuantification #-}

-- | JSON-aware 'callTrace' / 'callTraceVia' variants for
-- 'Ouroboros.Consensus.Util.EarlyExit.WithEarlyExit'.
module LeiosUtils.CallTrace.Json.EarlyExit
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
import Ouroboros.Consensus.Util.EarlyExit
  ( WithEarlyExit
  , earlyExitFromMaybe
  , withEarlyExit
  )

-- | Like 'LeiosUtils.CallTrace.Json.callTraceVia', but for actions in
-- 'WithEarlyExit'.  On early exit the traced result is 'Nothing'.
callTraceVia ::
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m, Aeson.ToJSON a, Aeson.ToJSON r') =>
  (r -> r') ->
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> WithEarlyExit m r) ->
  WithEarlyExit m r
callTraceVia f tracer (CallCtx ctx) name arg action =
  earlyExitFromMaybe $
    CT.callTraceVia
      (fmap f)
      (traceWith tracer . CallTrace)
      ctx
      name
      arg
      (withEarlyExit . action . CallCtx)

-- | Like 'LeiosUtils.CallTrace.Json.callTrace', but for actions in
-- 'WithEarlyExit'.
callTrace ::
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m, Aeson.ToJSON a) =>
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> WithEarlyExit m r) ->
  WithEarlyExit m r
callTrace = callTraceVia (const ())
