{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | CallTrace - Call trace utilities.
-- Lightweight call\/span tracing for instrumenting applications.
--
-- Use 'callTrace' to instrument your application and let it take care of
-- measurements and call stacks for amazing observability.
module LeiosUtils.CallTrace
  ( callTraceToObject
  , callTrace
  , CallId
  , CallName
  , ThreadName
  , CallCtx
  , CallTrace (..)
  , CallEvent (..)
  , CallInfo (..)
  , CallMeasure (..)
  , rootCallCtx
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (atomically)
  , StrictTVar
  , newTVar
  , readTVar
  , writeTVar
  )
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), diffTime)
import Control.Monad.Class.MonadTimer.SI (DiffTime)
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import Data.List (intercalate)
import Data.Word (Word64)

type CallId = Word64
type CallName = String
type ThreadName = String
type CallAncestors = [(CallId, CallName)]

-- | `CallInfo` holds a thread/name/id of a call and its parents' CallInfo.
data CallInfo = CallInfo
  { ciCallId :: CallId
  -- ^ Call identifier, unique amongst all `sibling` calls
  , ciCallParent :: Maybe CallInfo
  -- ^ Parent call info
  , ciCallName :: CallName
  -- ^ Call name, something like "module.name:function_name"
  , ciThread :: ThreadName
  -- ^ Thread name, something like a OS "thread id" or "forge"
  }
  deriving stock (Show, Eq)

data CallCtx m = CallCtx
  { ccCallInfo :: CallInfo
  , ccNextChildId :: StrictTVar m CallId
  }

-- | `CallTrace` denotes events that describe a Call's life, with its Argument of type `a` and a result of type `r`.
data CallTrace a r = CallTrace
  { ctCallInfo :: CallInfo
  , ctArgument :: a
  -- ^ Call argument (NOTE(bladyjoker): Was in `CallInfo a` but then I have to deal with existentials)
  , ctEvent :: CallEvent r
  -- ^ Start or End of a Call
  }
  deriving stock (Show, Eq)

-- TODO(bladyjoker): Add CallEmit for events that happen during the Call.
data CallEvent r
  = CallStart
  | CallEnd r CallMeasure
  deriving stock (Show, Eq)

data CallMeasure = CallMeasure
  { cmDuration :: DiffTime
  }
  deriving stock (Show, Eq)

callTraceToObject ::
  forall a r. (Aeson.ToJSON a, Aeson.ToJSON r) => CallTrace a r -> Aeson.Object
callTraceToObject ct =
  let
    eventObject = case ctEvent ct of
      CallStart ->
        [ "event" .= Aeson.String "Start"
        ]
      CallEnd result measure ->
        [ "event" .= Aeson.String "End"
        , "result" .= Aeson.toJSON result
        , "duration" .= Aeson.toJSON (cmDuration measure)
        ]
    ci = ctCallInfo ct
   in
    mconcat $
      [ "kind" .= Aeson.String "Call"
      , "id" .= ciCallId ci
      , "name" .= ciCallName ci
      , "thread" .= ciThread ci
      , "argument" .= (Aeson.toJSON . ctArgument $ ct)
      ]
        <> [ "call_name_stack" .= callNameStack ci
           , "call_id_stack" .= callIdStack ci
           ]
        <> eventObject

callTrace ::
  forall m a r.
  (MonadSTM m, MonadMonotonicTime m) =>
  -- | Tracing action
  (CallTrace a r -> m ()) ->
  -- | Parent context
  CallCtx m ->
  -- | Call thread
  ThreadName ->
  -- | CallName
  CallName ->
  -- | Call argument
  a ->
  -- | Continuation with the new call context (to be passed to children calls)
  (CallCtx m -> m r) ->
  m r
callTrace trace pctx thread cn arg action = do
  ctx <- childCallCtx pctx thread cn
  trace (CallTrace (ccCallInfo ctx) arg CallStart)
  (res, callMeasure) <- withMeasure (action ctx)
  trace (CallTrace (ccCallInfo ctx) arg (CallEnd res callMeasure))
  pure res

withMeasure :: MonadMonotonicTime m => m r -> m (r, CallMeasure)
withMeasure action = do
  before <- getMonotonicTime
  res <- action
  after <- getMonotonicTime
  return (res, CallMeasure{cmDuration = after `diffTime` before})

childCallCtx :: MonadSTM m => CallCtx m -> ThreadName -> CallName -> m (CallCtx m)
childCallCtx pctx thread cn = do
  cid <- atomically $ do
    n <- readTVar (ccNextChildId pctx)
    writeTVar (ccNextChildId pctx) (n + 1)
    pure n
  nextChildIdVar <- atomically $ newTVar 0
  let ci =
        CallInfo
          { ciCallId = cid
          , ciCallParent = Just $ ccCallInfo pctx
          , ciCallName = cn
          , ciThread = thread
          }
  return $
    CallCtx
      { ccCallInfo = ci
      , ccNextChildId = nextChildIdVar
      }

-- | Fresh top-level context to pass to the outermost 'callTrace' call.
rootCallCtx :: MonadSTM m => ThreadName -> m (CallCtx m)
rootCallCtx thread = do
  nextChildIdVar <- atomically $ newTVar 0

  return
    CallCtx
      { ccCallInfo =
          CallInfo
            { ciCallId = 0
            , ciCallParent = Nothing
            , ciCallName = ""
            , ciThread = thread
            }
      , ccNextChildId = nextChildIdVar
      }

-- | This call and its ancestors, nearest first.
callStack :: CallInfo -> CallAncestors
callStack ci = (ciCallId ci, ciCallName ci) : ancestors (ciCallParent ci)
 where
  ancestors Nothing = []
  ancestors (Just p) = (ciCallId p, ciCallName p) : ancestors (ciCallParent p)

callNameStack :: CallInfo -> String
callNameStack = intercalate " -> " . reverse . fmap snd . callStack

callIdStack :: CallInfo -> String
callIdStack = intercalate " -> " . reverse . fmap (show . fst) . callStack
