{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | CallTrace - Call trace utilities.
-- Lightweight call\/span tracing for instrumenting applications.
--
-- Use 'callTrace' to instrument your application and let it take care of
-- measurements and call stacks for amazing observability.
module LeiosUtils.CallTrace
  ( callTraceToObject
  , callTrace
  , CallLocalId
  , CallId
  , callId
  , CallName
  , ThreadName
  , CallCtx
  , CallTrace (..)
  , CallEvent (..)
  , CallInfo (..)
  , CallMeasure (..)
  , rootCallCtx
  , MonadAllocationCounter (getAllocationCounter)
  , foldCallTrace
  , foldCallTraceFromInit
  , CallState (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (atomically)
  , StrictTVar
  , newTVar
  , readTVar
  , writeTVar
  )
import Control.Monad (foldM, void, when)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), diffTime)
import Control.Monad.Class.MonadTimer.SI (DiffTime)
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Word (Word64)
import qualified GHC.Conc.Sync as IO

type CallLocalId = Word64
type CallId = [CallLocalId]
type CallName = String
type ThreadName = String

-- | `CallInfo` holds a thread/name/local id of a call and its parents' CallInfo.
data CallInfo = CallInfo
  { ciCallLocalId :: CallLocalId
  -- ^ Call local identifier, unique amongst all `sibling` calls
  -- `callId` forms a globally unique identifier
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
  , ccNextChildId :: StrictTVar m CallLocalId
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

-- TODO(bladyjoker): Add CallEmit e for events that happen during the Call.
data CallEvent r
  = CallStart
  | CallEnd r CallMeasure
  deriving stock (Show, Eq)

data CallMeasure = CallMeasure
  { cmDuration :: DiffTime
  , cmAllocations :: Int64
  }
  deriving stock (Show, Eq, Ord)

instance Monoid CallMeasure where
  mempty = CallMeasure 0 0

instance Semigroup CallMeasure where
  (CallMeasure d a) <> (CallMeasure d' a') = CallMeasure (d + d') (a + a')

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
        , "allocations" .= Aeson.toJSON (cmAllocations measure)
        ]
    ci = ctCallInfo ct
   in
    mconcat $
      [ "kind" .= Aeson.String "Call"
      , "id" .= intercalate "." (fmap show (callId ci))
      , "local_id" .= ciCallLocalId ci
      , "name_stack" .= intercalate " -> " (ancestorNames ci)
      , "name" .= ciCallName ci
      , "thread" .= ciThread ci
      , "argument" .= (Aeson.toJSON . ctArgument $ ct)
      ]
        <> eventObject

callTrace ::
  forall m a r.
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m) =>
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

withMeasure :: (MonadMonotonicTime m, MonadAllocationCounter m) => m r -> m (r, CallMeasure)
withMeasure action = do
  beforeTime <- getMonotonicTime
  beforeAlloc <- getAllocationCounter
  res <- action
  afterTime <- getMonotonicTime
  afterAlloc <- getAllocationCounter
  return
    ( res
    , CallMeasure
        { cmDuration = afterTime `diffTime` beforeTime
        , cmAllocations = beforeAlloc - afterAlloc
        }
    )

childCallCtx :: MonadSTM m => CallCtx m -> ThreadName -> CallName -> m (CallCtx m)
childCallCtx pctx thread cn = do
  cid <- atomically $ do
    n <- readTVar (ccNextChildId pctx)
    writeTVar (ccNextChildId pctx) (n + 1)
    pure n
  nextChildIdVar <- atomically $ newTVar 0
  let ci =
        CallInfo
          { ciCallLocalId = cid
          , ciCallParent = Just $ ccCallInfo pctx
          , ciCallName = cn
          , ciThread = thread
          }
  return $
    CallCtx
      { ccCallInfo = ci
      , ccNextChildId = nextChildIdVar
      }

rootCallInfo :: ThreadName -> CallInfo
rootCallInfo thread =
  CallInfo
    { ciCallLocalId = 0
    , ciCallParent = Nothing
    , ciCallName = ""
    , ciThread = thread
    }

-- | Fresh top-level context to pass to the outermost 'callTrace' call.
rootCallCtx :: MonadSTM m => ThreadName -> m (CallCtx m)
rootCallCtx thread = do
  nextChildIdVar <- atomically $ newTVar 0

  return
    CallCtx
      { ccCallInfo = rootCallInfo thread
      , ccNextChildId = nextChildIdVar
      }

-- `callAncestors` without `root`
callAncestors :: CallInfo -> [CallInfo]
callAncestors ci = case ciCallParent ci of
  Nothing -> []
  Just parCi -> if isJust (ciCallParent parCi) then parCi : callAncestors parCi else []

ancestorNames :: CallInfo -> [CallName]
ancestorNames = reverse . fmap ciCallName . callAncestors

-- `callId` is a globally unique Call identifier
callId :: CallInfo -> CallId
callId ci = reverse . fmap (ciCallLocalId) $ (ci : callAncestors ci)

-- | Allocation measurements machinery
class Monad m => MonadAllocationCounter m where
  getAllocationCounter :: m (Int64)

instance MonadAllocationCounter IO where
  getAllocationCounter = IO.getAllocationCounter

-- | CallTrace Model
data CallState = CallState
  { csActiveCalls :: Map CallId CallInfo
  , csInactiveCalls :: Map CallId CallInfo
  , csTotalMeasure :: CallMeasure
  }
  deriving stock (Show, Eq)

initCallState :: CallState
initCallState = CallState mempty mempty mempty

type CallTraceError r a = (String, CallTrace r a)

foldCallTraceFromInit :: [CallTrace r a] -> Either (CallTraceError r a) CallState
foldCallTraceFromInit t = foldCallTrace t initCallState

-- TODO(bladyjoker): Add `csMissingParents` for calls that start but parents are not in `csActiveCalls`
-- TODO(bladyjoker): Add `csMissingStart` for calls that end but they are not in `csActiveCalls`
-- TODO(bladyjoker): Add threading model, for example, if a parent and children are in the same thread, then parent call must contain all others (and measure have to align).
foldCallTrace :: [CallTrace r a] -> CallState -> Either (CallTraceError r a) CallState
foldCallTrace = flip (foldM foldFn)
 where
  foldFn st@CallState{..} ct@CallTrace{..} =
    let
      cid = callId ctCallInfo
      may `errN` err = maybe (Left (err, ct)) Right may
      may `errJ` err = maybe (Right ()) (\_ -> Left (err, ct)) may
     in
      case ctEvent of
        CallStart -> do
          _ci <- Map.lookup cid csActiveCalls `errJ` "Starting a Call that is already active"
          parCi <-
            ciCallParent ctCallInfo `errN` "Starting a root Call! Only root Call has no parent"
          when (isJust (ciCallParent parCi)) $
            void $
              Map.lookup (callId parCi) csActiveCalls
                `errN` "Starting a Call but the (non-root) parent is not active"
          return
            st
              { csActiveCalls = Map.insert cid ctCallInfo csActiveCalls
              }
        CallEnd _res cm -> do
          _ <- Map.lookup cid csActiveCalls `errN` "Ending a Call that is not active"
          return
            st
              { csActiveCalls = Map.delete cid csActiveCalls
              , csInactiveCalls = Map.insert cid ctCallInfo csInactiveCalls
              , csTotalMeasure = csTotalMeasure <> cm
              }
