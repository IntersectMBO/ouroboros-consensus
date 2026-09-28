{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , SomeJsonCallTrace (..)
  , callTrace
  , callTraceVia
  , ChildCallId
  , CallId
  , callId
  , CallName
  , ThreadName
  , ChildThreadId
  , CallCtxWith (..)
  , CallCtx
  , CallCtxWithJson (..)
  , CallTrace (..)
  , CallEvent (..)
  , CallInfo (..)
  , CallMeasure (..)
  , rootCallCtx
  , rootCallCtxWith
  , MonadAllocationCounter (getAllocationCounter)
  , foldCallTrace
  , foldCallTraceFromInit
  , CallState (..)
  , newCallCtx
  , newCallCtxWith
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
import qualified Data.Set as Set
import Data.Word (Word64)
import qualified GHC.Conc.Sync as IO

type ChildCallId = Word64
type CallId = [ChildCallId]
type CallName = String
type ThreadName = String
type ChildThreadId = Word64

-- | `CallInfo` holds a thread/name/local id of a call and its parents' CallInfo.
data CallInfo = CallInfo
  { ciChildCallId :: ChildCallId
  -- ^ Call child/local identifier, unique amongst all `sibling` calls
  -- `callId` forms a globally unique identifier
  , ciCallParent :: Maybe CallInfo
  -- ^ Parent call info
  , ciCallName :: CallName
  -- ^ Logical call name, should be unique amongst names (ie. like a fully qualified name)
  , ciThreadName :: ThreadName
  -- ^ Logical thread name (ie. like "Forge")
  , ciChildThreadId :: ChildThreadId
  -- ^ Unique identifier for the thread instance; distinguishes concurrent
  -- threads that share the same 'ciThreadName'
  }
  deriving stock (Show, Eq)

-- | Call context parameterised by the thread-arg type @t@. Use the
-- 'CallCtx' synonym when no thread arg is needed.
data CallCtxWith t m = CallCtxWith
  { ccCallInfo :: CallInfo
  , ccThreadName :: ThreadName
  -- ^ Execution thread inherited by child calls. Separate from 'ciThread' in
  -- 'ccCallInfo', which records the thread this call itself ran on and must
  -- not be mutated (it is embedded as 'ciCallParent' in every child forever).
  , ccChildThreadId :: ChildThreadId
  , ccThreadArg :: t
  -- ^ Caller-supplied semantic context for the thread instance (e.g. peer
  -- address, credential label). Carried in the context; not embedded in 'CallInfo'.
  , ccNextChildCallId :: StrictTVar m ChildCallId
  , ccNextChildThreadId :: StrictTVar m ChildThreadId
  -- ^ Shared counter for minting unique thread IDs within this hierarchy.
  }

-- | 'CallCtxWith' with no thread arg — backward-compatible alias for existing
-- call sites that do not supply a thread argument.
type CallCtx m = CallCtxWith () m

-- | Erase the thread-arg type, retaining only the ability to serialise it as
-- JSON — analogous to 'SomeJsonCallTrace'.
data CallCtxWithJson m
  = forall t. Aeson.ToJSON t => CallCtxWithJson (CallCtxWith t m)

-- | `CallTrace` denotes events that describe a Call's life, with its Argument of type `a` and a result of type `r`.
data CallTrace t a r = CallTrace
  { ctCallInfo :: CallInfo
  , ctThreadArgument :: t
  , ctCallArgument :: a
  -- ^ Call argument (NOTE(bladyjoker): Was in `CallInfo a` but then I have to deal with existentials)
  , ctEvent :: CallEvent r
  -- ^ Start or End of a Call
  }
  deriving stock (Show, Eq)

-- TODO(bladyjoker): Add CallEmit e for events that happen during the Call.
data CallEvent r
  = CallStart
  | CallEnd r !CallMeasure
  deriving stock (Show, Eq)

data CallMeasure = CallMeasure
  { cmDuration :: !DiffTime
  , cmAllocations :: !Int64
  }
  deriving stock (Show, Eq, Ord)

instance Monoid CallMeasure where
  mempty = CallMeasure 0 0

instance Semigroup CallMeasure where
  (CallMeasure d a) <> (CallMeasure d' a') = CallMeasure (d + d') (a + a')

-- TODO(bladyjoker): This needs to be tested too, probably need callTraceFromObject and use the same testsuite
callTraceToObject ::
  forall t a r. (Aeson.ToJSON t, Aeson.ToJSON a, Aeson.ToJSON r) => CallTrace t a r -> Aeson.Object
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
      , "thread" .= ciThreadName ci
      , "thread_id" .= formatThreadId ci
      , "thread_argument" .= (Aeson.toJSON . ctThreadArgument $ ct)
      , "name" .= ciCallName ci
      , "stack" .= formatCallStack ci
      , "id" .= formatCallId ci
      , "child_id" .= ciChildCallId ci
      , "parent_id" .= maybe "" formatCallId (ciCallParent ci)
      , "argument" .= (Aeson.toJSON . ctCallArgument $ ct)
      ]
        <> eventObject
 where
  formatCallId = intercalate "." . fmap show . callId
  formatCallStack = intercalate " -> " . reverse . fmap ciCallName . callStack
  formatThreadId = intercalate "." . fmap show . callThreadId

-- | A 'CallTrace' with its argument and result types packed away, retaining
-- only the ability to render it as JSON via 'callTraceToObject'.
--
-- 'Eq' and 'Show' can't be derived for this type -- the packed-away @a@/@r@
-- are existential, so there's no way to derive structural equality or
-- showsPrec across two values that may hide different types. Instead both
-- instances go via 'callTraceToObject': 'Aeson.Object' has real 'Eq'/'Show'
-- instances, so two calls compare equal iff their rendered JSON does.
data SomeJsonCallTrace
  = forall t a r. (Aeson.ToJSON t, Aeson.ToJSON a, Aeson.ToJSON r) => SomeJsonCallTrace (CallTrace t a r)

instance Eq SomeJsonCallTrace where
  SomeJsonCallTrace ct1 == SomeJsonCallTrace ct2 =
    callTraceToObject ct1 == callTraceToObject ct2

instance Show SomeJsonCallTrace where
  show (SomeJsonCallTrace ct) = show (callTraceToObject ct)

-- | Like 'callTrace', but the value recorded in the 'CallEnd' is @f res@
-- rather than @res@ itself -- useful when @res@ doesn't have a suitable
-- 'Aeson.ToJSON'\/'Show' instance (or you don't want to log all of it), but
-- a projection of it does. The returned value is still the real @res@,
-- untouched.
callTraceVia ::
  forall m t a r r'.
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m) =>
  -- | Project the result to whatever is actually recorded in the trace
  (r -> r') ->
  -- | Tracing action
  (CallTrace t a r' -> m ()) ->
  -- | Parent context
  CallCtxWith t m ->
  -- | CallName
  CallName ->
  -- | Call argument
  a ->
  -- | Continuation with the new call context (to be passed to children calls)
  (CallCtxWith t m -> m r) ->
  m r
callTraceVia f trace pctx cn arg action = do
  ctx <- childCallCtx pctx cn
  trace (CallTrace (ccCallInfo ctx) (ccThreadArg ctx) arg CallStart)
  (res, callMeasure) <- withMeasure (action ctx)
  trace (CallTrace (ccCallInfo ctx) (ccThreadArg ctx) arg (CallEnd (f res) callMeasure))
  pure res

callTrace ::
  forall m t a r.
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m) =>
  -- | Tracing action
  (CallTrace t a r -> m ()) ->
  -- | Parent context
  CallCtxWith t m ->
  -- | CallName
  CallName ->
  -- | Call argument
  a ->
  -- | Continuation with the new call context (to be passed to children calls)
  (CallCtxWith t m -> m r) ->
  m r
callTrace = callTraceVia id

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

childCallCtx :: MonadSTM m => CallCtxWith t m -> CallName -> m (CallCtxWith t m)
childCallCtx pctx cn = do
  (cid, nextChildCallIdVar) <- atomically $ do
    n <- readTVar (ccNextChildCallId pctx)
    writeTVar (ccNextChildCallId pctx) (n + 1)
    nextChildCallIdVar <- newTVar 0
    pure (n, nextChildCallIdVar)
  let ci =
        CallInfo
          { ciChildCallId = cid
          , ciCallParent = Just $ ccCallInfo pctx
          , ciCallName = cn
          , ciThreadName = ccThreadName pctx
          , ciChildThreadId = ccChildThreadId pctx
          }
  return $
    CallCtxWith
      { ccCallInfo = ci
      , ccThreadName = ccThreadName pctx
      , ccChildThreadId = ccChildThreadId pctx
      , ccThreadArg = ccThreadArg pctx
      , ccNextChildCallId = nextChildCallIdVar
      , ccNextChildThreadId = ccNextChildThreadId pctx
      }

rootCallInfo :: ThreadName -> CallInfo
rootCallInfo thread =
  CallInfo
    { ciChildCallId = 0
    , ciCallParent = Nothing
    , ciCallName = ""
    , ciThreadName = thread
    , ciChildThreadId = 0
    }

-- | Fresh top-level context to pass to the outermost 'callTrace' call.
rootCallCtx :: MonadSTM m => ThreadName -> m (CallCtx m)
rootCallCtx thread = rootCallCtxWith thread ()

-- | Like 'rootCallCtx' but supplies a thread argument.
rootCallCtxWith :: MonadSTM m => ThreadName -> t -> m (CallCtxWith t m)
rootCallCtxWith thread arg = do
  (nextChildCallIdVar, nextChildThreadIdVar) <- atomically $ do
    c <- newTVar 0
    t <- newTVar 0
    pure (c, t)
  return $
    CallCtxWith
      { ccCallInfo = rootCallInfo thread
      , ccThreadName = thread
      , ccChildThreadId = 0
      , ccThreadArg = arg
      , ccNextChildCallId = nextChildCallIdVar
      , ccNextChildThreadId = nextChildThreadIdVar
      }

-- | Branch a context onto a new thread, minting a fresh 'ThreadId' from the
-- shared counter. The new context is a child of 'pctx' in the call tree but
-- carries a distinct thread name and ID. The thread arg is reset to @()@; use
-- 'newCallCtxWith' to supply a custom arg.
newCallCtx :: MonadSTM m => CallCtxWith t m -> ThreadName -> m (CallCtx m)
newCallCtx pctx thread = newCallCtxWith pctx thread ()

-- | Like 'newCallCtx' but supplies a thread argument.
newCallCtxWith :: MonadSTM m => CallCtxWith t m -> ThreadName -> s -> m (CallCtxWith s m)
newCallCtxWith pctx thisThreadName thisThreadArg = do
  (thisThreadId, nextChildThreadIdVar) <- atomically $ do
    n <- readTVar (ccNextChildThreadId pctx)
    writeTVar (ccNextChildThreadId pctx) (n + 1)
    nextChildThreadIdVar <- newTVar 0
    pure (n, nextChildThreadIdVar)
  return $
    pctx
      { ccThreadName = thisThreadName
      , ccChildThreadId = thisThreadId
      , ccThreadArg = thisThreadArg
      , ccNextChildThreadId = nextChildThreadIdVar
      }

-- | `callStack` without `root`
callStack :: CallInfo -> [CallInfo]
callStack ci = case ciCallParent ci of
  Nothing -> []
  Just parCi -> ci : callStack parCi

-- `callId` is a globally unique Call identifier
callId :: CallInfo -> CallId
callId = reverse . fmap ciChildCallId . callStack

-- | The thread-ID path from root to the current call: consecutive equal IDs
-- are collapsed, so each entry marks a thread boundary. Analogous to 'callId'.
callThreadId :: CallInfo -> [ChildThreadId]
callThreadId = go []
 where
  go acc ci =
    let acc' = case acc of
          (x : _) | x == ciChildThreadId ci -> acc
          _ -> ciChildThreadId ci : acc
     in case ciCallParent ci of
          Nothing -> acc'
          Just par -> go acc' par

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

type CallTraceError t a r = (String, CallTrace t a r)

foldCallTraceFromInit :: [CallTrace t a r] -> Either (CallTraceError t a r) CallState
foldCallTraceFromInit t = foldCallTrace t initCallState

-- TODO(bladyjoker): Add `csMissingParents` for calls that start but parents are not in `csActiveCalls`
-- TODO(bladyjoker): Add `csMissingStart` for calls that end but they are not in `csActiveCalls`
-- TODO(bladyjoker): Add threading model, for example, if a parent and children are in the same thread, then parent call must contain all others (and measure have to align).
foldCallTrace :: [CallTrace t a r] -> CallState -> Either (CallTraceError t a r) CallState
foldCallTrace = flip (foldM foldFn)
 where
  foldFn st@CallState{..} ct@CallTrace{..} =
    let
      cid = callId ctCallInfo
      may `errN` err = maybe (Left (err, ct)) Right may
      b `errB` err = if b then Left (err, ct) else Right ()
     in
      case ctEvent of
        CallStart -> do
          (cid `Set.member` (Map.keysSet csInactiveCalls `Set.union` Map.keysSet csActiveCalls))
            `errB` "Call Id must be unique over all time"
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
          startCi <- Map.lookup cid csActiveCalls `errN` "Ending a Call that is not active"
          ( ciThreadName startCi /= ciThreadName ctCallInfo
              || ciChildThreadId startCi /= ciChildThreadId ctCallInfo
            )
            `errB` "Call ended on a different thread than it started"
          return
            st
              { csActiveCalls = Map.delete cid csActiveCalls
              , csInactiveCalls = Map.insert cid ctCallInfo csInactiveCalls
              , csTotalMeasure = csTotalMeasure <> cm
              }
