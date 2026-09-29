{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | CallTrace - Call trace utilities.
-- Lightweight call tracing for instrumenting applications.
--
-- Use 'callTrace' to instrument your application and let it take care of
-- measurements and call stacks for amazing observability.
module LeiosUtils.CallTrace
  ( callTrace
  , callTraceVia
  , ChildCallId
  , CallId
  , callId
  , callStack
  , callThreadId
  , CallName
  , ThreadName
  , ChildThreadId
  , CallCtx (..)
  , CallTrace (..)
  , CallEvent (..)
  , CallInfo (..)
  , CallMeasure (..)
  , rootCallCtx
  , MonadAllocationCounter (getAllocationCounter)
  , foldCallTrace
  , foldCallTraceFromInit
  , CallState (..)
  , newCallCtx
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
import Data.Int (Int64)
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

-- | Call context parameterised by the thread-arg type @t@.
data CallCtx t m = CallCtx
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

-- | `CallTrace` denotes events that describe a Call's life, with its Argument of type `a` and a result of type `r`.
data CallTrace t a r = CallTrace
  { ctCallInfo :: CallInfo
  , ctThreadArgument :: t
  , ctCallArgument :: a
  -- ^ Call argument
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

-- | Like 'callTrace', but the value recorded in the 'CallEnd' is @f res@
-- rather than @res@ itself -- useful when @res@ doesn't have a suitable
-- 'Show' instance (or you don't want to log all of it), but a projection
-- of it does. The returned value is still the real @res@, untouched.
callTraceVia ::
  forall m t a r r'.
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m) =>
  -- | Project the result to whatever is actually recorded in the trace
  (r -> r') ->
  -- | Tracing action
  (CallTrace t a r' -> m ()) ->
  -- | Parent context
  CallCtx t m ->
  -- | CallName
  CallName ->
  -- | Call argument
  a ->
  -- | Continuation with the new call context (to be passed to children calls)
  (CallCtx t m -> m r) ->
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
  CallCtx t m ->
  -- | CallName
  CallName ->
  -- | Call argument
  a ->
  -- | Continuation with the new call context (to be passed to children calls)
  (CallCtx t m -> m r) ->
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

childCallCtx :: MonadSTM m => CallCtx t m -> CallName -> m (CallCtx t m)
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
    CallCtx
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
-- Supply the thread argument @t@ (use @()@ when no argument is needed).
rootCallCtx :: MonadSTM m => ThreadName -> t -> m (CallCtx t m)
rootCallCtx thread arg = do
  (nextChildCallIdVar, nextChildThreadIdVar) <- atomically $ do
    c <- newTVar 0
    t <- newTVar 0
    pure (c, t)
  return $
    CallCtx
      { ccCallInfo = rootCallInfo thread
      , ccThreadName = thread
      , ccChildThreadId = 0
      , ccThreadArg = arg
      , ccNextChildCallId = nextChildCallIdVar
      , ccNextChildThreadId = nextChildThreadIdVar
      }

-- | Branch a context onto a new thread, minting a fresh 'ChildThreadId' from
-- the shared counter. The new context is a child of 'pctx' in the call tree
-- but carries a distinct thread name, ID, and argument.
-- Use @()@ for the argument when no thread argument is needed.
newCallCtx :: MonadSTM m => CallCtx t m -> ThreadName -> s -> m (CallCtx s m)
newCallCtx pctx thisThreadName thisThreadArg = do
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
