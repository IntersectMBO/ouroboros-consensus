{-# LANGUAGE PatternSynonyms #-}

-- | Utility functions for enclosing a code segment with tracing events.
module Ouroboros.Consensus.Util.Enclose
  ( Enclosing' (..)

    -- * Simple usage
  , Enclosing
  , pattern FallingEdge
  , encloseWith

    -- * Timing
  , EnclosingTimed
  , encloseTimedWith
  ) where

import Control.Monad.Class.MonadTime.SI
  ( DiffTime
  , MonadMonotonicTime (..)
  , diffTime
  )
import Control.Tracer (Tracer, traceWith)

-- $setup
-- >>> import Control.Tracer
-- >>> import Data.Functor.Contravariant
-- >>> import Ouroboros.Consensus.Util.IOLike

-- | Mini-abstraction for tracing of specific code segment, emitting events just
-- before and just after.
--
-- Usage example:
--
-- >>> :{
-- data TraceEvent
--   = TraceFoo String
--   | TraceComputed
--       -- | Input of the computation.
--       Int
--       -- | Either 'RisingEdge', or 'FallingEdgeWith' with the result.
--       (Enclosing' Int)
-- :}
--
-- >>> :{
-- work :: Monad m => Tracer m TraceEvent -> Int -> m Int
-- work tracer input = do
--   traceWith compTracer RisingEdge
--   output <- pure (input + 5)
--   traceWith compTracer $ FallingEdgeWith output
--   pure output
--   where
--     compTracer = TraceComputed input >$< tracer
-- :}
data Enclosing' a
  = -- | Preceding a specific code segment.
    RisingEdge
  | -- | Succeeding a specific code segment, with extra information.
    FallingEdgeWith !a
  deriving (Show, Eq, Ord)

-- | 'Enclosing'', but without extra data in the 'FallingEdge' case.
type Enclosing = Enclosing' ()

pattern FallingEdge :: Enclosing' ()
pattern FallingEdge = FallingEdgeWith ()

{-# COMPLETE RisingEdge, FallingEdge #-}

-- | Enclose an action using the given 'Tracer'.
--
-- >>> :{
-- data TraceEvent
--   = TraceFoo String
--   | TraceComputed
--       -- | Input of the computation.
--       Int
--       -- | Either 'RisingEdge' or 'FallingEdge'.
--       Enclosing
-- :}
--
-- >>> :{
-- work :: Monad m => Tracer m TraceEvent -> Int -> m Int
-- work tracer input =
--   encloseWith (TraceComputed input >$< tracer) $
--     pure (input + 5)
-- :}
encloseWith ::
  Applicative m =>
  Tracer m Enclosing ->
  m a ->
  m a
encloseWith tracer action =
  traceWith tracer RisingEdge *> action <* traceWith tracer FallingEdge

-- | 'Enclosing'', but with the elapsed time in the 'FallingEdge' case.
type EnclosingTimed = Enclosing' DiffTime

-- | Enclose an action and trace its elapsed time.
--
-- >>> :{
-- data TraceEvent
--   = TraceFoo String
--   | TraceComputed
--       -- | Input of the computation.
--       Int
--       -- | Either 'RisingEdge' or 'FallingEdge'.
--       EnclosingTimed
-- :}
--
-- >>> :{
-- work :: MonadMonotonicTime m => Tracer m TraceEvent -> Int -> m Int
-- work tracer input =
--   encloseTimedWith (TraceComputed input >$< tracer) $
--     pure (input + 5)
-- :}
encloseTimedWith ::
  MonadMonotonicTime m =>
  Tracer m EnclosingTimed ->
  m a ->
  m a
encloseTimedWith tracer action = do
  before <- getMonotonicTime
  traceWith tracer RisingEdge
  res <- action
  after <- getMonotonicTime
  traceWith tracer (FallingEdgeWith (after `diffTime` before))
  pure res
