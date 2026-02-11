{-# LANGUAGE PatternSynonyms #-}

-- | Utility functions for enclosing a code segment with tracing events.
module Ouroboros.Consensus.Util.Enclose
  ( Enclosing
  , Enclosing' (..)
  , EnclosingTimed
  , encloseTimedWith
  , encloseWith
  , pattern FallingEdge
  ) where

import Control.Monad.Class.MonadTime.SI
  ( DiffTime
  , MonadMonotonicTime (..)
  , diffTime
  )
import Control.Tracer (Tracer, traceWith)

data Enclosing' a
  = -- | Preceding a specific code segment.
    RisingEdge
  | -- | Succeeding a specific code segment, with extra information.
    FallingEdgeWith !a
  deriving (Show, Eq, Ord)

type Enclosing = Enclosing' ()

pattern FallingEdge :: Enclosing' ()
pattern FallingEdge = FallingEdgeWith ()

{-# COMPLETE RisingEdge, FallingEdge #-}

-- | Enclose an action using the given 'Tracer'.
encloseWith ::
  Applicative m =>
  Tracer m Enclosing ->
  m a ->
  m a
encloseWith tracer action =
  traceWith tracer RisingEdge *> action <* traceWith tracer FallingEdge

type EnclosingTimed = Enclosing' DiffTime

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
