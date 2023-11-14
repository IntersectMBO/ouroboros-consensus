{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PeerSimulator.StateView (
    ChainSyncException (..)
  , StateView (..)
  , StateViewTracers (..)
  , defaultStateViewTracers
  , snapshotStateView
  ) where

import           Control.Tracer (Tracer)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Consensus.Util.IOLike (IOLike, SomeException)
import           Test.Consensus.PeerSimulator.Trace (terseFragH)
import           Test.Consensus.PointSchedule (PeerId, TestFragH)
import           Test.Util.Tracer (recordingTracerTVar)

-- | A record to associate an exception thrown by the ChainSync
-- thread with the peer that it was running for.
data ChainSyncException = ChainSyncException
       { csePeerId    :: PeerId
       , cseException :: SomeException
       }
    deriving Show

data StateView = StateView {
    svSelectedChain       :: TestFragH,
    svChainSyncExceptions :: [ChainSyncException]
  }
  deriving Show

instance Condense StateView where
  condense StateView {svSelectedChain, svChainSyncExceptions} =
    "final selection: " ++ terseFragH svSelectedChain ++ "\nerrors: " ++ show svChainSyncExceptions

-- | State view tracers are a lightweight mechanism to record information that
-- can later be used to produce a state view. This mechanism relies on
-- contra-tracers which we already use in a pervasives way.
data StateViewTracers m = StateViewTracers {
    svtChainSyncExceptionsTracer :: Tracer m ChainSyncException
  , svtGetChainSyncExceptions    :: m [ChainSyncException]
  }

-- | Make default state view tracers. The tracers are all freshly initialised
-- and contain no information.
defaultStateViewTracers ::
  IOLike m =>
  m (StateViewTracers m)
defaultStateViewTracers = do
  (svtChainSyncExceptionsTracer, svtGetChainSyncExceptions) <- recordingTracerTVar
  pure StateViewTracers {svtChainSyncExceptionsTracer, svtGetChainSyncExceptions}

-- | Use the state view tracers as well as some extra information to produce a
-- state view. This mostly consists in reading and storing the current state of
-- the tracers.
snapshotStateView ::
  IOLike m =>
  StateViewTracers m ->
  TestFragH ->
  m StateView
snapshotStateView StateViewTracers{svtGetChainSyncExceptions} svSelectedChain = do
  svChainSyncExceptions <- svtGetChainSyncExceptions
  pure StateView {svSelectedChain, svChainSyncExceptions}
