{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PeerSimulator.StateView (
    ChainSyncException (..)
  , StateView (..)
  , StateViewTracers (..)
  , chainSyncKilled
  , defaultStateViewTracers
  , snapshotStateView
  ) where

import           Control.Exception (AsyncException (ThreadKilled),
                     fromException)
import           Control.Tracer (Tracer)
import           Data.Maybe (mapMaybe)
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Consensus.Util.IOLike (IOLike, SomeException,
                     atomically)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.TersePrinting (terseHFragment)
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.Tracer (recordingTracerTVar)

-- | A record to associate an exception thrown by the ChainSync
-- thread with the peer that it was running for.
data ChainSyncException = ChainSyncException
       { csePeerId    :: PeerId
       , cseException :: SomeException
       }
    deriving Show

instance Condense ChainSyncException where
  condense ChainSyncException{csePeerId, cseException} =
    condense csePeerId ++ ": " ++ show cseException

-- | A state view is a partial view of the state of the whole peer simulator.
-- This includes information about the part of the code that is being tested
-- (for instance the fragment that is selected by the ChainDB) but also
-- information about the mocked peers (for instance the exceptions raised in the
-- mocked ChainSync server threads).
data StateView = StateView {
    svSelectedChain       :: AnchoredFragment (Header TestBlock),
    svChainSyncExceptions :: [ChainSyncException]
  }
  deriving Show

instance Condense StateView where
  condense StateView {svSelectedChain, svChainSyncExceptions} =
    "SelectedChain: " ++ terseHFragment svSelectedChain ++ "\n"
    ++ "ChainSyncExceptions:\n" ++ unlines (("  - " ++) . condense <$> svChainSyncExceptions)

-- | Return the list of peer ids for all peers whose ChainSync thread was killed
-- by the node under test (that is it received 'ThreadKilled').
chainSyncKilled :: StateView -> [PeerId]
chainSyncKilled stateView =
  mapMaybe
    (\ChainSyncException{csePeerId, cseException} ->
       if wasKilled cseException
         then Just csePeerId
         else Nothing)
    (svChainSyncExceptions stateView)
  where
    wasKilled :: SomeException -> Bool
    wasKilled e = case fromException e of
      Just ThreadKilled -> True
      _                 -> False

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
  ChainDB m TestBlock ->
  m StateView
snapshotStateView StateViewTracers{svtGetChainSyncExceptions} chainDb = do
  svChainSyncExceptions <- svtGetChainSyncExceptions
  svSelectedChain <- atomically $ ChainDB.getCurrentChain chainDb
  pure StateView {svSelectedChain, svChainSyncExceptions}
