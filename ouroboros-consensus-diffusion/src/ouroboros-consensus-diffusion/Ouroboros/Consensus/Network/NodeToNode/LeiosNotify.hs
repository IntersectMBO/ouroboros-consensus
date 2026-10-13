{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The node's LeiosNotify client and server, and what the node-to-node
-- wiring needs to run them.
module Ouroboros.Consensus.Network.NodeToNode.LeiosNotify
  ( leiosNotifyClient
  , leiosNotifyProtocols
  , leiosNotifyPruner
  , leiosNotifyServer
  , withLeiosNotifyClientState
  ) where

import Cardano.Network.NodeToNode (NodeToNodeVersion (..), addSafetyMargin)
import Cardano.Network.PeerSelection (PeerTrustable)
import Control.Monad.Except (runExcept)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (BlockchainTime (..), SystemTime (..))
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory)
import Ouroboros.Consensus.Ledger.Abstract (GetTip, LedgerState, getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Leios.Protocol (LeiosProtocol)
import Ouroboros.Consensus.Leios.Types (LeiosPoint, LeiosVote)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck
  ( ClockSkew
  , SomeHeaderInFutureCheck
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.ControlMessage (ControlMessageSTM)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolLimits (..)
  , OuroborosBundleWithExpandedCtx
  , RunMiniProtocolWithExpandedCtx
  , StartOnDemandOrEagerly (..)
  , WithProtocolTemperature (..)
  , withHot
  )

-- | The node's LeiosNotify client. It checks each reply against the peer's
-- state, which it updates, and disconnects the peer if a check fails.
leiosNotifyClient ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  , ConvertRawHash blk
  , HasHardForkHistory blk
  ) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  SystemTime m ->
  SomeHeaderInFutureCheck m blk ->
  ClockSkew ->
  ControlMessageSTM m ->
  StrictTVar m (LeiosNotifyPeerState blk) ->
  LeiosNotifyClientPeerPipelined LeiosPoint (Header blk) LeiosVote m ()
leiosNotifyClient
  getTopLevelConfig
  getChainDB
  getBlockchainTime
  systemTime
  chainSyncFutureCheck
  maxClockSkew
  controlMessageSTM
  stateVar =
    leiosNotifyClientPeerHandlingReplies
      leiosNotifyPipelineDepth
      ( awaitImmTipCanForecastNow
          getTopLevelConfig
          (ChainDB.getImmutableLedger getChainDB)
          (getCurrentSlot getBlockchainTime)
      )
      controlMessageSTM
      ( \case
          MsgLeiosBlockAnnouncement hdr -> do
            immLedger <- atomically $ ChainDB.getImmutableLedger getChainDB
            st <- readTVarIO stateVar
            st' <-
              onLeiosNotifyAnnouncement
                systemTime
                chainSyncFutureCheck
                getTopLevelConfig
                immLedger
                st
                hdr
            -- The pruner may have run meanwhile; keep its work.
            atomically $ do
              pruneSlot <- lnpsPruneSlot <$> readTVar stateVar
              writeTVar stateVar $! pruneLeiosNotifyPeerState pruneSlot st'
          MsgLeiosBlockOffer point claimed -> do
            immLedger <- atomically $ ChainDB.getImmutableLedger getChainDB
            res <- atomically $ do
              st <- readTVar stateVar
              case runExcept (checkLeiosBlockOffer getTopLevelConfig immLedger point claimed st) of
                Left err -> pure (Left err)
                Right st' -> Right () <$ writeTVar stateVar st'
            either throwIO pure res
          MsgLeiosBlockTxsOffer point -> do
            res <- atomically $ do
              st <- readTVar stateVar
              case runExcept (checkLeiosClosureOffer point st) of
                Left err -> pure (Left err)
                Right st' -> Right () <$ writeTVar stateVar st'
            either throwIO pure res
          MsgLeiosVotes votes -> do
            immLedger <- atomically $ ChainDB.getImmutableLedger getChainDB
            now <- systemTimeCurrent systemTime
            res <- atomically $ do
              st <- readTVar stateVar
              case runExcept (checkLeiosVotes getTopLevelConfig maxClockSkew immLedger now votes st) of
                Left err -> pure (Left err)
                Right st' -> Right () <$ writeTVar stateVar st'
            either throwIO pure res
          MsgCanceled -> pure ()
      )

-- | The node's LeiosNotify server.
leiosNotifyServer ::
  IOLike m =>
  m (LeiosNotifyServerPeerLookahead LeiosPoint (Header blk) LeiosVote m)
leiosNotifyServer = do
  requests <- newTVarIO (0 :: Int)
  leiosNotifyServerPeerLookahead
    ( atomically $ do
        n <- readTVar requests
        if n >= leiosNotifyPipelineDepth
          then pure ExcessiveRequests
          else do
            writeTVar requests (n + 1)
            pure NotExcessiveRequests
    )
    -- The node never has a notification to send.
    retry

-- | Run the given action with a fresh LeiosNotify client state, registered
-- under the peer for as long as the action runs.
withLeiosNotifyClientState ::
  (IOLike m, Ord peer, NoThunks (Header blk)) =>
  StrictTVar m (Map peer (StrictTVar m (LeiosNotifyPeerState blk))) ->
  peer ->
  (StrictTVar m (LeiosNotifyPeerState blk) -> m a) ->
  m a
withLeiosNotifyClientState statesVar peer =
  bracket
    ( atomically $ do
        stateVar <- newTVar emptyLeiosNotifyPeerState
        modifyTVar statesVar (Map.insert peer stateVar)
        pure stateVar
    )
    (\_stateVar -> atomically $ modifyTVar statesVar (Map.delete peer))

-- | Prune every LeiosNotify client's state each time the immutable tip's slot
-- advances.
leiosNotifyPruner ::
  ( IOLike m
  , GetTip (LedgerState blk)
  ) =>
  ChainDB m blk ->
  StrictTVar m (Map peer (StrictTVar m (LeiosNotifyPeerState blk))) ->
  m Void
leiosNotifyPruner chainDB statesVar = go (SlotNo 0)
 where
  go prunedSlot = do
    immTipSlot <- atomically $ do
      immLedger <- ChainDB.getImmutableLedger chainDB
      case getTipSlot (ledgerState immLedger) of
        NotOrigin slot
          | prunedSlot < slot -> do
              states <- readTVar statesVar
              mapM_ (\stateVar -> modifyTVar stateVar (pruneLeiosNotifyPeerState slot)) states
              pure slot
        _ -> retry
    go immTipSlot

-- | LeiosNotify, to add to a node-to-node bundle, if the version has it.
--
-- TODO remove once 'NodeToNodeProtocols' has fields for the Leios mini-protocols.
leiosNotifyProtocols ::
  NodeToNodeVersion ->
  RunMiniProtocolWithExpandedCtx mode addr PeerTrustable b m a c ->
  OuroborosBundleWithExpandedCtx mode addr PeerTrustable b m a c
leiosNotifyProtocols version run
  | version >= NodeToNodeV_16 =
      mempty
        { withHot =
            WithHot
              [ MiniProtocol
                  { miniProtocolNum = leiosNotifyMiniProtocolNum
                  , miniProtocolStart = StartOnDemand
                  , miniProtocolLimits = leiosNotifyProtocolLimits
                  , miniProtocolRun = run
                  }
              ]
        }
  | otherwise = mempty

-- | The ingress queue must hold a reply to every request the client may leave
-- outstanding, each as large as 'maxLeiosNotifyReplyBytes' allows; the
-- server's, which only receives requests, needs far less.
leiosNotifyProtocolLimits :: MiniProtocolLimits
leiosNotifyProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue =
        addSafetyMargin $ leiosNotifyPipelineDepth * fromIntegral maxLeiosNotifyReplyBytes
    }
