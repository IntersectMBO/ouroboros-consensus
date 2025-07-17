{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Initialization of the 'BlockFetchConsensusInterface'
module Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface
  ( ChainDbView (..)
  , defaultChainDbView
  , mkBlockFetchConsensusInterface
  , readFetchModeDefault
  ) where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap
  ( UseBootstrapPeers
  , requiresBootstrapPeers
  )
import Cardano.Network.Types (LedgerStateJudgement)
import Control.Monad
import Control.Tracer (Tracer)
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Config.SupportsNode as SupportsNode
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping as CSJumping
import Ouroboros.Consensus.Peras.Weight (emptyPerasWeightSnapshot)
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise
  , ChainDB
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
  ( InvalidBlockPunishment
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Util.AnchoredFragment
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (MaxSlotNo)
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( BlockFetchConsensusInterface (..)
  , ChainSelStarvation
  , FetchMode (..)
  , FromConsensus (..)
  , PraosFetchMode (..)
  , mkReadFetchMode
  )
import Ouroboros.Network.SizeInBytes

-- | Abstract over the ChainDB
data ChainDbView m blk = ChainDbView
  { getCurrentChain :: STM m (AnchoredFragment (Header blk))
  , getCurrentChainWithTime :: STM m (AnchoredFragment (HeaderWithTime blk))
  , getIsFetched :: STM m (Point blk -> Bool)
  , getMaxSlotNo :: STM m MaxSlotNo
  , addBlockAsync :: InvalidBlockPunishment m -> blk -> m (AddBlockPromise m blk)
  , getChainSelStarvation :: STM m ChainSelStarvation
  }

defaultChainDbView :: ChainDB m blk -> ChainDbView m blk
defaultChainDbView chainDB =
  ChainDbView
    { getCurrentChain = ChainDB.getCurrentChain chainDB
    , getCurrentChainWithTime = ChainDB.getCurrentChainWithTime chainDB
    , getIsFetched = ChainDB.getIsFetched chainDB
    , getMaxSlotNo = ChainDB.getMaxSlotNo chainDB
    , addBlockAsync = ChainDB.addBlockAsync chainDB
    , getChainSelStarvation = ChainDB.getChainSelStarvation chainDB
    }

readFetchModeDefault ::
  (MonadSTM m, HasHeader blk) =>
  ConsensusMode ->
  BlockchainTime m ->
  STM m (AnchoredFragment blk) ->
  STM m UseBootstrapPeers ->
  STM m LedgerStateJudgement ->
  STM m FetchMode
readFetchModeDefault
  consensusMode
  btime
  getCurrentChain
  getUseBootstrapPeers
  getLedgerStateJudgement =
    mkReadFetchMode consensusMode getLedgerStateJudgement praosFetchMode
   where
    praosFetchMode = do
      mCurSlot <- getCurrentSlot btime
      usingBootstrapPeers <-
        requiresBootstrapPeers
          <$> getUseBootstrapPeers
          <*> getLedgerStateJudgement

      -- This logic means that when the node is using bootstrap peers and is in
      -- TooOld state it will always return BulkSync. Otherwise if the node
      -- isn't using bootstrap peers (i.e. has them disabled it will use the old
      -- logic of returning BulkSync if behind 1000 slots
      case (usingBootstrapPeers, mCurSlot) of
        (True, _) -> return FetchModeBulkSync
        (False, CurrentSlotUnknown) -> return FetchModeBulkSync
        (False, CurrentSlot curSlot) -> do
          curChainSlot <- AF.headSlot <$> getCurrentChain
          let slotsBehind = case curChainSlot of
                -- There's nothing in the chain. If the current slot is 0, then
                -- we're 1 slot behind.
                Origin -> unSlotNo curSlot + 1
                NotOrigin slot -> unSlotNo curSlot - unSlotNo slot
              maxSlotsBehind = 1000
          return $
            if slotsBehind < maxSlotsBehind
              -- When the current chain is near to "now", use deadline mode,
              -- when it is far away, use bulk sync mode.
              then FetchModeDeadline
              else FetchModeBulkSync

mkBlockFetchConsensusInterface ::
  forall m peer blk.
  ( IOLike m
  , BlockSupportsDiffusionPipelining blk
  , Ord peer
  , LedgerSupportsProtocol blk
  , SupportsNode.ConfigSupportsNode blk
  ) =>
  Tracer m (CSJumping.TraceEventDbf peer) ->
  BlockConfig blk ->
  ChainDbView m blk ->
  CSClient.ChainSyncClientHandleCollection peer m blk ->
  (Header blk -> SizeInBytes) ->
  -- | See 'readFetchMode'.
  STM m FetchMode ->
  DiffusionPipeliningSupport ->
  BlockFetchConsensusInterface peer (HeaderWithTime blk) blk m
mkBlockFetchConsensusInterface
  csjTracer
  bcfg
  chainDB
  csHandlesCol
  blockFetchSize
  readFetchMode
  pipelining =
    BlockFetchConsensusInterface{blockFetchSize = blockFetchSize . hwtHeader, ..}
   where
    getCandidates :: STM m (Map peer (AnchoredFragment (HeaderWithTime blk)))
    getCandidates = CSClient.viewChainSyncState (CSClient.cschcMap csHandlesCol) CSClient.csCandidate

    blockMatchesHeader :: HeaderWithTime blk -> blk -> Bool
    blockMatchesHeader hwt b = Block.blockMatchesHeader (hwtHeader hwt) b

    readCandidateChains :: STM m (Map peer (AnchoredFragment (HeaderWithTime blk)))
    readCandidateChains = getCandidates

    readCurrentChain :: STM m (AnchoredFragment (HeaderWithTime blk))
    readCurrentChain = getCurrentChainWithTime chainDB

    readFetchedBlocks :: STM m (Point blk -> Bool)
    readFetchedBlocks = getIsFetched chainDB

    -- See 'mkAddFetchedBlock_'
    mkAddFetchedBlock ::
      STM m (Point blk -> blk -> m ())
    mkAddFetchedBlock = do
      pipeliningPunishment <- InvalidBlockPunishment.mkForDiffusionPipelining
      pure $ mkAddFetchedBlock_ pipeliningPunishment pipelining

    -- Hand over the block to the ChainDB, but don't wait until it has been
    -- written to disk or processed.
    mkAddFetchedBlock_ ::
      ( BlockConfig blk ->
        Header blk ->
        InvalidBlockPunishment m ->
        InvalidBlockPunishment m
      ) ->
      DiffusionPipeliningSupport ->
      Point blk ->
      blk ->
      m ()
    mkAddFetchedBlock_ pipeliningPunishment enabledPipelining _pt blk = void $ do
      disconnect <- InvalidBlockPunishment.mkPunishThisThread
      -- A BlockFetch peer can either send an entire range or none of the
      -- range; anything else will incur a disconnect. And in 'FetchDeadline'
      -- mode, which is the relevant case for this kind of DoS attack (because
      -- in bulk sync, our honest peers will be streaming a very dense chain
      -- very quickly, meaning the adversary only has very small windows during
      -- which we're interested in its chains), the node only requests whole
      -- suffixes from peers: the BlockFetch decision logic does not avoid
      -- requesting a block that is already in-flight from other peers. Thus
      -- the adversary cannot send us blocks out-of-order (during
      -- 'FetchDeadline'), even if they control more than one of our peers.
      --
      -- Therefore, the following punishment logic only needs to cover the
      -- "whole chain received in-order from a single-peer" case. Which it
      -- currently does.
      --
      -- TODO maintain the context of which ChainSync candidate incurring this
      -- fetch request, and disconnect immediately if the invalid block is not
      -- the tip of that candidate. As-is, in 'FetchDeadline' they must also
      -- send the next block, but they might be able to wait long enough that
      -- it is not desirable when it arrives, and therefore not be disconnected
      -- from. So their choices are: cause a disconnect or else do nothing for
      -- long enough. Both are fine by us, from a DoS mitigation perspective.
      let punishment = InvalidBlockPunishment.branch $ \case
            -- invalid parents always cause a disconnect
            InvalidBlockPunishment.BlockPrefix -> disconnect
            -- when pipelining, we forgive an invalid block itself if it's
            -- better than the previous invalid block this peer delivered
            InvalidBlockPunishment.BlockItself -> case enabledPipelining of
              DiffusionPipeliningOff -> disconnect
              DiffusionPipeliningOn ->
                pipeliningPunishment bcfg (getHeader blk) disconnect
      addBlockAsync
        chainDB
        punishment
        blk

    readFetchedMaxSlotNo :: STM m MaxSlotNo
    readFetchedMaxSlotNo = getMaxSlotNo chainDB

    -- Note that @ours@ comes from the ChainDB and @cand@ from the ChainSync
    -- client.
    --
    -- Fragments are proxies for their corresponding chains; it is possible, in
    -- principle, that an empty fragment corresponds to the chain we want to
    -- adopt, and should therefore be preferred over other fragments (whose
    -- blocks we therefore do not want to download). The precondition to
    -- 'preferAnchoredCandidates' is designed precisely to rule out this
    -- possibility (for details, see the Consensus Report), but unfortunately we
    -- cannot always satisfy this precondition: although the chain sync client
    -- preserves an invariant that relates our current chain to the candidate
    -- fragment, by the time the block fetch download logic considers the
    -- fragment, our current chain might have changed.
    plausibleCandidateChain ::
      HasCallStack =>
      AnchoredFragment (HeaderWithTime blk) ->
      AnchoredFragment (HeaderWithTime blk) ->
      Bool
    plausibleCandidateChain ours cand =
      -- 1. The ChainDB maintains the invariant that the anchor of our fragment
      --    corresponds to the immutable tip.
      --
      -- 2. The ChainSync client locally maintains the invariant that our
      --    fragment and the candidate fragment have the same anchor point. This
      --    establishes the precondition required by @preferAnchoredCandidate@.
      --
      -- 3. However, by the time that the BlockFetch logic processes a fragment
      --    presented to it by the ChainSync client, our current fragment might
      --    have changed, and they might no longer be anchored at the same
      --    point. This means that we are no longer guaranteed that the
      --    precondition holds.
      --
      -- 4. Therefore, we check whether the candidate fragments still intersects
      --    with our fragment; if not, then it is only a matter of time until the
      --    ChainSync client disconnects from that peer.
      case AF.intersectionPoint ours cand of
        -- REVIEW: Hmm, maybe we want to change 'preferAnchoredCandidates' to
        -- also just return 'False' in this case (and we remove the
        -- precondition).
        Nothing -> False
        Just _ -> preferAnchoredCandidate bcfg weights ours cand

    compareCandidateChains ::
      AnchoredFragment (HeaderWithTime blk) ->
      AnchoredFragment (HeaderWithTime blk) ->
      Ordering
    compareCandidateChains = compareAnchoredFragments bcfg weights

    -- TODO requires https://github.com/IntersectMBO/ouroboros-network/pull/5161
    weights = emptyPerasWeightSnapshot

    headerForgeUTCTime :: FromConsensus (HeaderWithTime blk) -> STM m UTCTime
    headerForgeUTCTime =
      pure
        . fromRelativeTime (SupportsNode.getSystemStart bcfg)
        . hwtSlotRelativeTime
        . unFromConsensus

    readChainSelStarvation = getChainSelStarvation chainDB

    demoteChainSyncJumpingDynamo :: peer -> m ()
    demoteChainSyncJumpingDynamo = CSJumping.rotateDynamo csjTracer csHandlesCol
