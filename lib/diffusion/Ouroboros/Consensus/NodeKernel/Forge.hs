{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Run one leadership check and, if we are leader, forge and adopt a block.
--
-- This is spawned once per forge-credentials thread by
-- 'Ouroboros.Consensus.NodeKernel.forkBlockForging'.
module Ouroboros.Consensus.NodeKernel.Forge
  ( forge
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Tracer
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Proxy
import Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HeaderValidation
  ( BasicEnvelopeValidation (..)
  , HeaderState (..)
  , headerStateChainDep
  )
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.Mempool.API (TxMeasureWithDiffTime)
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Tracers
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockResult (..)
  , ChainDB
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.EarlyExit
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , AnchoredSeq (..)
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

forge ::
  forall m blk.
  (IOLike m, RunNode blk) =>
  Tracer m (TraceLabelCreds (TraceForgeEvent blk)) ->
  Tracer m (TraceLabelCreds (ForgeStateInfo blk)) ->
  TopLevelConfig blk ->
  ChainDB m blk ->
  Mempool m blk ->
  BlockForging m blk ->
  SlotNo ->
  WithEarlyExit m ()
forge forgeEventTracer forgeStateInfoTracer cfg chainDB mempool blockForging currentSlot = do
  let trace :: TraceForgeEvent blk -> WithEarlyExit m ()
      trace =
        lift
          . traceWith forgeEventTracer
          . TraceLabelCreds (forgeLabel blockForging)

  trace $ TraceStartLeadershipCheck currentSlot

  BlockContext{bcBlockNo, bcPrevPoint} <- getBlockContext trace chainDB currentSlot
  trace $ TraceBlockContext currentSlot bcBlockNo bcPrevPoint

  -- Get forker corresponding to bcPrevPoint
  --
  -- This might fail if, in between choosing 'bcPrevPoint' and this call to
  -- 'ChainDB.withReadOnlyForkerAtPoint', we switched to a fork where 'bcPrevPoint'
  -- is no longer on our chain. When that happens, we simply give up on the
  -- chance to produce a block.
  (fbArgs, txssz, snapSize, forgingOnTopOf) <-
    ChainDB.withReadOnlyForkerAtPoint chainDB (SpecificPoint bcPrevPoint) $ \case
      Left _ -> do
        trace $ TraceNoLedgerState currentSlot bcPrevPoint
        exitEarly
      Right forker -> do
        unticked <- lift $ atomically $ LedgerDB.roforkerGetLedgerState forker

        trace $ TraceLedgerState currentSlot bcPrevPoint

        ledgerView <- getLedgerView trace cfg currentSlot unticked

        let tickedChainDepState = getTickedChainDepState cfg currentSlot unticked ledgerView

        proof <-
          getIsLeaderProof
            trace
            forgeStateInfoTracer
            blockForging
            cfg
            currentSlot
            tickedChainDepState

        tickedLedgerState <- getTickedLedgerState trace cfg currentSlot bcPrevPoint unticked

        traceForgingMempoolSnapshot trace mempool currentSlot bcPrevPoint

        (txs, txssz, snapSize) <- getTransactionsToForge cfg mempool currentSlot tickedLedgerState forker

        let fbArgs =
              Block.ForgeBlockArgs
                { Block.fbConfig = cfg
                , Block.fbCurrentBlockNo = bcBlockNo
                , Block.fbCurrentSlotNo = currentSlot
                , Block.fbPerasCert = Nothing -- No PerasCert for now
                , Block.fbCurrentTickedLedgerState = forgetLedgerTables tickedLedgerState
                , Block.fbTxs = txs
                , Block.fbIsLeader = proof
                }
        pure
          ( fbArgs
          , txssz
          , snapSize
          , ledgerTipPoint (ledgerState unticked)
          )

  -- Actually produce the block
  newBlock <- lift $ Block.forgeBlock blockForging fbArgs

  trace $
    TraceForgedBlock
      currentSlot
      forgingOnTopOf
      newBlock
      snapSize
      txssz

  addBlockToChainDB trace chainDB mempool currentSlot (fbTxs fbArgs) newBlock

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo :: !BlockNo
  -- ^ the block number of the block to be forged
  , bcPrevPoint :: !(Point blk)
  -- ^ the point of /the predecessor of/ the block
  --
  -- Note that a block/header stores the hash of its predecessor but not the
  -- slot.
  }

-- | Figure out which block to connect to
--
-- Normally this will be the current block at the tip, but it may be the
-- /previous/ block, if there were multiple slot leaders
getBlockContext ::
  (IOLike m, RunNode blk) =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  ChainDB m blk ->
  SlotNo ->
  WithEarlyExit m (BlockContext blk)
getBlockContext trace chainDB currentSlot = do
  eBlkCtx <-
    lift $
      atomically $
        mkCurrentBlockContext currentSlot
          <$> ChainDB.getCurrentChain chainDB
  case eBlkCtx of
    Right blkCtx -> return blkCtx
    Left failure -> do
      trace failure
      exitEarly

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
  HasHeader (Header blk) =>
  Header blk -> BlockContext blk
blockContextFromPrevHeader hdr =
  -- Recall that an EBB has the same block number as its predecessor, so this
  -- @succ@ is even correct when @hdr@ is an EBB.
  BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
--
-- The 'bcPrevPoint' will either refer to the header at the tip of the current
-- chain or, in case there is already a block in this slot (e.g. another node
-- was also elected leader and managed to produce a block before us), the tip's
-- predecessor. If the chain is empty, then it will refer to the chain's anchor
-- point, which may be genesis.
mkCurrentBlockContext ::
  forall blk.
  RunNode blk =>
  -- | the current slot, i.e. the slot of the block about to be forged
  SlotNo ->
  -- | the current chain fragment
  --
  -- Recall that the anchor point is the tip of the ImmutableDB.
  AnchoredFragment (Header blk) ->
  -- | the event records the cause of the failure
  Either (TraceForgeEvent blk) (BlockContext blk)
mkCurrentBlockContext currentSlot c = case c of
  Empty AF.AnchorGenesis ->
    -- The chain is entirely empty.
    Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint
  Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
    let p :: Point blk = BlockPoint anchorSlot anchorHash
     in if anchorSlot < currentSlot
          then Right $ BlockContext (succ anchorBlockNo) p
          else Left $ TraceSlotIsImmutable currentSlot p anchorBlockNo
  c' :> hdr -> case blockSlot hdr `compare` currentSlot of
    -- The block at the tip of our chain has a slot number /before/ the
    -- current slot number. This is the common case, and we just want to
    -- connect our new block to the block at the tip.
    LT -> Right $ blockContextFromPrevHeader hdr
    -- The block at the tip of our chain has a slot that lies in the
    -- future. Although the chain DB should not contain blocks from the
    -- future, if the volatile DB contained such blocks on startup
    -- (due to a node clock misconfiguration) this invariant may be
    -- violated. See: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/docs/website/contents/for-developers/HandlingBlocksFromTheFuture.md#handling-blocks-from-the-future
    -- Also note that if the
    -- system is under heavy load, it is possible (though unlikely) that
    -- one or more slots have passed after @currentSlot@ that we got from
    -- @onSlotChange@ and before we queried the chain DB for the block
    -- at its tip. At the moment, we simply don't produce a block if this
    -- happens.

    -- TODO: We may wish to produce a block here anyway, treating this
    -- as similar to the @EQ@ case below, but we should be careful:
    --
    -- 1. We should think about what slot number to use.
    -- 2. We should be careful to distinguish between the case where we
    --    need to drop a block from the chain and where we don't.
    -- 3. We should be careful about slot numbers and EBBs.
    -- 4. We should probably not produce a block if the system is under
    --    very heavy load (e.g., if a lot of blocks have been produced
    --    after @currentTime@).
    --
    -- See <https://github.com/IntersectMBO/ouroboros-network/issues/1462>
    GT -> Left $ TraceBlockFromFuture currentSlot (blockSlot hdr)
    -- The block at the tip has the same slot as the block we're going to
    -- produce (@currentSlot@).
    EQ ->
      Right $
        if isJust (headerIsEBB hdr)
          -- We allow forging a block that is the successor of an EBB in the
          -- same slot.
          then blockContextFromPrevHeader hdr
          -- If @hdr@ is not an EBB, then forge an alternative to @hdr@: same
          -- block no and same predecessor.
          else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'

-- | Add a forged block to the ChainDB, tracing whether it was adopted, and
-- removing its transactions from the mempool if it turned out to be invalid.
addBlockToChainDB ::
  (IOLike m, RunNode blk) =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  ChainDB m blk ->
  Mempool m blk ->
  SlotNo ->
  [Validated (GenTx blk)] ->
  blk ->
  WithEarlyExit m ()
addBlockToChainDB trace chainDB mempool currentSlot txs newBlock = do
  let noPunish = InvalidBlockPunishment.noPunishment -- no way to punish yourself
  -- Make sure that if an async exception is thrown while a block is
  -- added to the chain db, we will remove txs from the mempool.

  -- 'addBlockAsync' is a non-blocking action, so `mask_` would suffice,
  -- but the finalizer is a blocking operation, hence we need to use
  -- 'uninterruptibleMask_' to make sure that async exceptions do not
  -- interrupt it.
  uninterruptibleMask_ $ do
    result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
    -- Block until we have processed the block
    mbCurTip <- lift $ atomically $ ChainDB.blockProcessed result

    -- Check whether we adopted our block
    when (mbCurTip /= SuccesfullyAddedBlock (blockPoint newBlock)) $ do
      isInvalid <-
        lift $
          atomically $
            ($ blockHash newBlock) . forgetFingerprint
              <$> ChainDB.getIsInvalidBlock chainDB
      case isInvalid of
        Nothing ->
          trace $ TraceDidntAdoptBlock currentSlot newBlock
        Just reason -> do
          trace $ TraceForgedInvalidBlock currentSlot newBlock reason
          -- We just produced a block that is invalid according to the
          -- ledger in the ChainDB, while the mempool said it is valid.
          -- There is an inconsistency between the two!
          --
          -- Remove all the transactions in that block, otherwise we'll
          -- run the risk of forging the same invalid block again. This
          -- means that we'll throw away some good transactions in the
          -- process.
          whenJust
            (NE.nonEmpty (map (txId . txForgetValidated) txs))
            (lift . removeTxsEvenIfValid mempool)
      exitEarly

    -- We successfully produced /and/ adopted a block
    --
    -- NOTE: we are tracing the transactions we retrieved from the Mempool,
    -- not the transactions actually /in the block/.
    -- The transactions in the block should be a prefix of the transactions
    -- in the mempool. If this is not the case, this is a bug.
    -- Unfortunately, we can't
    -- assert this here because the ability to extract transactions from a
    -- block, i.e., the @HasTxs@ class, is not implementable by all blocks,
    -- e.g., @DualBlock@.
    trace $ TraceAdoptedBlock currentSlot newBlock txs

-- | Obtain the ticked ledger view for 'currentSlot', required in order to
-- construct the ticked 'ChainDepState'.
getLedgerView ::
  (IOLike m, RunNode blk) =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  TopLevelConfig blk ->
  SlotNo ->
  ExtLedgerState blk EmptyMK ->
  WithEarlyExit m (LedgerView (BlockProtocol blk))
getLedgerView trace cfg currentSlot unticked = do
  ledgerView <-
    case runExcept $
      forecastFor
        ( ledgerViewForecastAt
            (configLedger cfg)
            (ledgerState unticked)
        )
        currentSlot of
      Left err -> do
        -- There are so many empty slots between the tip of our chain and the
        -- current slot that we cannot get a ledger view anymore. In
        -- principle, this is no problem; we can still produce a block (we use
        -- the ticked ledger state). However, we probably don't /want/ to
        -- produce a block in this case; we are most likely missing blocks
        -- on our chain.
        trace $ TraceNoLedgerView currentSlot err
        exitEarly
      Right lv ->
        return lv

  trace $ TraceLedgerView currentSlot
  pure ledgerView

-- | Tick the 'ChainDepState' for the 'SlotNo' we're producing a block for. We
-- only need the ticked 'ChainDepState' to check whether we're a leader.
-- This is much cheaper than ticking the entire 'ExtLedgerState'.
getTickedChainDepState ::
  RunNode blk =>
  TopLevelConfig blk ->
  SlotNo ->
  ExtLedgerState blk EmptyMK ->
  LedgerView (BlockProtocol blk) ->
  Ticked (ChainDepState (BlockProtocol blk))
getTickedChainDepState cfg currentSlot unticked ledgerView =
  tickChainDepState
    (configConsensus cfg)
    ledgerView
    currentSlot
    (headerStateChainDep (headerState unticked))

-- | Check whether we are leader for 'currentSlot', given the ticked
-- 'ChainDepState', and obtain the leadership proof if so.
getIsLeaderProof ::
  (IOLike m, RunNode blk) =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  Tracer m (TraceLabelCreds (ForgeStateInfo blk)) ->
  BlockForging m blk ->
  TopLevelConfig blk ->
  SlotNo ->
  Ticked (ChainDepState (BlockProtocol blk)) ->
  WithEarlyExit m (IsLeader (BlockProtocol blk))
getIsLeaderProof trace forgeStateInfoTracer blockForging cfg currentSlot tickedChainDepState = do
  proof <- do
    shouldForge <-
      lift $
        checkShouldForge
          blockForging
          ( contramap
              (TraceLabelCreds (forgeLabel blockForging))
              forgeStateInfoTracer
          )
          cfg
          currentSlot
          tickedChainDepState
    case shouldForge of
      ForgeStateUpdateError err -> do
        trace $ TraceForgeStateUpdateError currentSlot err
        exitEarly
      CannotForge cannotForge -> do
        trace $ TraceNodeCannotForge currentSlot cannotForge
        exitEarly
      NotLeader -> do
        trace $ TraceNodeNotLeader currentSlot
        exitEarly
      ShouldForge p -> return p

  -- At this point we have established that we are indeed slot leader
  trace $ TraceNodeIsLeader currentSlot
  pure proof

-- | Tick the ledger state for the 'SlotNo' we're producing a block for
getTickedLedgerState ::
  (IOLike m, RunNode blk) =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  TopLevelConfig blk ->
  SlotNo ->
  Point blk ->
  ExtLedgerState blk EmptyMK ->
  WithEarlyExit m (Ticked LedgerState blk DiffMK)
getTickedLedgerState trace cfg currentSlot bcPrevPoint unticked = do
  let tickedLedgerState =
        applyChainTick
          OmitLedgerEvents
          (configLedger cfg)
          currentSlot
          (ledgerState unticked)

  _ <- evaluate tickedLedgerState
  trace $ TraceForgeTickedLedgerState currentSlot bcPrevPoint
  pure tickedLedgerState

-- | Get a snapshot of the mempool that is consistent with the ledger, and
-- trace it.
--
-- NOTE: It is possible that due to adoption of new blocks the /current/
-- ledger will have changed. This doesn't matter: we will produce a block
-- that fits onto the ledger we got above; if the ledger in the meantime
-- changes, the block we produce here may or may not be adopted, but it
-- won't be invalid.
traceForgingMempoolSnapshot ::
  IOLike m =>
  (TraceForgeEvent blk -> WithEarlyExit m ()) ->
  Mempool m blk ->
  SlotNo ->
  Point blk ->
  WithEarlyExit m ()
traceForgingMempoolSnapshot trace mempool currentSlot bcPrevPoint = do
  (mempoolHash, mempoolSlotNo) <- lift $ atomically $ do
    snap <- getSnapshot mempool -- only used for its tip-like information
    pure (castHash $ snapshotStateHash snap, snapshotSlotNo snap)

  _ <- evaluate mempoolHash

  trace $ TraceForgingMempoolSnapshot currentSlot bcPrevPoint mempoolHash mempoolSlotNo

-- | Get a consistent snapshot of the mempool for the given ticked ledger state
-- and select transactions up to block capacity.
getTransactionsToForge ::
  (IOLike m, RunNode blk) =>
  TopLevelConfig blk ->
  Mempool m blk ->
  SlotNo ->
  Ticked LedgerState blk DiffMK ->
  ReadOnlyForker m l blk ->
  WithEarlyExit m ([Validated (GenTx blk)], TxMeasureWithDiffTime blk, MempoolSize)
getTransactionsToForge cfg mempool currentSlot tickedLedgerState forker = lift $ do
  mempoolSnapshot <-
    getSnapshotFor
      mempool
      currentSlot
      tickedLedgerState
      (roforkerReadTables forker)

  let (txs, txssz) =
        snapshotTake mempoolSnapshot $
          blockCapacityTxMeasure (configLedger cfg) tickedLedgerState
  -- NB respect the capacity of the ledger state we're extending,
  -- which is /not/ 'snapshotLedgerState'

  _ <- evaluate (length txs)

  pure (txs, txssz, snapshotMempoolSize mempoolSnapshot)
