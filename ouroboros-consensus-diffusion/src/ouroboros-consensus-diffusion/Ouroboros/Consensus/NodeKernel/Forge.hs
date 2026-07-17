{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.NodeKernel.Forge
  ( forge
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Tracer
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Measure
import Data.Proxy
import LeiosDemoDb
  ( LeiosDbConnection (..)
  )
import LeiosDemoTypes
  ( LeiosCert
  , TraceLeiosKernel (..)
  )
import qualified LeiosDemoTypes as Leios
import LeiosUtils.CallTrace (CallCtx, CallName, CallTrace, SomeJsonCallTrace (SomeJsonCallTrace))
import LeiosVoteState (LeiosVoteState (..))
import Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( emptyLedgerTables
  , forgetLedgerTables
  , prependDiffs
  )
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
import qualified Ouroboros.Consensus.Util.EarlyExit as EarlyExit
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , AnchoredSeq (..)
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

-- | Run one leadership check and, if we are leader, forge and adopt a block.
--
-- This is spawned once per forge-credentials thread by 'Ouroboros.Consensus.NodeKernel.forkBlockForging'.
forge ::
  forall m blk.
  (IOLike m, RunNode blk) =>
  Tracer m (TraceLabelCreds (TraceForgeEvent blk)) ->
  Tracer m (TraceLabelCreds (ForgeStateInfo blk)) ->
  Tracer m TraceLeiosKernel ->
  CallCtx m ->
  TopLevelConfig blk ->
  ChainDB m blk ->
  Mempool m blk ->
  LeiosVoteState m ->
  BlockForging m blk ->
  LeiosDbConnection m ->
  SlotNo ->
  WithEarlyExit m ()
forge forgeEventTracer forgeStateInfoTracer leiosTracer forgeCCtx cfg chainDB mempool leiosVoteState blockForging leiosConn currentSlot = do
  let trace :: TraceForgeEvent blk -> WithEarlyExit m ()
      trace =
        lift
          . traceWith forgeEventTracer
          . TraceLabelCreds (forgeLabel blockForging)

      -- NB: this runs directly in @m@, /not/ 'WithEarlyExit' -- it must trace
      -- the matching 'CallEnd' unconditionally, even when the traced action
      -- calls 'exitEarly'. See 'callTraceSameThreadEarlyExit'.
      ctrace :: (Aeson.ToJSON a, Aeson.ToJSON r) => CallTrace a (Maybe r) -> m ()
      ctrace =
        traceWith forgeEventTracer
          . TraceLabelCreds (forgeLabel blockForging)
          . TraceCall
          . SomeJsonCallTrace

      _forgeTrace ::
        (Aeson.ToJSON a, Aeson.ToJSON r) =>
        CallName -> a -> (CallCtx m -> WithEarlyExit m r) -> WithEarlyExit m r
      _forgeTrace = forgeTraceVia id

      -- \| Like 'forgeTrace', but the value recorded in the trace is @f r@
      -- rather than @r@ itself -- for results (e.g. 'LedgerView') that don't
      -- have (or shouldn't be given) a real 'Aeson.ToJSON' instance, but a
      -- small projection of them does. The returned value is still the real,
      -- un-projected @r@.
      forgeTraceVia ::
        (Aeson.ToJSON a, Aeson.ToJSON r') =>
        (r -> r') -> CallName -> a -> (CallCtx m -> WithEarlyExit m r) -> WithEarlyExit m r
      forgeTraceVia f = EarlyExit.callTraceSameThreadVia f ctrace forgeCCtx

      forgeTrace' ::
        (Aeson.ToJSON a, Aeson.ToJSON r) =>
        CallName -> a -> WithEarlyExit m r -> WithEarlyExit m r
      forgeTrace' = forgeTrace'Via id

      forgeTrace'Via ::
        (Aeson.ToJSON a, Aeson.ToJSON r') =>
        (r -> r') -> CallName -> a -> WithEarlyExit m r -> WithEarlyExit m r
      forgeTrace'Via f cn arg act = forgeTraceVia f cn arg (\_ -> act)

  trace $ TraceStartLeadershipCheck currentSlot

  BlockContext{bcBlockNo, bcPrevPoint} <-
    forgeTrace' "get-block-context" currentSlot $ getBlockContext trace chainDB currentSlot

  trace $ TraceBlockContext currentSlot bcBlockNo bcPrevPoint

  -- Get forker corresponding to bcPrevPoint
  --
  -- This might fail if, in between choosing 'bcPrevPoint' and this call to
  -- 'ChainDB.withReadOnlyForkerAtPoint', we switched to a fork where 'bcPrevPoint'
  -- is no longer on our chain. When that happens, we simply give up on the
  -- chance to produce a block.
  (forgeBlockArgs, forgingOnTopOf, snapSize, rbTxsSize) <-
    ChainDB.withReadOnlyForkerAtPoint chainDB (SpecificPoint bcPrevPoint) $ \case
      Left _ -> do
        trace $ TraceNoLedgerState currentSlot bcPrevPoint
        exitEarly
      Right forker -> do
        unticked <- lift $ atomically $ LedgerDB.roforkerGetLedgerState forker

        trace $ TraceLedgerState currentSlot bcPrevPoint

        ledgerView <-
          forgeTrace'Via
            (const ())
            "get-ledger-view"
            currentSlot
            (getLedgerView trace cfg currentSlot unticked)

        tickedChainDepState <-
          forgeTrace'Via
            (const ())
            "get-ticked-chain-dep-state"
            currentSlot
            $ getTickedChainDepState cfg currentSlot unticked ledgerView

        proof <-
          forgeTrace'Via
            (const ())
            "get-is-leader-proof"
            currentSlot
            $ getIsLeaderProof
              trace
              forgeStateInfoTracer
              blockForging
              cfg
              currentSlot
              tickedChainDepState

        tickedLedgerState <-
          forgeTrace'Via
            (const ())
            "get-ticked-ledger-state"
            currentSlot
            $ getTickedLedgerState trace cfg currentSlot bcPrevPoint unticked

        traceForgingMempoolSnapshot trace mempool currentSlot bcPrevPoint

        (rbTxs, ebTxs, rbTxsSize, mempoolSnapshot, mayLeiosCertAndAnnouncement) <-
          forgeTrace'Via
            (const ())
            "partition-mempool"
            currentSlot
            $ lift
            $ partitionMempool
              leiosConn
              leiosVoteState
              leiosTracer
              cfg
              mempool
              currentSlot
              tickedLedgerState
              unticked
              forker

        pure
          ( Block.ForgeBlockArgs
              { Block.fbConfig = cfg
              , Block.fbCurrentBlockNo = bcBlockNo
              , Block.fbCurrentSlotNo = currentSlot
              , Block.fbCurrentTickedLedgerState = forgetLedgerTables tickedLedgerState
              , Block.fbRbTxs = rbTxs
              , Block.fbEbTxs = ebTxs
              , Block.fbIsLeader = proof
              , Block.fbChainDepState = Just (headerStateChainDep (headerState unticked))
              , Block.fbMayLeiosCert = fst <$> mayLeiosCertAndAnnouncement
              , Block.fbLeiosDb = leiosConn
              , Block.fbLeiosTracer = leiosTracer
              , Block.fbLeiosVoteState = leiosVoteState
              }
          , ledgerTipPoint (ledgerState unticked)
          , snapshotMempoolSize mempoolSnapshot
          , rbTxsSize
          )

  -- Actually produce the block
  newBlock <-
    forgeTrace'Via
      (const ())
      "forge-block"
      currentSlot
      $ lift
      $ Block.forgeBlock blockForging forgeBlockArgs

  trace $
    TraceForgedBlock
      currentSlot
      forgingOnTopOf
      newBlock
      snapSize
      rbTxsSize

  forgeTrace'Via
    (const ())
    "add-block-to-chaindb"
    currentSlot
    $ addBlockToChainDB
      trace
      chainDB
      mempool
      currentSlot
      (Block.fbRbTxs forgeBlockArgs)
      (Block.fbEbTxs forgeBlockArgs)
      newBlock

-- | Decide whether the block we're about to forge should certify a
-- previously-announced EB on this chain, returning the assembled
-- certificate and the 'LeiosPoint' at which the EB was announced.
--
-- An EB is certifiable when:
--
--   * it was announced on this chain ('protocolStateLeiosAnnouncement' is a 'Just');
--   * enough slots ('minCertificationGap') have elapsed since the announcement;
--   * we have downloaded its closure into the 'LeiosDb', and
--   * we have assembled a certificate for the announcing RB.
--
-- Returns 'Nothing' for non-Leios eras and whenever any of the above is not yet satisfied.
decideLeiosCertify ::
  forall blk m.
  ( Monad m
  , ResolveLeiosBlock blk
  , ConvertRawHash blk
  , HasAnnTip blk
  ) =>
  LeiosDbConnection m ->
  LeiosVoteState m ->
  Tracer m TraceLeiosKernel ->
  -- | The slot we are forging for.
  SlotNo ->
  -- | The state of the header we are extending.
  HeaderState blk ->
  m (Maybe (LeiosCert, Leios.EbHash))
decideLeiosCertify leiosDb voteState tracer currentSlot headerState =
  case protocolStateLeiosAnnouncement @blk (headerStateChainDep headerState) of
    Nothing -> pure Nothing
    Just (ebPoint, _ebSize)
      | unSlotNo currentSlot - unSlotNo (Leios.pointSlotNo ebPoint) <= Leios.minCertificationGap ->
          pure Nothing
      | otherwise -> do
          -- TODO: Why exactly do we guard against this? Also, shouldn't we
          -- detect it the other way around: if we have a cert, but not
          -- downloaded it ourselves -> warning!
          mClosure <- leiosDbLookupEbClosure leiosDb (Leios.pointEbHash ebPoint)
          case mClosure of
            Nothing -> do
              traceWith tracer $
                MkTraceLeiosKernel $
                  "EB not yet downloaded: " <> show ebPoint
              pure Nothing
            Just _ ->
              -- The announcing RB is the block we're forging on top of;
              -- EB certification must happen within one block (this is the
              -- linear aspect of linear Leios).
              case headerStateTip headerState of
                Origin -> error "decideLeiosCertify: cannot certify on top of genesis"
                At tip -> do
                  -- the hash of the block we will be forging on top of (the announcing RB).
                  let tipHash = annTipHash tip
                      announcingRb = Leios.MkRbHash (toRawHash (Proxy @blk) tipHash)
                  mCert <- queryCert voteState announcingRb
                  case mCert of
                    Nothing -> do
                      traceWith tracer $
                        MkTraceLeiosKernel $
                          "EB downloaded but no certificate: " <> show ebPoint
                      pure Nothing
                    Just cert -> do
                      traceWith tracer $
                        TraceLeiosBlockCertified
                          { atSlot = currentSlot
                          , certifiedPoint = ebPoint
                          }
                      pure (Just (cert, Leios.pointEbHash ebPoint))

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

-- | There's no generic 'Aeson.ToJSON' for 'BlockNo'/'Point' 'blk', and adding
-- one would ripple through 'RunNode'. Piggyback on the 'Show' instances that
-- 'LedgerSupportsProtocol blk' already gives us for free (see the 'Show
-- (TraceForgeEvent blk)' deriving below, which relies on the same thing).
instance LedgerSupportsProtocol blk => Aeson.ToJSON (BlockContext blk) where
  toJSON BlockContext{bcBlockNo, bcPrevPoint} =
    Aeson.object
      [ "blockNo" .= show bcBlockNo
      , "prevPoint" .= show bcPrevPoint
      ]

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
  [Validated (GenTx blk)] ->
  blk ->
  WithEarlyExit m ()
addBlockToChainDB trace chainDB mempool currentSlot rbTxs ebTxs newBlock = do
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
            (NE.nonEmpty (map (txId . txForgetValidated) (rbTxs ++ ebTxs)))
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
    trace $ TraceAdoptedBlock currentSlot newBlock rbTxs

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
        -- current slot that we cannot get an ledger view anymore In
        -- principle, this is no problem; we can still produce a block (we use
        -- the ticked ledger state). However, we probably don't /want/ to
        -- produce a block in this case; we are most likely missing a blocks
        -- on our chain.
        trace $ TraceNoLedgerView currentSlot err
        exitEarly
      Right lv ->
        return lv

  trace $ TraceLedgerView currentSlot
  pure ledgerView

getTickedChainDepState ::
  (IOLike m, RunNode blk) =>
  TopLevelConfig blk ->
  SlotNo ->
  ExtLedgerState blk EmptyMK ->
  LedgerView (BlockProtocol blk) ->
  WithEarlyExit m (Ticked (ChainDepState (BlockProtocol blk)))
getTickedChainDepState cfg currentSlot unticked ledgerView =
  -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block for. We
  -- only need the ticked 'ChainDepState' to check the whether we're a leader.
  -- This is much cheaper than ticking the entire 'ExtLedgerState'.
  return $
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
  WithEarlyExit m (Ticked (LedgerState blk) DiffMK)
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

-- | Partition the mempool's transactions into those to go in the forged RB
-- and those for the EB to be announced, taking into account whether this
-- block certifies a previously-announced EB.
partitionMempool ::
  forall m blk.
  (IOLike m, RunNode blk) =>
  LeiosDbConnection m ->
  LeiosVoteState m ->
  Tracer m TraceLeiosKernel ->
  TopLevelConfig blk ->
  Mempool m blk ->
  SlotNo ->
  Ticked (LedgerState blk) DiffMK ->
  ExtLedgerState blk EmptyMK ->
  ReadOnlyForker' m blk ->
  m
    ( [Validated (GenTx blk)]
    , [Validated (GenTx blk)]
    , TxMeasureWithDiffTime blk
    , MempoolSnapshot blk
    , Maybe (LeiosCert, Leios.EbHash)
    )
partitionMempool leiosConn leiosVoteState leiosTracer cfg mempool currentSlot tickedLedgerState unticked forker = do
  let readTables = fmap castLedgerTables . roforkerReadTables forker . castLedgerTables

  -- Decide whether this block certifies a previously-announced EB
  mayLeiosCertAndAnnouncement <-
    decideLeiosCertify @blk
      leiosConn
      leiosVoteState
      leiosTracer
      currentSlot
      (headerState unticked)

  let rbCap = blockCapacityTxMeasure (configLedger cfg) tickedLedgerState
      ebCap = fromMaybe Data.Measure.zero $ ebCapacityTxMeasure (configLedger cfg) tickedLedgerState
  (rbTxs, ebTxs, rbTxsSize, mempoolSnapshot) <-
    case mayLeiosCertAndAnnouncement of
      Nothing -> do
        -- We don't have a Leios certificate: take transactions for an RB and
        -- an EB to be announced.
        snap <- getSnapshotFor mempool currentSlot tickedLedgerState readTables
        let (rbTxs', rbTxsSize') = snapshotTake snap rbCap
            ebTxs' =
              let (allTxs, _) = snapshotTake snap (Data.Measure.plus rbCap ebCap)
               in drop (length rbTxs') allTxs
        pure (rbTxs', ebTxs', rbTxsSize', snap)
      Just (_cert, announcedPoint) -> do
        -- We have a Leios certificate: only take transactions for a new EB, as the RB will
        -- carry the certificate and must not carry additional txs.

        -- Apply the EB's transactions onto the ledger state
        res <-
          resolveAndApplyLeiosClosure
            leiosConn
            (configLedger cfg)
            announcedPoint
            readTables
            emptyLedgerTables
            (ledgerState unticked)
        case res of
          Left err ->
            -- Should not happen: each closure tx was validated
            -- when inserted into the LeiosDb. Fail loudly.
            error $ "forkBlockForging: applyLeiosClosure failed, announcing no EB. " <> show err
          Right LeiosClosureApplied{lcaStateAfterEB, lcaClosureDiff} -> do
            -- Compose the EB closure diff (relative to the unticked
            -- parent) with the tick diff (relative to the
            -- state with the EB closure applied), so the mempool snapshot is revalidated
            -- against parent + closure + tick.
            let tickedLsAfterEB =
                  lcaClosureDiff
                    `prependDiffs` applyChainTick
                      OmitLedgerEvents
                      (configLedger cfg)
                      currentSlot
                      (forgetLedgerTables lcaStateAfterEB)
            snap <-
              getSnapshotForNoCache mempool currentSlot tickedLsAfterEB readTables
            let ebTxs' = fst (snapshotTake snap ebCap)
            pure ([], ebTxs', Data.Measure.zero, snap)

  -- force the mempool's computation before the tracer event
  _ <- evaluate (length rbTxs)
  _ <- evaluate (length ebTxs)

  pure (rbTxs, ebTxs, rbTxsSize, mempoolSnapshot, mayLeiosCertAndAnnouncement)
