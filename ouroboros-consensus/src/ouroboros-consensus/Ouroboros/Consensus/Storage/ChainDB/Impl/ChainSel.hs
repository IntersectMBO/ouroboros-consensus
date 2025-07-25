{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations involving chain selection: the initial chain selection and
-- adding a block.
module Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel
  ( addBlockAsync
  , chainSelSync
  , chainSelectionForBlock
  , initialChainSelection
  , triggerChainSelectionAsync

    -- * Exported for testing purposes
  , olderThanK
  ) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Control.Exception (assert)
import Control.Monad (forM, forM_, when)
import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor.Contravariant ((>$<))
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import Ouroboros.Consensus.Fragment.ValidatedDiff
  ( ValidatedChainDiff (..)
  )
import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
import Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HeaderValidation
  ( HeaderWithTime (..)
  , mkHeaderWithTime
  )
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
  , BlockComponent (..)
  , ChainType (..)
  , LoE (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
  ( InvalidBlockPunishment
  , noPunishment
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
  ( BlockCache
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Storage.ChainDB.Impl.Paths
  ( LookupBlockInfo
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Paths as Paths
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.VolatileDB (VolatileDB)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.AnchoredFragment
import Ouroboros.Consensus.Util.Enclose (encloseWith)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))
import Ouroboros.Network.AnchoredFragment
  ( Anchor
  , AnchoredFragment
  , AnchoredSeq (..)
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

-- | Perform the initial chain selection based on the tip of the ImmutableDB
-- and the contents of the VolatileDB.
--
-- Returns the chosen validated chain and corresponding ledger.
initialChainSelection ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  ) =>
  ImmutableDB m blk ->
  VolatileDB m blk ->
  LedgerDB.LedgerDB' m blk ->
  ResourceRegistry m ->
  Tracer m (TraceInitChainSelEvent blk) ->
  TopLevelConfig blk ->
  StrictTVar m (WithFingerprint (InvalidBlocks blk)) ->
  LoE () ->
  m (ChainAndLedger m blk)
initialChainSelection
  immutableDB
  volatileDB
  lgrDB
  rr
  tracer
  cfg
  varInvalid
  loE = do
    -- TODO: Improve the user experience by trimming any potential
    -- blocks from the future from the VolatileDB.
    --
    -- When we perform chain selection, it is theoretically possible
    -- that the blocks in the VolatileDB are from the future, if for
    -- some reason the clock of the node was set back (by a
    -- significant amount of time). This is a rare situation, but can
    -- arise for instance if the clock of the node was set in the
    -- \**far** future. In this case, node will be disconnected from
    -- other peers when diffusing these blocks. Once the node is
    -- restarted with a synchronized clock, it will diffuse said
    -- blocks from the future again (assuming they're still from the
    -- future after restart), which will cause other nodes to
    -- disconnect. By trimming blocks from the future from the
    -- VolatileDB we can prevent this inconvenient, albeit extremely
    -- rare, situation. However, it does not pose any security risk,
    -- and a node operator can correct the problem by either wiping
    -- out the VolatileDB or waiting enough time until the blocks are
    -- not from the **far** future anymore.
    (i :: Anchor blk, succsOf) <- atomically $ do
      invalid <- forgetFingerprint <$> readTVar varInvalid
      (,)
        <$> ImmutableDB.getTipAnchor immutableDB
        <*> ( ignoreInvalidSuc volatileDB invalid
                <$> VolatileDB.filterByPredecessor volatileDB
            )

    -- This is safe: the LedgerDB tip doesn't change in between the previous
    -- atomically block and this call to 'withTipForker'.
    --
    -- We don't use 'LedgerDB.withTipForker' here, because 'curForker' might be
    -- returned as part of the selected chain.
    curForker <-
      LedgerDB.getForkerAtTarget lgrDB rr VolatileTip >>= \case
        Left{} -> error "Unreachable, VolatileTip MUST be in the LedgerDB"
        Right frk -> pure frk

    chains <- constructChains i succsOf

    -- We use the empty fragment anchored at @i@ as the current chain (and
    -- ledger) and the default in case there is no better candidate.
    let curChain = Empty (AF.castAnchor i)
    curChainAndLedger <- VF.newM curChain curForker

    case NE.nonEmpty (filter (preferAnchoredCandidate bcfg curChain) chains) of
      -- If there are no candidates, no chain selection is needed
      Nothing -> return curChainAndLedger
      Just chains' ->
        chainSelection' curChainAndLedger chains' >>= \case
          -- The returned forker will be closed in 'openDBInternal'.
          Nothing -> pure curChainAndLedger
          Just newChain -> forkerClose curForker >> toChainAndLedger newChain
   where
    bcfg :: BlockConfig blk
    bcfg = configBlock cfg

    SecurityParam k = configSecurityParam cfg

    -- \| Turn the 'ValidatedChainDiff' into a 'ChainAndLedger'.
    --
    -- The rollback of the 'ChainDiff' must be empty, as the suffix starts
    -- from the tip of the ImmutableDB, and we can't roll back past that tip.
    -- This is guaranteed by the fact that all constructed candidates start
    -- from this tip.
    toChainAndLedger ::
      ValidatedChainDiff (Header blk) (Forker' m blk) ->
      m (ChainAndLedger m blk)
    toChainAndLedger (ValidatedChainDiff chainDiff ledger) =
      case chainDiff of
        ChainDiff rollback suffix
          | rollback == 0 ->
              VF.newM suffix ledger
          | otherwise ->
              error "constructed an initial chain with rollback"

    -- \| Use the VolatileDB to construct all chains starting from the tip of
    -- the ImmutableDB.
    constructChains ::
      Anchor blk ->
      -- \^ Tip of the ImmutableDB, @i@
      (ChainHash blk -> Set (HeaderHash blk)) ->
      m [AnchoredFragment (Header blk)]
    constructChains i succsOf =
      flip evalStateT Map.empty $
        mapM constructChain suffixesAfterI
     where
      -- We now prevent selecting more than k blocks in maximalCandidates
      -- when the LoE is enabled to avoid circumventing the LoE on startup.
      -- Shutting down a syncing node and then restarting it should not cause
      -- it to select the longest chain the VolDB, since that chain might be
      -- adversarial (ie the LoE did not allow the node to select it when it
      -- arrived).
      suffixesAfterI :: [NonEmpty (HeaderHash blk)]
      suffixesAfterI = Paths.maximalCandidates succsOf (unNonZero <$> limit) (AF.anchorToPoint i)
       where
        limit = case loE of
          LoEDisabled -> Nothing
          LoEEnabled () -> Just k

      constructChain ::
        NonEmpty (HeaderHash blk) ->
        StateT
          (Map (HeaderHash blk) (Header blk))
          m
          (AnchoredFragment (Header blk))
      constructChain hashes =
        AF.fromOldestFirst (AF.castAnchor i)
          <$> mapM (getKnownHeaderThroughCache volatileDB) (NE.toList hashes)

    -- \| Perform chain selection (including validation) on the given
    -- candidates.
    --
    -- PRECONDITION: all candidates are anchored at @i@.
    --
    -- PRECONDITION: all candidates must be preferred over the current chain.
    chainSelection' ::
      HasCallStack =>
      ChainAndLedger m blk ->
      -- \^ The current chain and ledger, corresponding to
      -- @i@.
      NonEmpty (AnchoredFragment (Header blk)) ->
      -- \^ Candidates anchored at @i@
      m (Maybe (ValidatedChainDiff (Header blk) (Forker' m blk)))
    chainSelection' curChainAndLedger candidates =
      atomically (forkerCurrentPoint ledger) >>= \curpt ->
        assert (all ((curpt ==) . castPoint . AF.anchorPoint) candidates) $
          assert (all (preferAnchoredCandidate bcfg curChain) candidates) $ do
            cse <- chainSelEnv
            chainSelection cse rr (Diff.extend <$> candidates)
     where
      curChain = VF.validatedFragment curChainAndLedger
      ledger = VF.validatedLedger curChainAndLedger
      chainSelEnv = do
        varTentativeState <- newTVarIO (initialTentativeHeaderState (Proxy @blk))
        varTentativeHeader <- newTVarIO SNothing
        pure
          ChainSelEnv
            { lgrDB
            , bcfg
            , varInvalid
            , blockCache = BlockCache.empty
            , curChainAndLedger
            , validationTracer = InitChainSelValidation >$< tracer
            , -- initial chain selection is not concerned about pipelining
              pipeliningTracer = nullTracer
            , varTentativeState
            , varTentativeHeader
            , punish = Nothing
            , getTentativeFollowers = pure []
            }

-- | Add a block to the ChainDB, /asynchronously/.
--
-- This adds a 'BlockToAdd' corresponding to the given block to the
-- 'cdbChainSelQueue' queue. The entries in that queue are processed using
-- 'chainSelSync', see that function for more information.
--
-- When the queue is full, this function will still block.
--
-- Compared to a synchronous approach, the asynchronous counterpart
-- doesn't have the following disadvantage: when a thread adding a
-- block to the ChainDB is killed, which can happen when disconnecting
-- from the corresponding node, we might have written the block to
-- disk, but not updated the corresponding in-memory state (e.g., that
-- of the VolatileDB), leaving both out of sync.
--
-- With this asynchronous approach, threads adding blocks asynchronously can
-- be killed without worries, the background thread processing the blocks
-- synchronously won't be killed. Only when the whole ChainDB shuts down will
-- that background thread get killed. But since there will be no more
-- in-memory state, it can't get out of sync with the file system state. On
-- the next startup, a correct in-memory state will be reconstructed from the
-- file system state.
--
-- PRECONDITON: the block to be added must not be from the future.
-- See 'Ouroboros.Consensus.Storage.ChainDB.API.addBlockAsync'.
addBlockAsync ::
  forall m blk.
  (IOLike m, HasHeader blk) =>
  ChainDbEnv m blk ->
  InvalidBlockPunishment m ->
  blk ->
  m (AddBlockPromise m blk)
addBlockAsync CDB{cdbTracer, cdbChainSelQueue} =
  addBlockToAdd (TraceAddBlockEvent >$< cdbTracer) cdbChainSelQueue

-- | Schedule reprocessing of blocks postponed by the LoE.
triggerChainSelectionAsync ::
  forall m blk.
  IOLike m =>
  ChainDbEnv m blk ->
  m (ChainSelectionPromise m)
triggerChainSelectionAsync CDB{cdbTracer, cdbChainSelQueue} =
  addReprocessLoEBlocks (TraceAddBlockEvent >$< cdbTracer) cdbChainSelQueue

-- | Add a block to the ChainDB, /synchronously/.
--
-- This is the only operation that actually changes the ChainDB. It will store
-- the block on disk and trigger chain selection, possibly switching to a
-- fork.
--
-- When the slot of the block is > the current slot, a chain selection will be
-- scheduled in the slot of the block.
chainSelSync ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , HasCallStack
  ) =>
  ChainDbEnv m blk ->
  ChainSelMessage m blk ->
  Electric m ()
-- Reprocess headers that were postponed by the LoE.
-- When we try to extend the current chain with a new block beyond the LoE
-- limit, the block will be added to the DB without modifying the chain.
-- When the LoE fragment advances later, these blocks have to be scheduled
-- for ChainSel again, but this does not happen automatically.
-- So we fetch all direct successors of each of the chain's blocks and run
-- ChainSel for them.
-- We run a background thread that polls the candidate fragments and sends
-- 'ChainSelReprocessLoEBlocks' whenever we receive a new header or lose a
-- peer.
--
-- Note that we do this even when we are caught-up, as we might want to select
-- blocks that were originally postponed by the LoE, but can be adopted once we
-- conclude that we are caught-up (and hence are longer bound by the LoE).
chainSelSync cdb@CDB{..} (ChainSelReprocessLoEBlocks varProcessed) = do
  (succsOf, chain) <- lift $ atomically $ do
    invalid <- forgetFingerprint <$> readTVar cdbInvalid
    (,)
      <$> ( ignoreInvalidSuc cdbVolatileDB invalid
              <$> VolatileDB.filterByPredecessor cdbVolatileDB
          )
      <*> Query.getCurrentChain cdb
  let
    chainHashes :: [ChainHash blk]
    chainHashes =
      castHash . pointHash
        <$> AF.anchorPoint chain : (blockPoint <$> AF.toOldestFirst chain)
    chainHashesSet :: Set (ChainHash blk)
    chainHashesSet = Set.fromList chainHashes

    -- The hashes of all immediate successor blocks of blocks on our chain
    -- (including the anchor) without hashes on our chain.
    loeHashes :: [HeaderHash blk]
    loeHashes =
      [ loeHash
      | hashSel <- chainHashes
      , loeHash <- Set.toList $ succsOf hashSel
      , BlockHash loeHash `Set.notMember` chainHashesSet
      ]
  for_ loeHashes $ \hash -> do
    hdr <- lift $ VolatileDB.getKnownBlockComponent cdbVolatileDB GetHeader hash
    chainSelectionForBlock cdb BlockCache.empty hdr noPunishment
  lift $ atomically $ putTMVar varProcessed ()
chainSelSync cdb@CDB{..} (ChainSelAddBlock BlockToAdd{blockToAdd = b, ..}) = do
  (isMember, invalid, curChain) <-
    lift $
      atomically $
        (,,)
          <$> VolatileDB.getIsMember cdbVolatileDB
          <*> (forgetFingerprint <$> readTVar cdbInvalid)
          <*> Query.getCurrentChain cdb

  let immBlockNo = AF.anchorBlockNo curChain

  -- We follow the steps from section "## Adding a block" in ChainDB.md

  if
    | olderThanK hdr isEBB immBlockNo -> do
        lift $ traceWith addBlockTracer $ IgnoreBlockOlderThanK (blockRealPoint b)
        lift $ deliverWrittenToDisk False
    | isMember (blockHash b) -> do
        lift $ traceWith addBlockTracer $ IgnoreBlockAlreadyInVolatileDB (blockRealPoint b)
        lift $ deliverWrittenToDisk True
    | Just (InvalidBlockInfo reason _) <- Map.lookup (blockHash b) invalid -> do
        lift $ traceWith addBlockTracer $ IgnoreInvalidBlock (blockRealPoint b) reason
        lift $ deliverWrittenToDisk False

        -- We wouldn't know the block is invalid if its prefix was invalid,
        -- hence 'InvalidBlockPunishment.BlockItself'.
        lift $
          InvalidBlockPunishment.enact
            blockPunish
            InvalidBlockPunishment.BlockItself

    -- The remaining cases
    | otherwise -> do
        let traceEv = AddedBlockToVolatileDB (blockRealPoint b) (blockNo b) isEBB
        lift $
          encloseWith (traceEv >$< addBlockTracer) $
            VolatileDB.putBlock cdbVolatileDB b
        lift $ deliverWrittenToDisk True
        chainSelectionForBlock cdb (BlockCache.singleton b) hdr blockPunish

  newTip <- lift $ atomically $ Query.getTipPoint cdb

  lift $ deliverProcessed newTip
 where
  addBlockTracer :: Tracer m (TraceAddBlockEvent blk)
  addBlockTracer = TraceAddBlockEvent >$< cdbTracer

  hdr :: Header blk
  hdr = getHeader b

  isEBB :: IsEBB
  isEBB = headerToIsEBB hdr

  -- \| Fill in the 'TMVar' for the 'varBlockWrittenToDisk' of the block's
  -- 'AddBlockPromise' with the given 'Bool'.
  deliverWrittenToDisk :: Bool -> m ()
  deliverWrittenToDisk writtenToDisk =
    atomically $
      putTMVar varBlockWrittenToDisk writtenToDisk

  -- \| Fill in the 'TMVar' for the 'varBlockProcessed' of the block's
  -- 'AddBlockPromise' with the given tip.
  deliverProcessed :: Point blk -> m ()
  deliverProcessed tip =
    atomically $
      putTMVar varBlockProcessed (SuccesfullyAddedBlock tip)

-- | Return 'True' when the given header should be ignored when adding it
-- because it is too old, i.e., we wouldn't be able to switch to a chain
-- containing the corresponding block because its block number is more than
-- @k@ blocks or exactly @k@ blocks back.
--
-- Special case: the header corresponds to an EBB which has the same block
-- number as the block @k@ blocks back (the most recent \"immutable\" block).
-- As EBBs share their block number with the block before them, the EBB is not
-- too old in that case and can be adopted as part of our chain.
--
-- This special case can occur, for example, when the VolatileDB is empty
-- (because of corruption). The \"immutable\" block is then also the tip of
-- the chain. If we then try to add the EBB after it, it will have the same
-- block number, so we must allow it.
olderThanK ::
  HasHeader (Header blk) =>
  -- | Header of the block to add
  Header blk ->
  -- | Whether the block is an EBB or not
  IsEBB ->
  -- | The block number of the most recent \"immutable\" block, i.e., the
  -- block @k@ blocks back.
  WithOrigin BlockNo ->
  Bool
olderThanK hdr isEBB immBlockNo
  | NotOrigin bNo == immBlockNo
  , isEBB == IsEBB =
      False
  | otherwise =
      NotOrigin bNo <= immBlockNo
 where
  bNo = blockNo hdr

-- | When we switch to a new selected chain, we are either extending the current
-- chain by adding blocks on top or we are switching to a fork.
data ChainSwitchType = AddingBlocks | SwitchingToAFork
  deriving (Show, Eq)

-- | Trigger chain selection for the given block.
--
-- PRECONDITION: the block is in the VolatileDB.
--
-- PRECONDITION: the slot of the block <= the current (wall) slot
--
-- = Constructing candidate fragments
--
-- The VolatileDB keeps a \"successors\" map in memory, telling us the hashes
-- of the known successors of any block, but it does not keep /headers/ in
-- memory, which are needed to construct candidate fargments. We try to reuse
-- the headers from the current chain fragment where possible, but it will not
-- contain all needed headers. This means that we will need to read some
-- blocks from disk and extract their headers. Under normal circumstances this
-- does not matter too much; although this will be done every time we add a
-- block, the expected number of headers to read from disk is very small:
--
-- * None if we stay on the current chain and this is just the next block
-- * A handful if we stay on the current chain and the block we just received
--   was a missing block and we already received some of its successors
-- * A handful if we switch to a short fork
--
-- This is expensive only
--
-- * on startup: in this case we need to read at least @k@ blocks from the
--   VolatileDB, and possibly more if there are some other chains in the
--   VolatileDB starting from the tip of the ImmutableDB
-- * when we switch to a distant fork
--
-- This cost is currently deemed acceptable.
chainSelectionForBlock ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , HasCallStack
  ) =>
  ChainDbEnv m blk ->
  BlockCache blk ->
  Header blk ->
  InvalidBlockPunishment m ->
  Electric m ()
chainSelectionForBlock cdb@CDB{..} blockCache hdr punish = electric $ withRegistry $ \rr -> do
  (invalid, succsOf, lookupBlockInfo, curChain, tipPoint) <-
    atomically $
      (,,,,)
        <$> (forgetFingerprint <$> readTVar cdbInvalid)
        <*> VolatileDB.filterByPredecessor cdbVolatileDB
        <*> VolatileDB.getBlockInfo cdbVolatileDB
        <*> Query.getCurrentChain cdb
        <*> Query.getTipPoint cdb
  -- This is safe: the LedgerDB tip doesn't change in between the previous
  -- atomically block and this call to 'withTipForker'.
  LedgerDB.withTipForker cdbLedgerDB rr $ \curForker -> do
    curChainAndLedger :: ChainAndLedger m blk <-
      -- The current chain we're working with here is not longer than @k@
      -- blocks (see 'getCurrentChain' and 'cdbChain'), which is easier to
      -- reason about when doing chain selection, etc.
      assert (fromIntegral (AF.length curChain) <= unNonZero k) $
        VF.newM curChain curForker

    let
      immBlockNo :: WithOrigin BlockNo
      immBlockNo = AF.anchorBlockNo curChain

      -- Let these two functions ignore invalid blocks
      lookupBlockInfo' = ignoreInvalid cdb invalid lookupBlockInfo
      succsOf' = ignoreInvalidSuc cdb invalid succsOf

    -- The preconditions
    assert (isJust $ lookupBlockInfo (headerHash hdr)) $ return ()

    let
      -- Trim the LoE fragment to be anchored in the immutable tip, ie the
      -- anchor of @curChain@. In particular, this establishes the property that
      -- it intersects with the current chain.
      sanitizeLoEFrag ::
        AnchoredFragment (HeaderWithTime blk) ->
        AnchoredFragment (HeaderWithTime blk)
      sanitizeLoEFrag loeFrag0 =
        case AF.splitAfterPoint loeFrag0 (AF.anchorPoint curChain) of
          Just (_, frag) -> frag
          -- As the (unsanitized) LoE fragment is rooted in a recent immutable
          -- tip, this case means that it doesn't intersect with the current
          -- chain. This can temporarily be the case; we are conservative and
          -- use the empty fragment anchored at the immutable tip for chain
          -- selection.
          Nothing -> AF.Empty $ AF.castAnchor $ AF.anchor curChain

    loeFrag <- fmap sanitizeLoEFrag <$> cdbLoE

    traceWith
      addBlockTracer
      (ChainSelectionLoEDebug curChain (AF.mapAnchoredFragment hwtHeader <$> loeFrag))

    if
      -- The chain might have grown since we added the block such that the
      -- block is older than @k@.
      | olderThanK hdr isEBB immBlockNo -> do
          traceWith addBlockTracer $ IgnoreBlockOlderThanK p

      -- The block is invalid
      | Just (InvalidBlockInfo reason _) <- Map.lookup (headerHash hdr) invalid -> do
          traceWith addBlockTracer $ IgnoreInvalidBlock p reason

          -- We wouldn't know the block is invalid if its prefix was invalid,
          -- hence 'InvalidBlockPunishment.BlockItself'.
          InvalidBlockPunishment.enact
            punish
            InvalidBlockPunishment.BlockItself

      -- The block fits onto the end of our current chain
      | pointHash tipPoint == headerPrevHash hdr -> do
          -- ### Add to current chain
          traceWith addBlockTracer (TryAddToCurrentChain p)
          addToCurrentChain rr succsOf' curChainAndLedger loeFrag

      -- The block is reachable from the current selection
      -- and it doesn't fit after the current selection
      | Just diff <- Paths.isReachable lookupBlockInfo' curChain p -> do
          -- ### Switch to a fork
          traceWith addBlockTracer (TrySwitchToAFork p diff)
          switchToAFork rr succsOf' lookupBlockInfo' curChainAndLedger loeFrag diff

      -- We cannot reach the block from the current selection
      | otherwise -> do
          -- ### Store but don't change the current chain
          traceWith addBlockTracer (StoreButDontChange p)
 where
  -- Note that we may have extended the chain, but have not trimmed it to
  -- @k@ blocks/headers. That is the job of the background thread, which
  -- will first copy the blocks/headers to trim (from the end of the
  -- fragment) from the VolatileDB to the ImmutableDB.

  SecurityParam k = configSecurityParam cdbTopLevelConfig

  p :: RealPoint blk
  p = headerRealPoint hdr

  isEBB :: IsEBB
  isEBB = headerToIsEBB hdr

  addBlockTracer :: Tracer m (TraceAddBlockEvent blk)
  addBlockTracer = TraceAddBlockEvent >$< cdbTracer

  mkChainSelEnv :: ChainAndLedger m blk -> ChainSelEnv m blk
  mkChainSelEnv curChainAndLedger =
    ChainSelEnv
      { lgrDB = cdbLedgerDB
      , bcfg = configBlock cdbTopLevelConfig
      , varInvalid = cdbInvalid
      , varTentativeState = cdbTentativeState
      , varTentativeHeader = cdbTentativeHeader
      , getTentativeFollowers =
          filter ((TentativeChain ==) . fhChainType) . Map.elems
            <$> readTVar cdbFollowers
      , blockCache = blockCache
      , curChainAndLedger = curChainAndLedger
      , validationTracer =
          TraceAddBlockEvent . AddBlockValidation >$< cdbTracer
      , pipeliningTracer =
          TraceAddBlockEvent . PipeliningEvent >$< cdbTracer
      , punish = Just (p, punish)
      }

  -- \| PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
  -- the current chain.
  addToCurrentChain ::
    HasCallStack =>
    ResourceRegistry m ->
    (ChainHash blk -> Set (HeaderHash blk)) ->
    ChainAndLedger m blk ->
    -- \^ The current chain and ledger
    LoE (AnchoredFragment (HeaderWithTime blk)) ->
    -- \^ LoE fragment
    m ()
  addToCurrentChain rr succsOf curChainAndLedger loeFrag = do
    -- Extensions of @B@ that do not exceed the LoE
    let suffixesAfterB = Paths.maximalCandidates succsOf Nothing (realPointToPoint p)

    -- Fragments that are anchored at @curHead@, i.e. suffixes of the
    -- current chain.
    candidates <- case NE.nonEmpty suffixesAfterB of
      -- If there are no suffixes after @b@, just use the suffix just
      -- containing @b@ as the sole candidate.
      Nothing ->
        return $ AF.fromOldestFirst curHead [hdr] NE.:| []
      Just suffixesAfterB' ->
        -- We can start with an empty cache, because we're only looking
        -- up the headers /after/ b, so they won't be on the current
        -- chain.
        flip evalStateT Map.empty $ forM suffixesAfterB' $ \hashes -> do
          hdrs <-
            mapM (getKnownHeaderThroughCache cdbVolatileDB) $
              NE.toList hashes
          return $ AF.fromOldestFirst curHead (hdr : hdrs)

    let chainDiffs =
          NE.nonEmpty $
            filter (preferAnchoredCandidate (bcfg chainSelEnv) curChain . Diff.getSuffix) $
              fmap (trimToLoE loeFrag curChainAndLedger) $
                fmap Diff.extend $
                  NE.toList candidates
    -- All candidates are longer than the current chain, so they will be
    -- preferred over it, /unless/ the block we just added is an EBB,
    -- which has the same 'BlockNo' as the block before it, so when
    -- using the 'BlockNo' as the proxy for the length (note that some
    -- protocols might do it differently), the candidate with the EBB
    -- appended will not be preferred over the current chain.
    --
    -- The consequence of this is that when adding an EBB, it will not
    -- be selected by chain selection and thus not appended to the chain
    -- until the block after it is added, which will again result in a
    -- candidate preferred over the current chain. In this case, the
    -- candidate will be a two-block (the EBB and the new block)
    -- extension of the current chain.
    case chainDiffs of
      Nothing -> return ()
      Just chainDiffs' ->
        chainSelection chainSelEnv rr chainDiffs' >>= \case
          Nothing ->
            return ()
          Just validatedChainDiff ->
            switchTo
              validatedChainDiff
              (varTentativeHeader chainSelEnv)
              AddingBlocks
   where
    chainSelEnv = mkChainSelEnv curChainAndLedger
    curChain = VF.validatedFragment curChainAndLedger
    curHead = AF.headAnchor curChain

  -- \| Trim the given candidate fragment to respect the LoE.
  --
  -- The returned fragment is such that:
  --
  -- - It is a prefix of the given fragment.
  -- - If it contains the tip of the LoE fragment, then it contains at most
  --   @k@ block after it.
  -- - If it does not contain the tip of the LoE fragment, then it is included
  --   in the LoE fragment.
  --
  -- The fragment is represented by the current chain and a diff with that
  -- current chain. It is tempting to only consider the suffix of the diff,
  -- but that would be incorrect, because the diff might not intersect with
  -- the LoE fragment, because the diff suffix is anchored somewhere on the
  -- current chain and LoE frag's tip might be older than that anchor.
  --
  -- PRECONDITIONS:
  --
  -- 1. The given 'ChainDiff' can apply on top of the given 'ChainAndLedger'.
  -- 2. The LoE fragment intersects with the current selection.
  trimToLoE ::
    (HasHeader blk', HeaderHash blk ~ HeaderHash blk') =>
    LoE (AnchoredFragment blk') ->
    ChainAndLedger m blk ->
    ChainDiff (Header blk) ->
    ChainDiff (Header blk)
  trimToLoE LoEDisabled _ diff = diff
  trimToLoE (LoEEnabled loe) curChain diff =
    case Diff.apply (VF.validatedFragment curChain) diff of
      Nothing ->
        error
          "trimToLoE: precondition 1 violated: the given 'ChainDiff' must apply on top of the given 'ChainAndLedger'"
      Just cand ->
        case AF.intersect cand loe of
          Nothing ->
            error
              "trimToLoE: precondition 2 violated: the LoE fragment must intersect with the current selection"
          Just (candPrefix, _, candSuffix, loeSuffix) ->
            let trimmedCandSuffix = AF.takeOldest (fromIntegral $ unNonZero k) candSuffix
                trimmedCand =
                  if AF.null loeSuffix
                    then fromJust $ AF.join candPrefix trimmedCandSuffix
                    else candPrefix
             in Diff.diff (VF.validatedFragment curChain) trimmedCand

  -- \| We have found a 'ChainDiff' through the VolatileDB connecting the new
  -- block to the current chain. We'll call the intersection/anchor @x@.
  --
  -- We try to extend this path by looking for forks that start with the
  -- given block, then we do chain selection and /possibly/ try to switch to
  -- a new fork.
  switchToAFork ::
    HasCallStack =>
    ResourceRegistry m ->
    (ChainHash blk -> Set (HeaderHash blk)) ->
    LookupBlockInfo blk ->
    ChainAndLedger m blk ->
    -- \^ The current chain (anchored at @i@) and ledger
    LoE (AnchoredFragment (HeaderWithTime blk)) ->
    -- \^ LoE fragment
    ChainDiff (HeaderFields blk) ->
    -- \^ Header fields for @(x,b]@
    m ()
  switchToAFork rr succsOf lookupBlockInfo curChainAndLedger loeFrag diff = do
    -- We use a cache to avoid reading the headers from disk multiple
    -- times in case they're part of multiple forks that go through @b@.
    let initCache = Map.singleton (headerHash hdr) hdr
    chainDiffs <-
      -- 5. Filter out candidates that are not preferred over the current
      -- chain.
      --
      -- The suffixes all fork off from the current chain within @k@
      -- blocks, so it satisfies the precondition of 'preferCandidate'.
      fmap
        ( filter
            ( preferAnchoredCandidate (bcfg chainSelEnv) curChain
                . Diff.getSuffix
            )
        )
        -- 4. Trim fragments so that they follow the LoE, that is, they
        -- extend the LoE or are extended by the LoE. Filter them out
        -- otherwise.
        . fmap (fmap (trimToLoE loeFrag curChainAndLedger))
        -- 3. Translate the 'HeaderFields' to 'Header' by reading the
        -- headers from disk.
        . flip evalStateT initCache
        . mapM translateToHeaders
        -- 2. Filter out candidates that are shorter than the current
        -- chain. We don't want to needlessly read the headers from disk
        -- for those candidates.
        . NE.filter (not . Diff.rollbackExceedsSuffix)
        -- 1. Extend the diff with candidates fitting on @B@ and not exceeding the LoE
        . Paths.extendWithSuccessors succsOf lookupBlockInfo
        $ diff

    case NE.nonEmpty chainDiffs of
      -- No candidates preferred over the current chain
      Nothing -> return ()
      Just chainDiffs' ->
        chainSelection chainSelEnv rr chainDiffs' >>= \case
          Nothing ->
            return ()
          Just validatedChainDiff ->
            switchTo
              validatedChainDiff
              (varTentativeHeader chainSelEnv)
              SwitchingToAFork
   where
    chainSelEnv = mkChainSelEnv curChainAndLedger
    curChain = VF.validatedFragment curChainAndLedger

  mkSelectionChangedInfo ::
    AnchoredFragment (Header blk) ->
    -- \^ old chain
    AnchoredFragment (Header blk) ->
    -- \^ new chain
    ExtLedgerState blk EmptyMK ->
    -- \^ new tip
    SelectionChangedInfo blk
  mkSelectionChangedInfo oldChain newChain newTip =
    SelectionChangedInfo
      { newTipPoint = castRealPoint tipPoint
      , newTipEpoch = tipEpoch
      , newTipSlotInEpoch = tipSlotInEpoch
      , newTipTrigger = p
      , newTipSelectView
      , oldTipSelectView =
          selectView (configBlock cfg)
            <$> eitherToMaybe (AF.head oldChain)
      }
   where
    cfg :: TopLevelConfig blk
    cfg = cdbTopLevelConfig

    ledger :: LedgerState blk EmptyMK
    ledger = ledgerState newTip

    summary :: History.Summary (HardForkIndices blk)
    summary =
      hardForkSummary
        (configLedger cfg)
        ledger

    (tipPoint, (tipEpoch, tipSlotInEpoch), newTipSelectView) =
      case AF.head newChain of
        Left _anchor -> error "cannot have switched to an empty chain"
        Right tipHdr ->
          let query = History.slotToEpoch' (blockSlot tipHdr)
              tipEpochData = History.runQueryPure query summary
              sv = selectView (configBlock cfg) tipHdr
           in (blockRealPoint tipHdr, tipEpochData, sv)

  -- \| Try to apply the given 'ChainDiff' on the current chain fragment. The
  -- 'LedgerDB' is updated in the same transaction.
  --
  -- Note that we /cannot/ have switched to a different current chain in the
  -- meantime, since this function will only be called by a single
  -- background thread.
  --
  -- It /is/ possible that the background thread copying headers older than
  -- @k@ from the VolatileDB to the ImmutableDB has removed some headers
  -- from the beginning of the current chain fragment, but does not affect
  -- us, as we cannot roll back more than @k@ headers anyway.
  switchTo ::
    HasCallStack =>
    ValidatedChainDiff (Header blk) (Forker' m blk) ->
    -- \^ Chain and ledger to switch to
    StrictTVar m (StrictMaybe (Header blk)) ->
    -- \^ Tentative header
    ChainSwitchType ->
    m ()
  switchTo vChainDiff varTentativeHeader chainSwitchType = do
    traceWith addBlockTracer $
      ChangingSelection $
        castPoint $
          AF.headPoint $
            getSuffix $
              getChainDiff vChainDiff
    (curChain, newChain, events, prevTentativeHeader, newLedger) <- atomically $ do
      InternalChain curChain curChainWithTime <- readTVar cdbChain -- Not Query.getCurrentChain!
      curLedger <- getVolatileTip cdbLedgerDB
      newLedger <- forkerGetLedgerState newForker
      case Diff.apply curChain chainDiff of
        -- Impossible, as described in the docstring
        Nothing ->
          error "chainDiff doesn't fit onto current chain"
        Just newChain -> do
          let lcfg = configLedger cdbTopLevelConfig
              diffWithTime =
                -- the new ledger state can translate the slots of the new
                -- headers
                Diff.map
                  ( mkHeaderWithTime
                      lcfg
                      (ledgerState newLedger)
                  )
                  chainDiff
              newChainWithTime =
                case Diff.apply curChainWithTime diffWithTime of
                  Nothing -> error "chainDiff failed for HeaderWithTime"
                  Just x -> x

          writeTVar cdbChain $ InternalChain newChain newChainWithTime
          forkerCommit newForker

          -- Inspect the new ledger for potential problems
          let events :: [LedgerEvent blk]
              events =
                inspectLedger
                  cdbTopLevelConfig
                  (ledgerState curLedger)
                  (ledgerState newLedger)

          -- Clear the tentative header
          prevTentativeHeader <- swapTVar varTentativeHeader SNothing

          case chainSwitchType of
            -- When adding blocks, the intersection point of the old and new
            -- tentative/selected chain is not receding, in which case
            -- `fhSwitchFork` is unnecessary. In the case of pipelining a
            -- block, it would even result in rolling back by one block and
            -- rolling forward again.
            AddingBlocks -> pure ()
            SwitchingToAFork -> do
              -- Update the followers
              followerHandles <- Map.elems <$> readTVar cdbFollowers
              -- The suffix of @curChain@ that we are going to orphan by
              -- adopting @chainDiff@.
              let oldSuffix = AF.anchorNewest (getRollback chainDiff) curChain
              forM_ followerHandles $ \hdl -> fhSwitchFork hdl oldSuffix

          return (curChain, newChain, events, prevTentativeHeader, newLedger)
    let mkTraceEvent = case chainSwitchType of
          AddingBlocks -> AddedToCurrentChain
          SwitchingToAFork -> SwitchedToAFork
        selChangedInfo = mkSelectionChangedInfo curChain newChain newLedger
    traceWith addBlockTracer $
      mkTraceEvent events selChangedInfo curChain newChain
    whenJust (strictMaybeToMaybe prevTentativeHeader) $
      traceWith $
        PipeliningEvent . OutdatedTentativeHeader >$< addBlockTracer

    forkerClose newForker
   where
    ValidatedChainDiff chainDiff newForker = vChainDiff

  -- \| We have a new block @b@ that doesn't fit onto the current chain, but
  -- we have found a 'ChainDiff' connecting it to the current chain via
  -- intersection point @x@. We may also have extended that 'ChainDiff' with
  -- more blocks fitting onto @b@, i.e., a suffix @s@.
  --
  -- We now translate that 'ChainDiff' from 'HeaderFields' to 'Header's by
  -- reading the headers from disk.
  --
  -- Note that we need to read the headers corresponding to the hashes
  -- @(x,b)@ and @(b,?]@ from disk. Not for @b@, as that's in our cache.
  translateToHeaders ::
    ChainDiff (HeaderFields blk) ->
    StateT
      (Map (HeaderHash blk) (Header blk))
      m
      (ChainDiff (Header blk))
  -- \^ Fork, anchored at @x@, contains (the header of) @b@ and ends
  -- with the suffix @s@.
  translateToHeaders =
    Diff.mapM (getKnownHeaderThroughCache cdbVolatileDB . headerFieldHash)

-- | Check whether the header for the hash is in the cache, if not, get
-- the corresponding header from the VolatileDB and store it in the cache.
--
-- PRECONDITION: the header (block) must exist in the VolatileDB.
getKnownHeaderThroughCache ::
  (MonadThrow m, HasHeader blk) =>
  VolatileDB m blk ->
  HeaderHash blk ->
  StateT (Map (HeaderHash blk) (Header blk)) m (Header blk)
getKnownHeaderThroughCache volatileDB hash =
  gets (Map.lookup hash) >>= \case
    Just hdr -> return hdr
    Nothing -> do
      hdr <- lift $ VolatileDB.getKnownBlockComponent volatileDB GetHeader hash
      modify (Map.insert hash hdr)
      return hdr

-- | Environment used by 'chainSelection' and related functions.
data ChainSelEnv m blk = ChainSelEnv
  { lgrDB :: LedgerDB.LedgerDB' m blk
  , validationTracer :: Tracer m (TraceValidationEvent blk)
  , pipeliningTracer :: Tracer m (TracePipeliningEvent blk)
  , bcfg :: BlockConfig blk
  , varInvalid :: StrictTVar m (WithFingerprint (InvalidBlocks blk))
  , varTentativeState :: StrictTVar m (TentativeHeaderState blk)
  , varTentativeHeader :: StrictTVar m (StrictMaybe (Header blk))
  , getTentativeFollowers :: STM m [FollowerHandle m blk]
  , blockCache :: BlockCache blk
  , curChainAndLedger :: ChainAndLedger m blk
  , punish :: Maybe (RealPoint blk, InvalidBlockPunishment m)
  -- ^ The block that this chain selection invocation is processing, and the
  -- punish action for the peer that sent that block; see
  -- 'InvalidBlockPunishment'.
  --
  -- One subtlety:
  --
  -- o If a BlockFetch client adds an invalid block but that block isn't
  --   part of any desirable paths through the VolDB, then we won't attempt
  --   to validate it and so we won't discover it's invalid. The peer will
  --   not be punished. This seems acceptable, since it means we have turned
  --   our focus to a another peer offering better blocks and so this peer
  --   is no longer causing us BlockFetch work.
  --
  -- Thus invalid blocks can be skipped entirely. This is part of
  -- the reason we bothered to restrict the expressiveness of the
  -- 'InvalidBlockPunishment' combinators.
  }

-- | Perform chain selection with the given candidates. If a validated
-- candidate was chosen to replace the current chain, return it along with the
-- corresponding ledger.
--
-- PRECONDITION: all candidates must be preferred over the current chain.
--
-- PRECONDITION: the candidate chain diffs must fit on the (given) current
-- chain.
chainSelection ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , HasCallStack
  ) =>
  ChainSelEnv m blk ->
  ResourceRegistry m ->
  NonEmpty (ChainDiff (Header blk)) ->
  -- | The (valid) chain diff and corresponding LedgerDB that was selected,
  -- or 'Nothing' if there is no valid chain diff preferred over the current
  -- chain.
  m (Maybe (ValidatedChainDiff (Header blk) (Forker' m blk)))
chainSelection chainSelEnv rr chainDiffs =
  assert
    ( all
        (preferAnchoredCandidate bcfg curChain . Diff.getSuffix)
        chainDiffs
    )
    $ assert
      ( all
          (isJust . Diff.apply curChain)
          chainDiffs
      )
    $ go (sortCandidates (NE.toList chainDiffs))
 where
  ChainSelEnv{..} = chainSelEnv

  curChain = VF.validatedFragment curChainAndLedger

  sortCandidates :: [ChainDiff (Header blk)] -> [ChainDiff (Header blk)]
  sortCandidates =
    sortBy (flip (compareAnchoredFragments bcfg) `on` Diff.getSuffix)

  -- 1. Take the first candidate from the list of sorted candidates
  -- 2. Validate it
  --    - If it is invalid -> discard it and go to 1 with the rest of the
  --      list.
  --    - If it is valid and has the same tip -> return it
  --    - If it is valid, but is a prefix of the original ->
  --        add it to the list, sort it and go to 1. See the comment
  --        [Ouroboros] below.
  go ::
    [ChainDiff (Header blk)] ->
    m (Maybe (ValidatedChainDiff (Header blk) (Forker' m blk)))
  go [] = return Nothing
  go (candidate : candidates0) = do
    mTentativeHeader <- setTentativeHeader
    validateCandidate chainSelEnv rr candidate >>= \case
      InsufficientSuffix ->
        -- When the body of the tentative block turns out to be invalid, we
        -- have a valid *empty* prefix, as the tentative header fits on top
        -- of the current chain.
        assert (isNothing mTentativeHeader) $ do
          candidates1 <- truncateRejectedBlocks candidates0
          go (sortCandidates candidates1)
      FullyValid validatedCandidate@(ValidatedChainDiff candidate' _) ->
        -- The entire candidate is valid
        assert (Diff.getTip candidate == Diff.getTip candidate') $
          return $
            Just validatedCandidate
      ValidPrefix candidate' -> do
        whenJust mTentativeHeader clearTentativeHeader
        -- Prefix of the candidate because it contained rejected blocks
        -- (invalid blocks). Note that the
        -- spec says go back to candidate selection,
        -- because there might still be some candidates that contain the
        -- same rejected block. To simplify the control flow, we do it
        -- differently: instead of recomputing the candidates taking
        -- rejected blocks into account, we just truncate the remaining
        -- candidates that contain rejected blocks.
        candidates1 <- truncateRejectedBlocks candidates0
        -- Only include the prefix if it is still preferred over the current
        -- chain. When the candidate is now empty because of the truncation,
        -- it will be dropped here, as it will not be preferred over the
        -- current chain.
        let candidates2
              | preferAnchoredCandidate bcfg curChain (Diff.getSuffix candidate') =
                  candidate' : candidates1
              | otherwise =
                  candidates1
        go (sortCandidates candidates2)
   where
    -- \| Set and return the tentative header, if applicable. Also return the
    -- new 'TentativeHeaderState' in case the corresponding block body turns
    -- out to be invalid.
    setTentativeHeader :: m (Maybe (Header blk, TentativeHeaderState blk))
    setTentativeHeader = do
      pipeliningResult <-
        (\ts -> isPipelineable bcfg ts candidate)
          <$> readTVarIO varTentativeState
      whenJust pipeliningResult $ \(tentativeHeader, _) -> do
        let setTentative = SetTentativeHeader tentativeHeader
        encloseWith (setTentative >$< pipeliningTracer) $
          atomically $
            writeTVar varTentativeHeader $
              SJust tentativeHeader
        -- As we are only extending the existing chain, the intersection
        -- point is not receding, in which case fhSwitchFork is not
        -- necessary.

        -- Just in case, explicitly yield to ensure that a capability (by
        -- default, the node uses just two) has the opportunity to switch
        -- to a ChainSync server thread.
        yield
      pure pipeliningResult

    -- \| Clear a tentative header that turned out to be invalid. Also, roll
    -- back the tentative followers.
    clearTentativeHeader :: (Header blk, TentativeHeaderState blk) -> m ()
    clearTentativeHeader (tentativeHeader, tentativeSt) = do
      atomically $ do
        writeTVar varTentativeHeader SNothing
        writeTVar varTentativeState tentativeSt
        forTentativeFollowers $ \followerHandle -> do
          let oldSuffix = AF.Empty (AF.headAnchor curChain) AF.:> tentativeHeader
          fhSwitchFork followerHandle oldSuffix
      traceWith pipeliningTracer $ TrapTentativeHeader tentativeHeader
     where
      forTentativeFollowers f = getTentativeFollowers >>= mapM_ f

  -- \| Truncate the given (remaining) candidates that contain rejected
  -- blocks. Discard them if they are truncated so much that they are no
  -- longer preferred over the current chain.
  --
  -- A block is rejected if it is invalid (present in 'varInvalid',
  -- i.e., 'cdbInvalid').
  truncateRejectedBlocks ::
    [ChainDiff (Header blk)] ->
    m [ChainDiff (Header blk)]
  truncateRejectedBlocks cands = do
    invalid <- atomically $ readTVar varInvalid
    let isRejected hdr =
          Map.member (headerHash hdr) (forgetFingerprint invalid)
    return $
      filter (preferAnchoredCandidate bcfg curChain . Diff.getSuffix) $
        map (Diff.takeWhileOldest (not . isRejected)) cands

-- [Ouroboros]
--
-- Ouroboros says that when we are given an invalid chain by a peer, we
-- should reject that peer's chain. However, since we're throwing all
-- blocks together in the ChainDB, we can't tell which block or which
-- chain came from which peer, so we can't simply reject a peer's chain.
--
-- It might be that a good peer gave us a valid chain, but another peer
-- gave us an invalid block that fits onto the chain of the good peer. In
-- that case, we do still want to adopt the chain of the good peer, which
-- is a prefix of the chain that we constructed using all the blocks we
-- found in the VolatileDB, including the invalid block.
--
-- This is the reason why we still take valid prefixes of a invalid chains
-- into account during chain selection: they might correspond to the good
-- peer's valid chain.

-- | Result of 'validateCandidate'.
data ValidationResult m blk
  = -- | The entire candidate fragment was valid.
    FullyValid (ValidatedChainDiff (Header blk) (Forker' m blk))
  | -- | The candidate fragment contained invalid blocks that had to
    -- be truncated from the fragment.
    ValidPrefix (ChainDiff (Header blk))
  | -- | After truncating the invalid blocks from
    -- the 'ChainDiff', it no longer contains enough blocks in its suffix to
    -- compensate for the number of blocks it wants to roll back.
    InsufficientSuffix

-- | Validate a candidate by applying its blocks to the ledger, and return a
-- 'ValidatedChainDiff' for it, i.e., a chain diff along with a ledger
-- corresponding to its tip (the most recent block).
--
-- PRECONDITION: the candidate (chain diff) must fit onto the given current
-- chain.
--
-- If all blocks in the fragment are valid, then the chain diff in the
-- returned 'ValidatedChainDiff' is the same as the given candidate chain
-- diff.
--
-- If a block in the fragment is invalid, then the fragment in the returned
-- 'ValidatedChainDiff' is a prefix of the given candidate chain diff (upto
-- the last valid block).
--
-- Note that this function returns a 'Forker', and that this forker should be
-- closed when it is no longer used!
ledgerValidateCandidate ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  ) =>
  ChainSelEnv m blk ->
  ResourceRegistry m ->
  ChainDiff (Header blk) ->
  m (ValidatedChainDiff (Header blk) (Forker' m blk))
ledgerValidateCandidate chainSelEnv rr chainDiff@(ChainDiff rollback suffix) =
  LedgerDB.validateFork lgrDB rr traceUpdate blockCache rollback newBlocks >>= \case
    ValidateExceededRollBack{} ->
      -- Impossible: we asked the LedgerDB to roll back past the immutable
      -- tip, which is impossible, since the candidates we construct must
      -- connect to the immutable tip.
      error "found candidate requiring rolling back past the immutable tip"
    ValidateLedgerError (AnnLedgerError ledger' pt e) -> do
      lastValid <- atomically $ forkerCurrentPoint ledger'
      let chainDiff' = Diff.truncate (castPoint lastValid) chainDiff
      traceWith validationTracer (InvalidBlock e pt)
      addInvalidBlock e pt
      traceWith validationTracer (ValidCandidate (Diff.getSuffix chainDiff'))

      -- punish the peer who sent a block if it is invalid or a block from its
      -- prefix is invalid
      --
      -- Note that it is a chain selection invariant that all candidates
      -- involve the block being processed: see Lemma 11.1 (Properties of the
      -- set of candidates) in the Chain Selection chapter of the The Cardano
      -- Consensus and Storage Layer technical report.
      whenJust punish $ \(addedPt, punishment) -> do
        let m =
              InvalidBlockPunishment.enact punishment $
                if addedPt == pt
                  then InvalidBlockPunishment.BlockItself
                  else InvalidBlockPunishment.BlockPrefix
        case realPointSlot pt `compare` realPointSlot addedPt of
          LT -> m
          GT -> pure ()
          EQ -> when (lastValid /= realPointToPoint addedPt) m
      -- If pt and addedPt have the same slot, and addedPt is the tip of
      -- the ledger that pt was validated against, then addedPt is an
      -- EBB and is valid.
      --
      -- Otherwise, either pt == addedPt or addedPt comes after pt, so
      -- we should punish. (Tacit assumption made here: it's impossible
      -- three blocks in a row have the same slot.)

      ValidatedDiff.newM chainDiff' ledger'
    ValidateSuccessful ledger' -> do
      traceWith validationTracer (ValidCandidate suffix)
      ValidatedDiff.newM chainDiff ledger'
 where
  ChainSelEnv
    { lgrDB
    , validationTracer
    , blockCache
    , varInvalid
    , punish
    } = chainSelEnv

  traceUpdate = traceWith $ UpdateLedgerDbTraceEvent >$< validationTracer

  newBlocks :: [Header blk]
  newBlocks = AF.toOldestFirst suffix

  -- \| Record the invalid block in 'cdbInvalid' and change its fingerprint.
  addInvalidBlock :: ExtValidationError blk -> RealPoint blk -> m ()
  addInvalidBlock e (RealPoint slot hash) = atomically $
    modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
      WithFingerprint
        (Map.insert hash (InvalidBlockInfo e slot) invalid)
        (succ fp)

-- | Validate a candidate chain using 'ledgerValidateCandidate'.
validateCandidate ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  ) =>
  ChainSelEnv m blk ->
  ResourceRegistry m ->
  ChainDiff (Header blk) ->
  m (ValidationResult m blk)
validateCandidate chainSelEnv rr chainDiff =
  ledgerValidateCandidate chainSelEnv rr chainDiff >>= \case
    validatedChainDiff
      | ValidatedDiff.rollbackExceedsSuffix validatedChainDiff ->
          cleanup validatedChainDiff >> return InsufficientSuffix
      | AF.length (Diff.getSuffix chainDiff) == AF.length (Diff.getSuffix chainDiff') ->
          -- No truncation
          return $ FullyValid validatedChainDiff
      | otherwise ->
          -- In case of invalid blocks, we throw away the ledger
          -- corresponding to the truncated fragment and will have to
          -- validate it again, even when it's the sole candidate.
          return $ ValidPrefix chainDiff'
     where
      chainDiff' = ValidatedDiff.getChainDiff validatedChainDiff
 where
  -- If this function does not return a validated chain diff, then there is a
  -- leftover forker that we have to close so that its resources are correctly
  -- released.
  cleanup :: ValidatedChainDiff b (Forker' m blk) -> m ()
  cleanup = forkerClose . getLedger

{-------------------------------------------------------------------------------
  'ChainAndLedger'
-------------------------------------------------------------------------------}

-- | Instantiate 'ValidatedFragment' in the way that chain selection requires.
type ChainAndLedger m blk = ValidatedFragment (Header blk) (Forker' m blk)

{-------------------------------------------------------------------------------
  Diffusion pipelining
-------------------------------------------------------------------------------}

-- | Check whether a 'ChainDiff' can be pipelined. If it can, the tentative
-- header as well as the new 'TentativeHeaderState' (to be used in case the
-- block body turns out to be invalid) is returned.
--
-- PRECONDITION: The 'ChainDiff' fits on top of the current chain and is better.
isPipelineable ::
  (HasHeader (Header blk), BlockSupportsDiffusionPipelining blk) =>
  BlockConfig blk ->
  TentativeHeaderState blk ->
  ChainDiff (Header blk) ->
  Maybe (Header blk, TentativeHeaderState blk)
isPipelineable bcfg st ChainDiff{..}
  | -- we apply exactly one header
    AF.Empty _ :> hdr <- getSuffix
  , Just st' <- updateTentativeHeaderState bcfg hdr st
  , -- ensure that the diff is applied to the chain tip
    getRollback == 0 =
      Just (hdr, st')
  | otherwise = Nothing

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Wrap a @getter@ function so that it returns 'Nothing' for invalid blocks.
ignoreInvalid ::
  HasHeader blk =>
  proxy blk ->
  InvalidBlocks blk ->
  (HeaderHash blk -> Maybe a) ->
  (HeaderHash blk -> Maybe a)
ignoreInvalid _ invalid getter hash
  | Map.member hash invalid = Nothing
  | otherwise = getter hash

-- | Wrap a @successors@ function so that invalid blocks are not returned as
-- successors.
ignoreInvalidSuc ::
  HasHeader blk =>
  proxy blk ->
  InvalidBlocks blk ->
  (ChainHash blk -> Set (HeaderHash blk)) ->
  (ChainHash blk -> Set (HeaderHash blk))
ignoreInvalidSuc _ invalid succsOf =
  Set.filter (`Map.notMember` invalid) . succsOf
