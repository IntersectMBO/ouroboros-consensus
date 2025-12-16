{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Queries
module Ouroboros.Consensus.Storage.ChainDB.Impl.Query
  ( -- * Queries
    getBlockComponent
  , getCurrentChain
  , getCurrentChainWithTime
  , getCurrentLedger
  , getHeaderStateHistory
  , getImmutableLedger
  , getIsFetched
  , getIsInvalidBlock
  , getIsValid
  , getMaxSlotNo
  , getPastLedger
  , getPerasWeightSnapshot
  , getPerasCertSnapshot
  , getReadOnlyForkerAtPoint
  , getStatistics
  , getTipBlock
  , getTipHeader
  , getTipPoint
  , waitForImmutableBlock

    -- * Low-level queries
  , getAnyBlockComponent
  , getAnyKnownBlock
  , getAnyKnownBlockComponent
  , getChainSelStarvation
  ) where

import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Control.ResourceRegistry (ResourceRegistry)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderStateHistory
  ( HeaderStateHistory (..)
  )
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime)
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot
  , takeVolatileSuffix
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( BlockComponent (..)
  , ChainDbFailure (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasCertSnapshot)
import Ouroboros.Consensus.Storage.VolatileDB (VolatileDB)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util (eitherToMaybe)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (MaxSlotNo, maxSlotNoFromWithOrigin)
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( ChainSelStarvation (..)
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type

-- | Return the last @k@ headers.
--
-- While the in-memory fragment ('cdbChain') might temporarily have more weight
-- than @k@ (until the background thread has copied those blocks to the
-- ImmutableDB), this function will never return a fragment heavier than @k@.
--
-- The anchor point of the returned fragment will be the most recent
-- \"immutable\" block, i.e. a block that cannot be rolled back. In
-- ChainDB.md, we call this block @i@.
--
-- Note that the returned fragment may have weight less than @k@ in case the
-- whole chain itself weights less than @k@, or in case the VolatileDB was
-- corrupted. In the latter case, we don't take blocks already in the
-- ImmutableDB into account, as we know they /must/ have been \"immutable\" at
-- some point, and, therefore, /must/ still be \"immutable\".
getCurrentChain ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  , HasHeader (Header blk)
  , ConsensusProtocol (BlockProtocol blk)
  ) =>
  ChainDbEnv m blk ->
  STM m (AnchoredFragment (Header blk))
getCurrentChain cdb@CDB{..} =
  getCurrentChainLike cdb $ icWithoutTime <$> readTVar cdbChain

-- | Same as 'getCurrentChain', /mutatis mutandi/.
getCurrentChainWithTime ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  , HasHeader (HeaderWithTime blk)
  , ConsensusProtocol (BlockProtocol blk)
  ) =>
  ChainDbEnv m blk ->
  STM m (AnchoredFragment (HeaderWithTime blk))
getCurrentChainWithTime cdb@CDB{..} =
  getCurrentChainLike cdb $ icWithTime <$> readTVar cdbChain

-- | This function is the generalised helper for 'getCurrentChain' and
-- 'getCurrentChainWithTime'. See 'getCurrentChain' for the explanation of it's
-- behaviour.
getCurrentChainLike ::
  forall m blk h.
  ( IOLike m
  , StandardHash blk
  , HasHeader h
  , HeaderHash blk ~ HeaderHash h
  , ConsensusProtocol (BlockProtocol blk)
  ) =>
  ChainDbEnv m blk ->
  STM m (AnchoredFragment h) ->
  STM m (AnchoredFragment h)
getCurrentChainLike cdb@CDB{..} getCurChain = do
  weights <- forgetFingerprint <$> getPerasWeightSnapshot cdb
  takeVolatileSuffix weights k <$> getCurChain
 where
  k = configSecurityParam cdbTopLevelConfig

-- | Get a 'HeaderStateHistory' populated with the 'HeaderState's of the
-- last @k@ blocks of the current chain.
getHeaderStateHistory :: ChainDbEnv m blk -> STM m (HeaderStateHistory blk)
getHeaderStateHistory = LedgerDB.getHeaderStateHistory . cdbLedgerDB

getTipBlock ::
  forall m blk.
  ( IOLike m
  , HasHeader blk
  , HasHeader (Header blk)
  ) =>
  ChainDbEnv m blk ->
  m (Maybe blk)
getTipBlock cdb@CDB{..} = do
  tipPoint <- atomically $ getTipPoint cdb
  case pointToWithOriginRealPoint tipPoint of
    Origin -> return Nothing
    NotOrigin p -> Just <$> getAnyKnownBlock cdbImmutableDB cdbVolatileDB p

getTipHeader ::
  forall m blk.
  ( IOLike m
  , HasHeader blk
  , HasHeader (Header blk)
  ) =>
  ChainDbEnv m blk ->
  m (Maybe (Header blk))
getTipHeader CDB{..} = do
  anchorOrHdr <- AF.head . icWithoutTime <$> atomically (readTVar cdbChain)
  case anchorOrHdr of
    Right hdr -> return $ Just hdr
    Left anch ->
      case pointToWithOriginRealPoint (castPoint (AF.anchorToPoint anch)) of
        Origin -> return Nothing
        NotOrigin p ->
          -- In this case, the fragment is empty but the anchor point is not
          -- genesis. It must be that the VolatileDB got emptied and that our
          -- current tip is now the tip of the ImmutableDB.

          -- Note that we can't use 'getBlockAtTip' because a block might have
          -- been appended to the ImmutableDB since we obtained 'anchorOrHdr'.
          Just <$> ImmutableDB.getKnownBlockComponent cdbImmutableDB GetHeader p

getTipPoint ::
  forall m blk.
  (IOLike m, HasHeader (Header blk)) =>
  ChainDbEnv m blk -> STM m (Point blk)
getTipPoint CDB{..} =
  (castPoint . AF.headPoint . icWithoutTime) <$> readTVar cdbChain

getBlockComponent ::
  forall m blk b.
  IOLike m =>
  ChainDbEnv m blk ->
  BlockComponent blk b ->
  RealPoint blk ->
  m (Maybe b)
getBlockComponent CDB{..} = getAnyBlockComponent cdbImmutableDB cdbVolatileDB

getIsFetched ::
  forall m blk.
  (IOLike m, HasHeader blk) =>
  ChainDbEnv m blk -> STM m (Point blk -> Bool)
getIsFetched CDB{..} = do
  checkQueue <- memberChainSelQueue cdbChainSelQueue
  checkVolDb <- VolatileDB.getIsMember cdbVolatileDB
  return $ \pt ->
    case pointToWithOriginRealPoint pt of
      Origin -> False
      NotOrigin pt' -> checkQueue pt' || checkVolDb (realPointHash pt')

getIsInvalidBlock ::
  forall m blk.
  (IOLike m, HasHeader blk) =>
  ChainDbEnv m blk ->
  STM m (WithFingerprint (HeaderHash blk -> Maybe (ExtValidationError blk)))
getIsInvalidBlock CDB{..} =
  fmap (fmap (fmap invalidBlockReason) . flip Map.lookup) <$> readTVar cdbInvalid

getChainSelStarvation ::
  forall m blk.
  IOLike m =>
  ChainDbEnv m blk ->
  STM m ChainSelStarvation
getChainSelStarvation CDB{..} = readTVar cdbChainSelStarvation

getIsValid ::
  forall m blk.
  (IOLike m, HasHeader blk) =>
  ChainDbEnv m blk ->
  STM m (RealPoint blk -> Maybe Bool)
getIsValid CDB{..} = do
  prevApplied <- LedgerDB.getPrevApplied cdbLedgerDB
  invalid <- forgetFingerprint <$> readTVar cdbInvalid
  return $ \pt@(RealPoint _ hash) ->
    -- A block can not both be in the set of invalid blocks and
    -- previously-applied blocks, so the order in which we check them does not
    -- matter.
    if
      | Map.member hash invalid -> Just False
      | Set.member pt prevApplied -> Just True
      | otherwise -> Nothing

getMaxSlotNo ::
  forall m blk.
  (IOLike m, HasHeader (Header blk)) =>
  ChainDbEnv m blk -> STM m MaxSlotNo
getMaxSlotNo CDB{..} = do
  -- Note that we need to look at both the current chain and the VolatileDB
  -- in all cases (even when the VolatileDB is not empty), because the
  -- VolatileDB might have been corrupted.
  --
  -- For example, imagine the VolatileDB has been corrupted so that it only
  -- contains block 9'. The ImmutableDB contains blocks 1-10. The max slot
  -- of the current chain will be 10 (being the anchor point of the empty
  -- current chain), while the max slot of the VolatileDB will be 9.
  --
  -- Moreover, we have to look in 'ChainSelQueue' too.
  curChainMaxSlotNo <-
    maxSlotNoFromWithOrigin . AF.headSlot . icWithoutTime
      <$> readTVar cdbChain
  volatileDbMaxSlotNo <- VolatileDB.getMaxSlotNo cdbVolatileDB
  queuedMaxSlotNo <- getMaxSlotNoChainSelQueue cdbChainSelQueue
  return $ curChainMaxSlotNo `max` volatileDbMaxSlotNo `max` queuedMaxSlotNo

-- | Get current ledger
getCurrentLedger :: ChainDbEnv m blk -> STM m (ExtLedgerState blk EmptyMK)
getCurrentLedger CDB{..} = LedgerDB.getVolatileTip cdbLedgerDB

-- | Get the immutable ledger, i.e., typically @k@ blocks back.
getImmutableLedger :: ChainDbEnv m blk -> STM m (ExtLedgerState blk EmptyMK)
getImmutableLedger CDB{..} = LedgerDB.getImmutableTip cdbLedgerDB

-- | Get the ledger for the given point.
--
-- When the given point is not among the last @k@ blocks of the current
-- chain (i.e., older than @k@ or not on the current chain), 'Nothing' is
-- returned.
getPastLedger ::
  ChainDbEnv m blk ->
  Point blk ->
  STM m (Maybe (ExtLedgerState blk EmptyMK))
getPastLedger CDB{..} = LedgerDB.getPastLedgerState cdbLedgerDB

getReadOnlyForkerAtPoint ::
  IOLike m =>
  ChainDbEnv m blk ->
  ResourceRegistry m ->
  Target (Point blk) ->
  m (Either LedgerDB.GetForkerError (LedgerDB.ReadOnlyForker' m blk))
getReadOnlyForkerAtPoint CDB{..} = LedgerDB.getReadOnlyForker cdbLedgerDB

getStatistics :: IOLike m => ChainDbEnv m blk -> m (Maybe LedgerDB.Statistics)
getStatistics CDB{..} = LedgerDB.getTipStatistics cdbLedgerDB

getPerasWeightSnapshot ::
  ChainDbEnv m blk -> STM m (WithFingerprint (PerasWeightSnapshot blk))
getPerasWeightSnapshot CDB{..} = PerasCertDB.getWeightSnapshot cdbPerasCertDB

getPerasCertSnapshot ::
  ChainDbEnv m blk -> STM m (PerasCertSnapshot blk)
getPerasCertSnapshot CDB{..} = PerasCertDB.getCertSnapshot cdbPerasCertDB

-- | Wait until the slot of the given point is smaller or equal than the immutable tip slot,
--   and then return:
--   - the block at the target slot if there is a block in the immutable DB at that slot;
--   - the block from the next occupied slot.
waitForImmutableBlock ::
  forall blk m. IOLike m => ChainDbEnv m blk -> RealPoint blk -> m (Maybe (RealPoint blk))
waitForImmutableBlock CDB{cdbImmutableDB} targetRealPoint = do
  -- first, wait until the target slot is older than the immutable tip
  _ <- atomically $ do
    ImmutableDB.getTip cdbImmutableDB >>= \case
      Origin -> retry
      At tip -> do
        check (ImmutableDB.tipSlotNo tip >= realPointSlot targetRealPoint)
        pure (ImmutableDB.tipToRealPoint tip)
  -- then, query the DB for a point at or directly following the target slot
  ImmutableDB.getBlockAtOrAfterPoint cdbImmutableDB targetRealPoint

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Variant of 'getAnyBlockComponent' instantiated with 'GetBlock'.
getAnyKnownBlock ::
  forall m blk.
  ( IOLike m
  , HasHeader blk
  ) =>
  ImmutableDB m blk ->
  VolatileDB m blk ->
  RealPoint blk ->
  m blk
getAnyKnownBlock immutableDB volatileDB =
  getAnyKnownBlockComponent immutableDB volatileDB GetBlock

-- | Wrapper around 'getAnyBlockComponent' for blocks we know should exist.
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlockComponent ::
  forall m blk b.
  ( IOLike m
  , HasHeader blk
  ) =>
  ImmutableDB m blk ->
  VolatileDB m blk ->
  BlockComponent blk b ->
  RealPoint blk ->
  m b
getAnyKnownBlockComponent immutableDB volatileDB blockComponent p = do
  mBlock <-
    mustExist p
      <$> getAnyBlockComponent immutableDB volatileDB blockComponent p
  case mBlock of
    Right b -> return b
    Left err -> throwIO err

-- | Get a block component from either the immutable DB or volatile DB.
--
-- Returns 'Nothing' if the 'Point' is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlockComponent ::
  forall m blk b.
  IOLike m =>
  ImmutableDB m blk ->
  VolatileDB m blk ->
  BlockComponent blk b ->
  RealPoint blk ->
  m (Maybe b)
getAnyBlockComponent immutableDB volatileDB blockComponent p = do
  -- Note: to determine whether a block is in the ImmutableDB, we can
  -- look at the slot of its tip, which we'll call @immTipSlot@. If the
  -- slot of the requested point > @immTipSlot@, then the block will not
  -- be in the ImmutableDB but in the VolatileDB. However, there is a
  -- race condition here: if between the time we got @immTipSlot@ and
  -- the time we look up the block in the VolatileDB the block was moved
  -- from the VolatileDB to the ImmutableDB, and it was deleted from the
  -- VolatileDB, we won't find the block, even though it is in the
  -- ChainDB.
  --
  -- Therefore, we first query the VolatileDB and if the block is not in
  -- it, then we can get @immTipSlot@ and compare it to the slot of the
  -- requested point. If the slot <= @immTipSlot@ it /must/ be in the
  -- ImmutableDB (no race condition here).
  mbVolatileB <-
    VolatileDB.getBlockComponent
      volatileDB
      blockComponent
      hash
  case mbVolatileB of
    Just b -> return $ Just b
    Nothing -> do
      -- ImmutableDB will throw an exception if we ask for a block past the tip
      immTipSlot <- atomically $ ImmutableDB.getTipSlot immutableDB
      if NotOrigin (realPointSlot p) > immTipSlot
        then
          -- It's not supposed to be in the ImmutableDB and the VolatileDB
          -- didn't contain it, so return 'Nothing'.
          return Nothing
        else
          eitherToMaybe
            <$> ImmutableDB.getBlockComponent immutableDB blockComponent p
 where
  hash = realPointHash p

mustExist :: RealPoint blk -> Maybe b -> Either (ChainDbFailure blk) b
mustExist p Nothing = Left $ ChainDbMissingBlock p
mustExist _ (Just b) = Right b
