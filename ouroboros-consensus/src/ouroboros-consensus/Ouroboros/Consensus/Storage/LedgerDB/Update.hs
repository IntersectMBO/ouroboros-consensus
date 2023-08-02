{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Updating the LedgerDB
module Ouroboros.Consensus.Storage.LedgerDB.Update (
    DiffsToFlush (..)
  , ValidateResult (..)
  , flushIntoBackingStore
  , flushLedgerDB
  , garbageCollectPrevApplied
  , setCurrent
  , validate
  ) where

import           Cardano.Slotting.Slot
import           Control.Monad.Trans (lift)
import           Data.Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Util.IOLike

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent ::
     forall blk m.
     ( MonadSTM m
     , IsLedger (LedgerState blk)
     , HasLedgerTables (LedgerState blk)
     )
  => StrictTVar m (DbChangelog' blk)
  -> AnchorlessDbChangelog' blk
  -> STM m ()
setCurrent v dblog =
  modifyTVar v (\pruned ->
    let s = fromWithOrigin 0
          . pointSlot
          . getTip
          $ changelogLastFlushedState pruned
    in DbChangelog {
          changelogLastFlushedState = changelogLastFlushedState pruned
        , anchorlessChangelog       = AnchorlessDbChangelog {
              adcLastFlushedSlot = adcLastFlushedSlot $ anchorlessChangelog pruned
            , adcStates          = adcStates dblog
            , adcDiffs           =
                ltliftA2 (f s) (adcDiffs $ anchorlessChangelog pruned) (adcDiffs dblog)
            }
        })
  where
    f :: (Ord k, Eq v)
      => SlotNo
      -> SeqDiffMK k v
      -> SeqDiffMK k v
      -> SeqDiffMK k v
    f s (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) = SeqDiffMK $
      if DS.minSlot prunedSeq == DS.minSlot extendedSeq
      then extendedSeq
      else snd $ DS.splitAtSlot s extendedSeq


-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollectPrevApplied :: IOLike m
                          => StrictTVar m (Set (RealPoint blk))
                          -> SlotNo
                          -> STM m ()
garbageCollectPrevApplied prevApplied slotNo = modifyTVar prevApplied $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateResult blk =
    ValidateSuccessful       (AnchorlessDbChangelog' blk)
  | ValidateLedgerError      (AnnLedgerError' blk)
  | ValidateExceededRollBack ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => StrictTVar m (Set (RealPoint blk))
         -> ResolveBlock m blk
         -> TopLevelConfig blk
         -> LedgerBackingStoreValueHandle' m blk
         -> AnchorlessDbChangelog' blk
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate prevApplied
         resolve
         config
         ldbhandle
         changelog
         blockCache
         numRollbacks
         trace
         hdrs = do
    aps <- mkAps <$> atomically (readTVar prevApplied)
    res <- fmap rewrap $ defaultResolveWithErrors resolve $
             switch
               (configLedgerDb config)
               numRollbacks
               (lift . lift . trace)
               aps
               (lift . lift . readKeySetsWith ldbhandle)
               changelog
    atomically $ modifyTVar prevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError' blk) (Either ExceededRollback (AnchorlessDbChangelog' blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l. l ~ ExtLedgerState blk
          => Set (RealPoint blk)
          -> [Ap n l blk ( ResolvesBlocks    n   blk
                         , ThrowsLedgerError n l blk
                         )]
    mkAps prev =
      [ case ( Set.member (headerRealPoint hdr) prev
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->          ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> Weaken $ ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> Weaken $ ApplyVal   blk
          (True,  Just blk) -> Weaken $ ReapplyVal blk
      | hdr <- hdrs
      ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= annLedgerErrRef e)

    addPoints :: [RealPoint blk]
              -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushLedgerDB :: (MonadSTM m, GetTip l, HasLedgerTables l)
              => StrictTVar m (DbChangelog l)
              -> LedgerBackingStore m l
              -> m ()
flushLedgerDB chlogVar bstore = do
  diffs <- atomically $ do
    ldb' <- readTVar chlogVar
    let (toFlush, toKeep) = splitForFlushing ldb'
    case toFlush of
      Nothing -> pure ()
      Just {} -> writeTVar chlogVar toKeep
    pure toFlush
  mapM_ (flushIntoBackingStore bstore) diffs

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.LgrDb'
-- 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.flushLock' write lock must be
-- held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> m ()
flushIntoBackingStore backingStore dblog =
  bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)
