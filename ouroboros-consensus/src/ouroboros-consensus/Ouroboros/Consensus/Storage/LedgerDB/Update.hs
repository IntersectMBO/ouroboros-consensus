{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Updating the LedgerDB
module Ouroboros.Consensus.Storage.LedgerDB.Update (
    DiffsToFlush (..)
  , LedgerDBUpdate (..)
  , ValidateResult (..)
  , flushIntoBackingStore
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
import qualified Ouroboros.Network.AnchoredSeq as AS

data LedgerDBUpdate = Prune | Extend
  deriving (Show)

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent ::
     forall blk m.
     ( MonadSTM m
     , IsLedger (LedgerState blk)
     , HasLedgerTables (LedgerState blk)
     , HasCallStack
     )
  => StrictTVar m (DbChangelog' blk)
  -> DbChangelog' blk
  -> LedgerDBUpdate
  -> STM m ()
setCurrent v dblog = \case
  Prune ->
    updatePruning (changelogLastFlushedState dblog)
  Extend ->
    updateExtending (changelogVolatileStates dblog) (changelogDiffs dblog)
 where
   updatePruning :: ExtLedgerState blk EmptyMK -> STM m ()
   updatePruning lf =
     let
       f :: (Ord k, Eq v) => SeqDiffMK k v -> SeqDiffMK k v
       f (SeqDiffMK seqExtended) =
         SeqDiffMK $ snd $ DS.splitAtSlot (fromWithOrigin 0 $ pointSlot $ getTip lf) seqExtended
     in
       modifyTVar v (\extended ->
                       DbChangelog {
                          changelogLastFlushedState = lf
                        , changelogVolatileStates   = changelogVolatileStates extended
                        , changelogDiffs            =
                            mapLedgerTables f (changelogDiffs extended)
                        })

   updateExtending ::
        AS.AnchoredSeq
         (WithOrigin SlotNo)
         (ExtLedgerState blk EmptyMK)
         (ExtLedgerState blk EmptyMK)
     -> LedgerTables (ExtLedgerState blk) SeqDiffMK
     -> STM m ()
   updateExtending newVolatile extendedDiffs =
     let
       f :: (Ord k, Eq v) => SeqDiffMK k v -> SeqDiffMK k v -> SeqDiffMK k v
       f (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) =
         SeqDiffMK $
         if DS.minSlot prunedSeq == DS.minSlot extendedSeq
         then extendedSeq
         else
           let (imm, _) = DS.splitAtSlot (fromWithOrigin 0 $ pointSlot $ getTip $ AS.anchor newVolatile) prunedSeq
               (_, rest) = DS.splitAtFromEnd (AS.length newVolatile) extendedSeq
           in DS.append imm rest
     in
       modifyTVar v (\pruned ->
                       DbChangelog {
                          changelogLastFlushedState = changelogLastFlushedState pruned
                        , changelogVolatileStates   = newVolatile
                        , changelogDiffs            =
                            zipLedgerTables f (changelogDiffs pruned) extendedDiffs
                        })
-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.LgrDb'
-- 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.flushLock' write lock must be
-- held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> m ()
flushIntoBackingStore (LedgerBackingStore backingStore) dblog =
  bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)

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
    ValidateSuccessful       (DbChangelog' blk)
  | ValidateLedgerError      (AnnLedgerError' blk)
  | ValidateExceededRollBack ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => StrictTVar m (Set (RealPoint blk))
         -> ResolveBlock m blk
         -> TopLevelConfig blk
         -> LedgerBackingStoreValueHandle' m blk
         -> DbChangelog' blk
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate prevApplied
         resolve
         config
         (LedgerBackingStoreValueHandle s vh)
         ledgerDB
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
               (lift . lift . readKeySetsWith (fmap (s,) . bsvhRead vh))
               ledgerDB
    atomically $ modifyTVar prevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError' blk) (Either ExceededRollback (DbChangelog' blk))
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
