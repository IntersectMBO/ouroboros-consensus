{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Querying a DbChangelog
module Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query (
    anchor
  , current
  , flushableLength
  , getPastLedgerAt
  , immutableTipSlot
  , isSaturated
  , lastFlushedState
  , maxRollback
  , rollback
  , rollbackToAnchor
  , rollbackToPoint
  , snapshots
  , tip
  ) where

import           Cardano.Slotting.Slot
import qualified Control.Exception as Exn
import           Data.Semigroup (Sum (..))
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq hiding (empty,
                     extend)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Prelude hiding (splitAt)

-- | The ledger state at the tip of the chain
current :: GetTip l => DbChangelog l -> l EmptyMK
current =
    either id id
  . AS.head
  . changelogVolatileStates

-- | Information about the state of the ledger at the anchor
anchor :: DbChangelog l -> l EmptyMK
anchor =
    AS.anchor
  . changelogVolatileStates

-- | Get the most recently flushed ledger state. This is what will be serialized
-- when snapshotting.
lastFlushedState :: DbChangelog l -> l EmptyMK
lastFlushedState = changelogLastFlushedState

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
snapshots :: DbChangelog l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . AS.toNewestFirst
    . changelogVolatileStates

-- | How many blocks can we currently roll back?
maxRollback :: GetTip l => DbChangelog l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . changelogVolatileStates

-- | Reference to the block at the tip of the chain
tip :: GetTip l => DbChangelog l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
isSaturated :: GetTip l => SecurityParam -> DbChangelog l -> Bool
isSaturated (SecurityParam k) db =
    maxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
getPastLedgerAt ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasTickedLedgerTables l
     )
  => Point blk
  -> DbChangelog l
  -> Maybe (l EmptyMK)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Roll back the volatile states up to the specified point.
rollbackToPoint ::
     ( StandardHash l
     , GetTip l
     , HasLedgerTables l
     )
  => Point l -> DbChangelog l -> Maybe (DbChangelog l)
rollbackToPoint pt dblog = do
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . either id id)
        vol
    let ndropped = AS.length vol - AS.length vol'
        diffs'   = mapLedgerTables (trunc ndropped) changelogDiffs
    Exn.assert (ndropped >= 0) $ pure DbChangelog {
          changelogLastFlushedState
        , changelogDiffs          = diffs'
        , changelogVolatileStates = vol'
        }
  where
    DbChangelog {
        changelogLastFlushedState
      , changelogDiffs
      , changelogVolatileStates = vol
      } = dblog

rollbackToAnchor ::
     (GetTip l, HasLedgerTables l)
  => DbChangelog l -> DbChangelog l
rollbackToAnchor dblog =
    DbChangelog {
        changelogLastFlushedState
      , changelogDiffs          = diffs'
      , changelogVolatileStates = AS.Empty (AS.anchor vol)
      }
  where
    DbChangelog {
        changelogLastFlushedState
      , changelogDiffs
      , changelogVolatileStates = vol
      } = dblog

    ndropped = AS.length vol
    diffs'   =
      mapLedgerTables (trunc ndropped) changelogDiffs

trunc ::
     (Ord k, Eq v)
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (SeqDiffMK sq) =
  SeqDiffMK $ fst $ splitAtFromEnd n sq


-- | Get a prefix of the DbChangelog that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasTickedLedgerTables l
     )
  => Point blk
  -> DbChangelog l
  -> Maybe (DbChangelog l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just $ rollbackToAnchor db
    | otherwise
    = rollbackToPoint (castPoint pt) db

immutableTipSlot ::
     GetTip l
  => DbChangelog l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . AS.anchor
    . changelogVolatileStates

-- | This will be wrong once we have more than one table.
flushableLength :: (HasLedgerTables l, GetTip l) => DbChangelog l -> Word64
flushableLength chlog =
    (\(Sum x) -> x - fromIntegral (AS.length (changelogVolatileStates chlog)))
  . foldLedgerTables f
  $ changelogDiffs chlog
 where
   f :: (Ord k, Eq v)
     => SeqDiffMK k v
     -> Sum Word64
   f (SeqDiffMK sq) = Sum $ fromIntegral $ DS.length sq
