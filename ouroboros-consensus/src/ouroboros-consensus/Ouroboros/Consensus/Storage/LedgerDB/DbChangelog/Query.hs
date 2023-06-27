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
current :: GetTip l => AnchorlessDbChangelog l -> l EmptyMK
current =
    either id id
  . AS.head
  . adcStates

-- | Information about the state of the ledger at the anchor
anchor :: AnchorlessDbChangelog l -> l EmptyMK
anchor =
    AS.anchor
  . adcStates

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
snapshots :: AnchorlessDbChangelog l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . AS.toNewestFirst
    . adcStates

-- | How many blocks can we currently roll back?
maxRollback :: GetTip l => AnchorlessDbChangelog l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . adcStates

-- | Reference to the block at the tip of the chain
tip :: GetTip l => AnchorlessDbChangelog l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
isSaturated :: GetTip l => SecurityParam -> AnchorlessDbChangelog l -> Bool
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
  -> AnchorlessDbChangelog l
  -> Maybe (l EmptyMK)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Roll back the volatile states up to the specified point.
rollbackToPoint ::
     ( StandardHash l
     , GetTip l
     , HasLedgerTables l
     )
  => Point l -> AnchorlessDbChangelog l -> Maybe (AnchorlessDbChangelog l)
rollbackToPoint pt dblog = do
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . either id id)
        adcStates
    let ndropped = AS.length adcStates - AS.length vol'
        diffs'   = ltmap (trunc ndropped) adcDiffs
    Exn.assert (ndropped >= 0) $ pure AnchorlessDbChangelog {
          adcLastFlushedSlot
        , adcDiffs  = diffs'
        , adcStates = vol'
        }
  where
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs
      , adcStates
      } = dblog

rollbackToAnchor ::
     (GetTip l, HasLedgerTables l)
  => AnchorlessDbChangelog l -> AnchorlessDbChangelog l
rollbackToAnchor dblog =
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs  = diffs'
      , adcStates = AS.Empty (AS.anchor vol)
      }
  where
    AnchorlessDbChangelog {
        adcLastFlushedSlot
      , adcDiffs
      , adcStates = vol
      } = dblog

    ndropped = AS.length vol
    diffs'   =
      ltmap (trunc ndropped) adcDiffs

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
  -> AnchorlessDbChangelog l
  -> Maybe (AnchorlessDbChangelog l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just $ rollbackToAnchor db
    | otherwise
    = rollbackToPoint (castPoint pt) db

immutableTipSlot ::
     GetTip l
  => AnchorlessDbChangelog l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . AS.anchor
    . adcStates

-- | This will be wrong once we have more than one table.
flushableLength :: (HasLedgerTables l, GetTip l) => AnchorlessDbChangelog l -> Word64
flushableLength chlog =
    (\(Sum x) -> x - fromIntegral (AS.length (adcStates chlog)))
  . ltcollapse
  . ltmap (K2 . f)
  $ adcDiffs chlog
 where
   f :: (Ord k, Eq v)
     => SeqDiffMK k v
     -> Sum Word64
   f (SeqDiffMK sq) = Sum $ fromIntegral $ DS.length sq
