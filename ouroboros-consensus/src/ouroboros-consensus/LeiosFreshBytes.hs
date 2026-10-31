{-# LANGUAGE NamedFieldPuns #-}

-- | The bytes the chain has freshly announced to this node: one monotone
-- counter, read by the egress scheduler's credit buckets, which refill by what
-- the chain announces rather than by a clock.
--
-- Two things add to it, each once and only while fresh:
--
-- * an adopted block, by its size plus the list size of the EB its header
--   announces, if any ('adoptedBytes'): adoption counts each block once,
--   only once it is validated and servable, and only one of two equivocating
--   announcements;
-- * an EB's closure, the sum of its transaction sizes, when its body is first
--   claimed ('addFresh'), since an announcement does not carry it.
--
-- Fresh means at most the window after the slot's wall-clock onset, the age up
-- to which EB announcements are relayed; older blocks are catch-up and refill
-- nothing. Both halves judge by that one clock and window.
module LeiosFreshBytes
  ( FreshBytes
  , newFreshBytes
  , readFreshBytes
  , isFreshAge
  , adoptedBytes
  , addAdopted
  , addFresh
  , newestFirst
  ) where

import Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import Control.Monad (when)
import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (HasHeader)

-- | The window, the counter and the newest adopted slot it has considered.
data FreshBytes m = FreshBytes
  { fbWindow :: !NominalDiffTime
  , fbBytes :: !(StrictTVar m Int)
  , fbLastSlot :: !(StrictTVar m (WithOrigin SlotNo))
  }

-- | A counter at zero with the given window, considering only blocks after
-- the given slot, the chain's tip when the node starts, so a restart refills
-- nothing.
newFreshBytes :: IOLike m => NominalDiffTime -> WithOrigin SlotNo -> m (FreshBytes m)
newFreshBytes fbWindow tip = FreshBytes fbWindow <$> newTVarIO 0 <*> newTVarIO tip

readFreshBytes :: IOLike m => FreshBytes m -> STM m Int
readFreshBytes = readTVar . fbBytes

-- | Whether a block or EB this long after its slot's onset still counts.
isFreshAge :: FreshBytes m -> NominalDiffTime -> Bool
isFreshAge FreshBytes{fbWindow} age = age <= fbWindow

-- | The bytes a newly adopted chain adds, and the newest slot considered after
-- it. The chain comes newest first, each block with its age, which grows
-- along it; the walk stops at the first block past the window, so a
-- long-adopted chain costs nothing to read again. A block counts if it is
-- within the window and its slot is above the last considered. Keyed by slot,
-- not hash, so nothing needs pruning: a block re-adopted after a rollback, or
-- a fork's block at or below a slot already considered, is not counted again,
-- which only ever undercounts.
adoptedBytes ::
  NominalDiffTime ->
  WithOrigin SlotNo ->
  -- | The chain's blocks, newest first: slot, age and bytes
  [(SlotNo, NominalDiffTime, Int)] ->
  (Int, WithOrigin SlotNo)
adoptedBytes window lastSlot chain =
  ( sum [n | (s, _, n) <- takeWhile (\(_, age, _) -> age <= window) chain, At s > lastSlot]
  , case chain of
      (s, _, _) : _ -> max lastSlot (At s)
      [] -> lastSlot
  )

-- | Add what 'adoptedBytes' finds; returns what it added and the new total.
-- Only a change is written, since the egress rule reads the total in
-- transactions of its own.
addAdopted ::
  IOLike m => FreshBytes m -> [(SlotNo, NominalDiffTime, Int)] -> STM m (Int, Int)
addAdopted FreshBytes{fbWindow, fbBytes, fbLastSlot} chain = do
  lastSlot <- readTVar fbLastSlot
  let (n, lastSlot') = adoptedBytes fbWindow lastSlot chain
  when (lastSlot' /= lastSlot) $ writeTVar fbLastSlot lastSlot'
  when (n > 0) $ modifyTVar fbBytes (+ n)
  (,) n <$> readTVar fbBytes

-- | Add bytes the caller has already found fresh and new; returns the new
-- total.
addFresh :: IOLike m => FreshBytes m -> Int -> STM m Int
addFresh FreshBytes{fbBytes} n = do
  when (n > 0) $ modifyTVar fbBytes (+ n)
  readTVar fbBytes

-- | The fragment's blocks newest first, lazily, so a walk that stops early
-- (as 'adoptedBytes' does at the first block past the window) never builds
-- the whole list the way 'AF.toNewestFirst' does. 'AF.head' is the newest
-- block; 'AF.last' is the oldest.
newestFirst :: HasHeader b => AF.AnchoredFragment b -> [b]
newestFirst af = case AF.head af of
  Left _anchor -> []
  Right b -> b : newestFirst (AF.dropNewest 1 af)
