{-# LANGUAGE NamedFieldPuns #-}

-- | The bytes the chain has freshly announced to this node: one monotone
-- counter, read by the egress scheduler's credit buckets, which refill by what
-- the chain announces rather than by a clock.
--
-- An adopted block adds to it, once and only while fresh, its size plus the
-- size of the endorser block its header announces, if any ('adoptedBytes'):
-- adoption counts each block once, only once it is validated and servable,
-- and only one of two equivocating announcements.
--
-- Fresh means at most the window after the slot's wall-clock onset, the age up
-- to which endorser block announcements are relayed; older blocks are catch-up
-- and refill nothing.
module Ouroboros.Consensus.Leios.FreshBytes
  ( FreshBytes
  , newFreshBytes
  , readFreshBytes
  , adoptedBytes
  , addAdopted
  , newestFirst
  , freshMaxBytesFromView
  , maxBlockBodyBytes
  ) where

import Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import Control.Monad (when)
import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), LeiosProtocol (..))
import Ouroboros.Consensus.Protocol.Abstract (LedgerView)
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

-- | The fragment's blocks newest first, lazily, so a walk that stops early
-- (as 'adoptedBytes' does at the first block past the window) never builds
-- the whole list the way 'AF.toNewestFirst' does. 'AF.head' is the newest
-- block; 'AF.last' is the oldest.
newestFirst :: HasHeader b => AF.AnchoredFragment b -> [b]
newestFirst af = case AF.head af of
  Left _anchor -> []
  Right b -> b : newestFirst (AF.dropNewest 1 af)

-- | The most bytes one announcement can carry under a ledger view: the block
-- body at its maximum, plus, where the view has Leios, an endorser block and
-- its closure at theirs. Diffusion sizes its egress credit buckets by it.
freshMaxBytesFromView :: LeiosProtocol p => proxy p -> LedgerView p -> Int
freshMaxBytesFromView p lv =
  maxBlockBodyBytes + maybe 0 leiosMaxBytes (getLeiosLedgerView p lv)
 where
  leiosMaxBytes llv = fromIntegral (llvMaxEbBodySize llv) + fromIntegral (llvMaxEbTxsSize llv)

-- | The block body's maximum. A stand-in for the ledger's own parameter,
-- which the protocol-generic ledger view does not carry; small beside the
-- endorser block terms.
maxBlockBodyBytes :: Int
maxBlockBodyBytes = 90112
