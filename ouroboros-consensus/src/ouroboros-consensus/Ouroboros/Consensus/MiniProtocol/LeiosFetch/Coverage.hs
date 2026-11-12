{-# LANGUAGE BangPatterns #-}

-- | Distinct, confirmed target payload for one EB acquisition. This is not a
-- wire counter: retransmissions, duplicate replies and framing belong to mux.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#tracing@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Coverage
  ( Coverage
  , ClosureSummary (..)
  , beginCoverage
  , beginForgedCoverage
  , confirmNetwork
  , finishCoverage
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Word (Word32, Word64)

-- | Confirmed acquisition origins, counted once per target offset. Network
-- payload means useful validated requests with positive storage confirmation;
-- it does not mean the first physical insertion into SQLite. Local DB/mempool
-- fills and the remaining bytes supplied by our own forge stay local.
data ClosureSummary = ClosureSummary
  { closureTotalBytes :: !Word64
  , closureLocalBytes :: !Word64
  , closureNetworkBytes :: !Word64
  , closureNetworkTransactions :: !Word32
  , closureMissingAtBodyBytes :: !(Maybe Word64)
  -- ^ Missing after DB fills and the bounded mempool pass; unknown if the
  -- acquisition began solely at the forge.
  , closureCompletedByForge :: !Bool
  }
  deriving (Eq, Show)

-- | Keep only an offset set while incomplete; collapse it to scalar facts as
-- soon as the closure completes. Retirement removes the point's whole entry.
data Coverage
  = Receiving !Word64 !(Maybe Word64) !IntSet !Word64
  | Complete !ClosureSummary
  deriving (Eq, Show)

beginCoverage :: Word64 -> Word64 -> Maybe Coverage
beginCoverage total locallyHeld
  | locallyHeld > total = Nothing
  | otherwise =
      let !missing = total - locallyHeld in Just (Receiving total (Just missing) IntSet.empty 0)

beginForgedCoverage :: Word64 -> Coverage
beginForgedCoverage total = Receiving total Nothing IntSet.empty 0

-- | The caller supplies only positively confirmed, originally requested
-- offsets while this target's fetched job pool is live. Repeated/late receipts
-- cannot add the same offset twice, and unknown coverage is never fabricated.
confirmNetwork :: IntMap Word32 -> Coverage -> Maybe Coverage
confirmNetwork _ complete@Complete{} = Just complete
confirmNetwork confirmed (Receiving total missing held bytes) =
  let fresh = IntMap.withoutKeys confirmed held
      !nextBytes = bytes + sum (map fromIntegral (IntMap.elems fresh))
      !nextHeld = IntSet.union held (IntMap.keysSet fresh)
   in if nextBytes > total then Nothing else Just (Receiving total missing nextHeld nextBytes)

-- | Emit at most once, only after semantic closure completion. Callers must
-- independently establish complete confirmation for locally forged data.
finishCoverage :: Bool -> Coverage -> Maybe (ClosureSummary, Coverage)
finishCoverage _ Complete{} = Nothing
finishCoverage forged (Receiving total missing offsets network) =
  let !summary =
        ClosureSummary total (total - network) network (fromIntegral (IntSet.size offsets)) missing forged
   in Just (summary, Complete summary)
