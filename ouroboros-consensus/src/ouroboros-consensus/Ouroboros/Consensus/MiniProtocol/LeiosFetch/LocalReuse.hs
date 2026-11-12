{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | One bounded mempool pass after the database's body fills have committed.
-- See @docs/fetch-policy/leiosfetch-improved.md#offers-and-local-possession@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse
  ( LocalSource (..)
  , mempoolLocalSource
  , LocalReuse
  , newLocalReuse
  , reuseMempool
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (foldM, when)
import qualified Data.ByteString as BS
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Mempool.API (Mempool (lookupLeiosTxs))
import Ouroboros.Consensus.Storage.LeiosDB.API
import Ouroboros.Consensus.Util.IOLike

-- | Borrow only explicitly named exact TXs in STM; serialize immutable selected
-- values outside STM and all fetch/cache locks. No full snapshot or source epoch.
data LocalSource m tx = LocalSource
  { localLookup :: [(TxHash, BytesSize)] -> STM m (Map.Map TxHash tx)
  -- ^ Strict bounded selection. May wait for the mempool owner; closing is
  -- checked in this same transaction by the caller.
  , localBytes :: tx -> m (Maybe LeiosTx)
  -- ^ Materialize a selected immutable value outside locks. Losing its source
  -- membership later does not invalidate exact bytes already selected.
  }

-- | Bind the maintained exact index to the ledger's whole-TX byte projection.
mempoolLocalSource ::
  forall blk m.
  (Applicative m, LedgerSupportsMempool blk) => Mempool m blk -> LocalSource m (Validated (GenTx blk))
mempoolLocalSource pool =
  LocalSource
    { localLookup = lookupLeiosTxs pool
    , localBytes = pure . fmap MkLeiosTx . txBytesForLeios (Proxy @blk) . txForgetValidated @blk
    }

-- | A node-lifetime source and one materialization permit. The permit bounds
-- active slices, not the total number of accepted receipt tasks. The action
-- receives a fresh relevance check, so a completed/retired point stops the pass.
newtype LocalReuse m = LocalReuse
  { reuseMempool :: m Bool -> LeiosPoint -> IntSet -> [(TxOffset, (TxHash, BytesSize))] -> m IntSet
  }

-- | Build once per node. Closing is read in the same STM transactions as gate
-- and source waits, so shutdown can wake a permanently retrying local source.
-- Accepted writes still await their receipts; permits release on every exit.
newLocalReuse :: IOLike m => STM m Bool -> LocalSource m tx -> LeiosDbWriter m -> m (LocalReuse m)
newLocalReuse closing source writer = do
  gate <- S.newTVarIO False
  pure $ LocalReuse $ \relevant point held references ->
    -- DB-confirmed offsets need neither the materialization gate nor a source
    -- read. Filter once, before slicing, to keep this a linear pass.
    resolve gate relevant point held [item | item@(off, _) <- references, not (IntSet.member off held)]
 where
  acquire gate = atomically $ do
    stopped <- closing
    if stopped
      then pure False
      else do
        busy <- S.readTVar gate
        S.check (not busy)
        S.writeTVar gate True
        pure True
  release gate admitted = when admitted $ atomically $ S.writeTVar gate False
  resolve _ _ _ held [] = pure held
  resolve gate relevant point held references = do
    wanted <- relevant
    if not wanted
      then pure held
      else do
        let (batch, rest) = splitSlice references
        next <- bracket (acquire gate) (release gate) $ \admitted -> do
          current <- if admitted then relevant else pure False
          if current then Just <$> resolveSlice point held batch else pure Nothing
        maybe (pure held) (\updated -> resolve gate relevant point updated rest) next
  resolveSlice point held batch
    | null missing = pure held
    | otherwise = do
        hits <- atomically $ do
          stopped <- closing
          if stopped then pure Map.empty else localLookup source (map snd missing)
        payloads <- foldM (materialize hits) [] missing
        -- Admission immediately before closing may enqueue afterwards. Its existing
        -- receipt task must drain that accepted write before storage closes.
        stopped <- atomically closing
        imported <-
          if stopped || null payloads
            then pure IntSet.empty
            else do
              receipt <- await =<< writeTxs writer point payloads
              pure (txWriteConfirmed receipt `IntSet.difference` IntSet.fromList (txWriteWrongSize receipt))
        pure (held `IntSet.union` imported)
   where
    missing = filter (\(off, _) -> not (IntSet.member off held)) batch
  materialize hits acc (off, (h, size)) = case Map.lookup h hits of
    Nothing -> pure acc
    Just tx -> do
      bytes <- localBytes source tx
      pure $ case bytes of
        Just value@(MkLeiosTx raw)
          | BS.length raw == fromIntegral size, hashLeiosTx value == h -> (off, raw) : acc
        _ -> acc

-- Whole transactions may overhang this target; zero-sized/missing entries still
-- consume count credit. Release the permit between slices, never while recursing.
splitSlice ::
  [(TxOffset, (TxHash, BytesSize))] ->
  ([(TxOffset, (TxHash, BytesSize))], [(TxOffset, (TxHash, BytesSize))])
splitSlice = go (0 :: Int) (0 :: Word64) []
 where
  go _ _ acc [] = (reverse acc, [])
  go count bytes acc rest@(item@(_, (_, size)) : more)
    | count >= 128 || bytes >= 20000 = (reverse acc, rest)
    | otherwise = go (count + 1) (bytes + fromIntegral size) (item : acc) more
