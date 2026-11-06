{-# LANGUAGE BangPatterns #-}

-- | Observe durable storage, without owning the caller's acquisition or write
-- lifetime. Existing handoff/legacy/forge supervisors must still await every
-- accepted write even when its original peer exits.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.Leios.LocalStore (observeLeiosDB, withLocalStore) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Word (Word64)
import Ouroboros.Consensus.Leios.TxCache
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Storage.LeiosDB.API
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | Supply this handle before ChainDB/kernel consumers receive the database.
-- Pruning advances the cache frontier before invoking actual database GC. Raw
-- backends remain usable independently by storage tests.
observeLeiosDB :: IOLike m => LeiosTxCache m -> LeiosDbHandle m -> LeiosDbHandle m
observeLeiosDB cache raw =
  raw
    { openWriter = observedWriter <$> openWriter raw
    , garbageCollect = \slot -> pruneTo cache slot >> garbageCollect raw slot
    }
 where
  observedWriter writer =
    writer
      { writeEbBody = \point body -> do
          -- Force the compact copies before constructing a receipt closure. It
          -- must not retain the caller's original EB body through lazy metadata.
          let !h = endorserBlockHash body
              !refs = packReferences body
          written <- writeEbBody writer point body
          pure $ Promise $ do
            held <- await written
            trackDurableReferences cache h refs
            let present = IntSet.fromList held
            publishConfirmed cache point $
              reverse $
                foldReferences
                  (\acc off txh _ -> if IntSet.member off present then (off, txh) : acc else acc)
                  []
                  refs
            pure held
      , writeTxs = \point txs -> do
          let !metadata =
                forceMetadata
                  [(off, hashLeiosTx (MkLeiosTx bytes), fromIntegral (BS.length bytes)) | (off, bytes) <- txs]
          written <- writeTxs writer point txs
          pure $ Promise $ do
            receipt <- await written
            publish cache point metadata receipt
            pure receipt
      , copyTxs = \point fills -> do
          let !metadata =
                forceMetadata
                  [(fillTargetOffset f, fillExpectedHash f, fillExpectedSize f) | f <- fills]
          written <- copyTxs writer point fills
          pure $ Promise $ do
            receipt <- await written
            forM_ fills $ \fill ->
              when
                ( IntMap.lookup (fillTargetOffset fill) (txCopyFailures receipt)
                    `elem` [Just CopySourceUnavailable, Just CopySourceMismatch]
                )
                $ invalidateLocationIfMatches cache (fillExpectedHash fill) (fillSource fill)
            publish cache point metadata (txCopyWriteReceipt receipt)
            pure receipt
      }
  publish c point metadata receipt =
    let valid = txWriteConfirmed receipt `IntSet.difference` IntSet.fromList (txWriteWrongSize receipt)
     in publishConfirmed c point [(off, h) | (off, h, _) <- metadata, IntSet.member off valid]

-- The strict accumulator traverses the entire list, forcing the hash and size
-- while input bytes are still owned by the submitter. Return a fresh spine whose
-- elements contain no payload or local-copy descriptor references.
forceMetadata :: [(TxOffset, TxHash, BytesSize)] -> [(TxOffset, TxHash, BytesSize)]
forceMetadata = reverse . foldl' (\acc (off, h, n) -> off `seq` h `seq` n `seq` ((off, h, n) : acc)) []

-- | Cache lifetime encloses ChainDB/kernel consumers but is optional for the
-- released/no-atomic path. The surrounding raw database scope still owns close.
withLocalStore ::
  IOLike m =>
  Maybe (CacheConfig, Word64, Word64) ->
  LeiosDbHandle m ->
  ((LeiosDbHandle m, Maybe (LeiosTxCache m)) -> m a) ->
  m a
withLocalStore Nothing raw action = action (raw, Nothing)
withLocalStore (Just (config, k0, k1)) raw action = do
  cache <- newOptimizedCache config k0 k1
  action (observeLeiosDB cache raw, Just cache)
