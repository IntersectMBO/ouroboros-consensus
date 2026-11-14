{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | One whole-EB selection from the maintained mempool index. The caller owns
-- a bounded preparation entry through submission and durable reconciliation.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse
  ( LocalSource (..)
  , mempoolLocalSource
  , LocalReuse
  , PreparedTxs (..)
  , newLocalReuse
  , prepareBodyTxs
  ) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Mempool.API (Mempool (getLeiosTxIndex))
import Ouroboros.Consensus.Util.IOLike

-- | Strictly select named immutable TXs and capture their validation version in
-- one transaction. Materialization happens outside STM and fetch/cache locks.
-- A missing version permits byte reuse, but never fabricates validated status.
data LocalSource m tx = LocalSource
  { localLookup :: [(TxHash, BytesSize)] -> STM m (Map.Map TxHash tx, Maybe ProtVerParity)
  , localBytes :: tx -> m (Maybe LeiosTx)
  }

-- | Select immutable values and their validation version from one current
-- mempool state. Its index read can retry while a writer owns that state;
-- the bounded body worker waits outside fetch/cache locks. The enclosing
-- selection also observes closing, which can skip optional reuse and drain.
-- A readable last-published view during mutation is a separate follow-up.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#local-materialization-and-confirmation@.
mempoolLocalSource ::
  forall blk m.
  (MonadSTM m, LedgerSupportsMempool blk) => Mempool m blk -> LocalSource m (Validated (GenTx blk))
mempoolLocalSource pool =
  LocalSource
    { localLookup = \requested -> do
        (index, version) <- getLeiosTxIndex pool
        let !selected = Map.restrictKeys index (Set.fromList (map fst requested))
            !parity = protVerParity <$> version
        pure (selected, parity)
    , localBytes = pure . fmap MkLeiosTx . txLeiosBytes . txForgetValidated @blk
    }

-- | Verified bytes for the combined body write, and independently captured
-- validation facts. Neither list establishes target durability.
data PreparedTxs = PreparedTxs
  { preparedBytes :: ![(TxOffset, BS.ByteString)]
  , preparedValidated :: ![(TxHash, ProtVerParity)]
  }

newtype LocalReuse m = LocalReuse
  { prepareBodyTxs :: STM m Bool -> [(TxOffset, (TxHash, BytesSize))] -> m PreparedTxs
  }

-- | This helper owns no queue, permit or receipt task. Admission is the shared
-- whole-EB pool's responsibility. Closing is read in the selection transaction,
-- so it can wake a permanently unavailable mempool and skip optional reuse.
newLocalReuse :: IOLike m => LocalSource m tx -> LocalReuse m
newLocalReuse source = LocalReuse $ \closing references -> do
  (hits, parity) <- atomically $ do
    stopped <- closing
    if stopped then pure (Map.empty, Nothing) else localLookup source (map snd references)
  foldM (materialize hits parity) (PreparedTxs [] []) references
 where
  materialize hits parity acc (off, (h, size)) = case Map.lookup h hits of
    Nothing -> pure acc
    Just tx -> do
      bytes <- localBytes source tx
      case bytes of
        Just value@(MkLeiosTx raw)
          | BS.length raw == fromIntegral size
          , hashLeiosTx value == h ->
              let !item = (off, raw)
                  !validated = maybe (preparedValidated acc) (\v -> (h, v) : preparedValidated acc) parity
               in pure $! PreparedTxs (item : preparedBytes acc) validated
        _ -> pure acc
