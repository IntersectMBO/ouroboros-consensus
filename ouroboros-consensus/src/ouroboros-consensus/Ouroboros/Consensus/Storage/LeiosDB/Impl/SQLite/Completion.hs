{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Which stored EBs still miss txs, kept in memory rather than in the
-- schema: the writer updates it as it writes, the readers consult it, and
-- opening the database rebuilds it from the closures.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Completion
  ( Missing
  , txIsWritten
  , unwrittenOffsets
  , scanMissing
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Leios.Types (EbHash)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (withReadOnlyConn)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries (sql_scan_closures)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read (boundAt)

-- | The offsets each stored EB body still misses. An EB whose body is stored
-- is complete iff it is absent here; eviction removes it.
type Missing = Map EbHash IntSet

-- | Whether a tx of a closure holds written bytes. Unwritten txs are zeroes,
-- and no transaction's CBOR starts with a zero byte: that is the integer 0.
txIsWritten :: ByteString -> Bool
txIsWritten bs = not (BS.null bs) && BS.head bs /= 0

-- | The offsets of a closure's unwritten txs, given its 'txBounds' and its
-- bytes ('Nothing' before any tx was written).
unwrittenOffsets :: ByteString -> Maybe ByteString -> IntSet
unwrittenOffsets bounds = \case
  Nothing -> IntSet.fromDistinctAscList [0 .. n - 1]
  Just txs ->
    IntSet.fromDistinctAscList
      [ i
      | i <- [0 .. n - 1]
      , not $ txIsWritten $ BS.drop (boundAt bounds i) (BS.take (boundAt bounds (i + 1)) txs)
      ]
 where
  n = BS.length bounds `div` 4 - 1

-- | Rebuild 'Missing' from the volatile partition's closures. Reads every
-- stored closure once.
scanMissing :: HasCallStack => FilePath -> IO Missing
scanMissing volPath =
  withReadOnlyConn volPath $ \db ->
    withStmt db sql_scan_closures $ \stmt ->
      let loop !acc =
            dbStep stmt >>= \case
              DB.Done -> pure acc
              DB.Row -> do
                ebHash <- ebHashFromBlob =<< DB.columnBlob stmt 0
                bounds <- DB.columnBlob stmt 1
                txs <-
                  DB.columnType stmt 2 >>= \case
                    DB.NullColumn -> pure Nothing
                    _ -> Just <$> DB.columnBlob stmt 2
                let missing = unwrittenOffsets bounds txs
                loop $
                  if IntSet.null missing
                    then acc
                    else Map.insert ebHash missing acc
       in loop Map.empty
