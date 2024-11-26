{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Args (
    BackingStoreArgs (..)
  , FlushFrequency (..)
  , LedgerDbFlavorArgs (..)
  , QueryBatchSize (..)
  , defaultLedgerDbFlavorArgs
  , queryBatchSize
  , shouldFlush
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import qualified Data.SOP.Dict as Dict
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB
import           Ouroboros.Consensus.Util.Args

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | The /maximum/ number of keys to read in a backing store range query.
--
-- When performing a ledger state query that involves on-disk parts of the
-- ledger state, we might have to read ranges of key-value pair data (e.g.,
-- UTxO) from disk using backing store range queries. Instead of reading all
-- data in one go, we read it in batches. 'QueryBatchSize' determines the size
-- of these batches.
--
-- INVARIANT: Should be at least 1.
--
-- It is fine if the result of a range read contains less than this number of
-- keys, but it should never return more.
data QueryBatchSize =
    -- | A default value, which is determined by a specific
    -- 'SnapshotPolicy'. See 'defaultSnapshotPolicy' as an example.
    DefaultQueryBatchSize
    -- | A requested value: the number of keys to read from disk in each batch.
  | RequestedQueryBatchSize Word64

    -- | To disable queries, to be used in tests
  | DisableQuerySize
  deriving (Show, Eq, Generic)
  deriving anyclass NoThunks

queryBatchSize :: QueryBatchSize -> Word64
queryBatchSize requestedQueryBatchSize = case requestedQueryBatchSize of
    RequestedQueryBatchSize value -> value
    DefaultQueryBatchSize         -> 100_000
    DisableQuerySize              -> 0

-- | The number of blocks in the immutable part of the chain that we have to see
-- before we flush the ledger tables to disk. See 'onDiskShouldFlush'.
data FlushFrequency =
  -- | A default value, which is determined by a specific 'SnapshotPolicy'. See
    -- 'defaultSnapshotPolicy' as an example.
    DefaultFlushFrequency
    -- | A requested value: the number of diffs in the immutable part of the
    -- chain required before flushing.
  | RequestedFlushFrequency Word64
    -- | To disable flushing, to be used in tests
  | DisableFlushing
  deriving (Show, Eq, Generic)

shouldFlush :: FlushFrequency -> (Word64 -> Bool)
shouldFlush requestedFlushFrequency = case requestedFlushFrequency of
      RequestedFlushFrequency value -> (>= value)
      DefaultFlushFrequency         -> (>= 100)
      DisableFlushing               -> const False

data LedgerDbFlavorArgs f m = V1Args {
      v1FlushFrequency :: FlushFrequency
    , v1QueryBatchSize :: QueryBatchSize
    , v1BackendArgs    :: BackingStoreArgs f m
  }

data BackingStoreArgs f m =
    LMDBBackingStoreArgs FilePath (HKD f LMDBLimits) (Dict.Dict MonadIOPrim m)
  | InMemoryBackingStoreArgs

class (MonadIO m, PrimState m ~ PrimState IO) => MonadIOPrim m
instance (MonadIO m, PrimState m ~ PrimState IO) => MonadIOPrim m

defaultLedgerDbFlavorArgs :: Incomplete LedgerDbFlavorArgs  m
defaultLedgerDbFlavorArgs = V1Args DefaultFlushFrequency DefaultQueryBatchSize defaultBackingStoreArgs

defaultBackingStoreArgs :: Incomplete BackingStoreArgs m
defaultBackingStoreArgs = InMemoryBackingStoreArgs
