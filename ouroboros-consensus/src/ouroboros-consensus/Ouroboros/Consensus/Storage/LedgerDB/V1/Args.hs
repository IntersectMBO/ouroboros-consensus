{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Args
  ( BackingStoreArgs (..)
  , FlushFrequency (..)
  , LedgerDbFlavorArgs (..)
  , shouldFlush
  ) where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import qualified Data.SOP.Dict as Dict
import Data.Word
import GHC.Generics
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB
import Ouroboros.Consensus.Util.Args

-- | The number of blocks in the immutable part of the chain that we have to see
-- before we flush the ledger tables to disk. See 'onDiskShouldFlush'.
data FlushFrequency
  = -- | A default value, which is determined by a specific 'SnapshotPolicy'. See
    -- 'defaultSnapshotPolicy' as an example.
    DefaultFlushFrequency
  | -- | A requested value: the number of diffs in the immutable part of the
    -- chain required before flushing.
    RequestedFlushFrequency Word64
  | -- | To disable flushing, to be used in tests
    DisableFlushing
  deriving (Show, Eq, Generic)

shouldFlush :: FlushFrequency -> (Word64 -> Bool)
shouldFlush requestedFlushFrequency = case requestedFlushFrequency of
  RequestedFlushFrequency value -> (>= value)
  DefaultFlushFrequency -> (>= 100)
  DisableFlushing -> const False

data LedgerDbFlavorArgs f m = V1Args
  { v1FlushFrequency :: FlushFrequency
  , v1BackendArgs :: BackingStoreArgs f m
  }

data BackingStoreArgs f m
  = LMDBBackingStoreArgs FilePath (HKD f LMDBLimits) (Dict.Dict MonadIOPrim m)
  | InMemoryBackingStoreArgs

class (MonadIO m, PrimState m ~ PrimState IO) => MonadIOPrim m
instance (MonadIO m, PrimState m ~ PrimState IO) => MonadIOPrim m
