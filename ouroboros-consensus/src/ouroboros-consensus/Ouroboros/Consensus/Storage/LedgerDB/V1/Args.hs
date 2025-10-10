{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Args
  ( FlushFrequency (..)
  , LedgerDbBackendArgs (..)
  , shouldFlush
  ) where

import Data.Word
import GHC.Generics
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore

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

data LedgerDbBackendArgs m l = V1Args
  { v1FlushFrequency :: FlushFrequency
  , v1BackendArgs :: SomeBackendArgs m l
  }
