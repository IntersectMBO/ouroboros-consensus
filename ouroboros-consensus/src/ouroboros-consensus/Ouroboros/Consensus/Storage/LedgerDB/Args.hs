{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Arguments for LedgerDB initialization.
module Ouroboros.Consensus.Storage.LedgerDB.Args
  ( LedgerDbArgs (..)
  , QueryBatchSize (..)
  , defaultArgs
  , defaultQueryBatchSize
  ) where

import Control.ResourceRegistry
import Control.Tracer
import Data.Kind
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Backends
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import Ouroboros.Consensus.Util.Args
import System.FS.API

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | Arguments required to initialize a LedgerDB.
type LedgerDbArgs ::
  (Type -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
data LedgerDbArgs f m blk = LedgerDbArgs
  { lgrSnapshotPolicyArgs :: SnapshotPolicyArgs
  , lgrBackend :: SomeBackendArgs m blk
  , lgrGenesis :: HKD f (m (ExtLedgerState blk ValuesMK))
  , lgrHasFS :: HKD f (SomeHasFS m)
  , lgrConfig :: LedgerDbCfgF f (ExtLedgerState blk)
  , lgrTracer :: Tracer m (TraceEvent blk)
  , lgrRegistry :: HKD f (ResourceRegistry m)
  , lgrQueryBatchSize :: QueryBatchSize
  , lgrStartSnapshot :: Maybe DiskSnapshot
  -- ^ If provided, the ledgerdb will start using said snapshot and fallback
  -- to genesis. It will ignore any other existing snapshots. Useful for
  -- db-analyser.
  }

-- | Default arguments
defaultArgs ::
  Applicative m =>
  Incomplete LedgerDbArgs m blk
defaultArgs =
  LedgerDbArgs
    { lgrSnapshotPolicyArgs = defaultSnapshotPolicyArgs
    , lgrBackend = undefined
    , lgrGenesis = NoDefault
    , lgrHasFS = NoDefault
    , lgrConfig = LedgerDbCfg NoDefault NoDefault OmitLedgerEvents
    , lgrQueryBatchSize = DefaultQueryBatchSize
    , lgrTracer = nullTracer
    , lgrRegistry = NoDefault
    , lgrStartSnapshot = Nothing
    }

{-------------------------------------------------------------------------------
  QueryBatchSize
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
data QueryBatchSize
  = -- | A default value, which is determined by a specific
    -- 'QueryBatchSize'. See 'defaultQueryBatchSize' as an example.
    DefaultQueryBatchSize
  | -- | A requested value: the number of keys to read from disk in each batch.
    RequestedQueryBatchSize Word64
  deriving (Show, Eq, Generic)
  deriving anyclass NoThunks

defaultQueryBatchSize :: QueryBatchSize -> Word64
defaultQueryBatchSize requestedQueryBatchSize = case requestedQueryBatchSize of
  RequestedQueryBatchSize value -> value
  -- Experiments showed that 100_000 is a reasonable value, which yields
  -- acceptable performance. We might want to tweak this further, but for now
  -- this default seems good enough.
  DefaultQueryBatchSize -> 100_000
