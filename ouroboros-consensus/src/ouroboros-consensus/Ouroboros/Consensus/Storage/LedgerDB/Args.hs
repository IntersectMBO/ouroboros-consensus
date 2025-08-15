{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
  , LedgerDbFlavorArgs (..)
  , QueryBatchSize (..)
  , defaultArgs
  , defaultQueryBatchSize

    -- * 'GetVolatileSuffix'
  , GetVolatileSuffix (..)
  , praosGetVolatileSuffix
  ) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Control.ResourceRegistry
import Control.Tracer
import Data.Kind
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS
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
  , lgrGenesis :: HKD f (m (ExtLedgerState blk ValuesMK))
  , lgrHasFS :: HKD f (SomeHasFS m)
  , lgrConfig :: LedgerDbCfgF f (ExtLedgerState blk)
  , lgrTracer :: Tracer m (TraceEvent blk)
  , lgrFlavorArgs :: LedgerDbFlavorArgs f m
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
    , lgrGenesis = NoDefault
    , lgrHasFS = NoDefault
    , lgrConfig = LedgerDbCfg NoDefault NoDefault OmitLedgerEvents
    , lgrQueryBatchSize = DefaultQueryBatchSize
    , lgrTracer = nullTracer
    , -- This value is the closest thing to a pre-UTxO-HD node, and as such it
      -- will be the default for end-users.
      lgrFlavorArgs = LedgerDbFlavorArgsV2 (V2.V2Args V2.InMemoryHandleArgs)
    , lgrRegistry = NoDefault
    , lgrStartSnapshot = Nothing
    }

data LedgerDbFlavorArgs f m
  = LedgerDbFlavorArgsV1 (V1.LedgerDbFlavorArgs f m)
  | LedgerDbFlavorArgsV2 (V2.LedgerDbFlavorArgs f m)

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

{-------------------------------------------------------------------------------
  GetVolatileSuffix
-------------------------------------------------------------------------------}

-- | Get the volatile suffix of the given 'AnchoredSeq' of states that the
-- LedgerDB maintains.
newtype GetVolatileSuffix m blk = GetVolatileSuffix
  { getVolatileSuffix ::
      forall s.
      AS.Anchorable (WithOrigin SlotNo) s s =>
      STM
        m
        ( AnchoredSeq (WithOrigin SlotNo) s s ->
          AnchoredSeq (WithOrigin SlotNo) s s
        )
  }
  deriving NoThunks via OnlyCheckWhnfNamed "GetVolatileSuffix" (GetVolatileSuffix m blk)

-- | Return the the most recent @k@ blocks, which is the rule mandated by Praos.
praosGetVolatileSuffix :: IOLike m => SecurityParam -> GetVolatileSuffix m blk
praosGetVolatileSuffix secParam =
  GetVolatileSuffix $ pure $ AS.anchorNewest k
 where
  k = unNonZero $ maxRollbacks secParam
