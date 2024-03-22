{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | Arguments for LedgerDB initialization.
module Ouroboros.Consensus.Storage.LedgerDB.Impl.Args (
    LedgerDbArgs (..)
  , LedgerDbFlavorArgs (..)
  , defaultArgs
  ) where

import           Control.Tracer
import           Data.Kind
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.FS.API

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | Arguments required to initialize a LedgerDB.
type LedgerDbArgs ::
     (Type -> Type)
  -> (Type -> Type)
  -> Type
  -> Type
data LedgerDbArgs f m blk = LedgerDbArgs {
      lgrSnapshotPolicyArgs  :: SnapshotPolicyArgs
    , lgrGenesis             :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrHasFS               :: HKD f (SomeHasFS m)
    , lgrSSDHasFS            :: HKD f (SomeHasFS m)
    , lgrSnapshotTablesSSD   :: Bool
    , lgrSnapshotStateSSD    :: Bool
    , lgrConfig              :: HKD f (LedgerDbCfg (ExtLedgerState blk))
    , lgrTracer              :: Tracer m (TraceLedgerDBEvent blk)
    , lgrFlavorArgs          :: LedgerDbFlavorArgs f m
    , lgrRegistry            :: HKD f (ResourceRegistry m)
      -- | If provided, the ledgerdb will start using said snapshot and fallback
      -- to genesis. It will ignore any other existing snapshots. Useful for
      -- db-analyser.
    , lgrStartSnapshot       :: Maybe DiskSnapshot
    }

-- | Default arguments
defaultArgs ::
     ( Applicative m
     )
  => Incomplete LedgerDbArgs m blk
defaultArgs = LedgerDbArgs {
      lgrSnapshotPolicyArgs = SnapshotPolicyArgs DefaultSnapshotInterval DefaultNumOfDiskSnapshots
    , lgrGenesis            = NoDefault
    , lgrHasFS              = NoDefault
    , lgrSSDHasFS           = NoDefault
    , lgrSnapshotTablesSSD  = False
    , lgrSnapshotStateSSD   = False
    , lgrConfig             = NoDefault
    , lgrTracer             = nullTracer
    , lgrFlavorArgs         = LedgerDbFlavorArgsV1 V1.defaultLedgerDbFlavorArgs
    , lgrRegistry           = NoDefault
    , lgrStartSnapshot      = Nothing
    }

data LedgerDbFlavorArgs f m =
    LedgerDbFlavorArgsV1 (V1.LedgerDbFlavorArgs f m)
  | LedgerDbFlavorArgsV2 (V2.LedgerDbFlavorArgs f m)
