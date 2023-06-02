{-# LANGUAGE NamedFieldPuns #-}
-- |

module Ouroboros.Consensus.Storage.LedgerDB.Args (
    LedgerDBArgs (..)
  , defaultArgs
  ) where

import           Control.Tracer
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.Types
import           Ouroboros.Consensus.Util.Args
import           System.FS.API (SomeHasFS (..))

data LedgerDBArgs f m blk = LedgerDBArgs {
      lgrDiskPolicy           :: DiskPolicy
    , lgrGenesis              :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrHasFS                :: SomeHasFS m
    , lgrTopLevelConfig       :: HKD f (TopLevelConfig blk)
    , lgrTraceLedger          :: Tracer m (DbChangelog' blk)
    , lgrTracer               :: Tracer m (TraceLedgerDBEvent blk)
    , lgrBackingStoreSelector :: !(BackingStoreSelector m)
    }

-- | Default arguments
defaultArgs ::
     Applicative m
  => SomeHasFS m
  -> DiskPolicy
  -> BackingStoreSelector m
  -> LedgerDBArgs Defaults m blk
defaultArgs lgrHasFS diskPolicy bss = LedgerDBArgs {
      lgrDiskPolicy           = diskPolicy
    , lgrGenesis              = NoDefault
    , lgrHasFS
    , lgrTopLevelConfig       = NoDefault
    , lgrTraceLedger          = nullTracer
    , lgrTracer               = nullTracer
    , lgrBackingStoreSelector = bss
    }

