{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args
  ( FlavorImplSpecificTrace (..)
  , HandleArgs (..)
  , HandleEnv (..)
  , LedgerDbFlavorArgs (..)
  , SomeHasFSAndBlockIO (..)
  , LSMHandleArgs (..)
  , LSMResources (..)
  ) where

import Control.ResourceRegistry
import Data.Typeable
import Database.LSMTree (LSMTreeTrace (..), Salt, Session)
import Ouroboros.Consensus.Util.Args
import System.FS.API
import System.FS.BlockIO.API

data LedgerDbFlavorArgs f m = V2Args (HandleArgs f m)

-- | The arguments that are needed to create a 'HandleEnv' for the different
-- backends.
data HandleArgs f m
  = InMemoryHandleArgs
  | LSMHandleArgs (LSMHandleArgs f m)

data LSMHandleArgs f m = LSMArgs
  { lsmFilePath :: HKD f FsPath
  -- ^ The file path relative to the fast storage directory in which the LSM
  -- trees database will be located.
  , lsmSalt :: HKD f Salt
  , lsmMkFS :: HKD f (ResourceRegistry m -> m (ResourceKey m, SomeHasFSAndBlockIO m))
  }

data SomeHasFSAndBlockIO m where
  SomeHasFSAndBlockIO :: (Eq h, Typeable h) => HasFS m h -> HasBlockIO m h -> SomeHasFSAndBlockIO m

-- | The environment used to create new handles
data HandleEnv m
  = InMemoryHandleEnv
  | -- | The environment for creating LSM handles. It carries the 'Session'
    -- together with its resource key and the resource key of the 'HasBlockIO'.
    LSMHandleEnv !(LSMResources m)

data LSMResources m = LSMResources
  { sessionKey :: !(ResourceKey m)
  , sessionResource :: !(Session m)
  , blockIOKey :: !(ResourceKey m)
  }

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | LSMTrace LSMTreeTrace
  deriving Show
