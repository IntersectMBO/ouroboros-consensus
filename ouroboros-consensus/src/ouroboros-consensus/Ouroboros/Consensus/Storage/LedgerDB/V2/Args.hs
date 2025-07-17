{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args
  ( FlavorImplSpecificTrace (..)
  , HandleArgs (..)
  , HandleEnv (..)
  , LedgerDbFlavorArgs (..)
  , SomeHasFSAndBlockIO (..)
  , LSMHandleArgs (..)
  ) where

import Control.ResourceRegistry
import Data.Typeable
import Database.LSMTree (LSMTreeTrace (..), Salt, Session)
import Ouroboros.Consensus.Util.Args
import System.FS.API
import System.FS.BlockIO.API

data LedgerDbFlavorArgs f m = V2Args (HandleArgs f m)

data HandleArgs f m
  = InMemoryHandleArgs
  | LSMHandleArgs (LSMHandleArgs f m)

data LSMHandleArgs f m = LSMArgs
  { lsmFilePath :: HKD f FilePath
  , lsmGenSalt :: HKD f (m Salt)
  , lsmMkFS :: HKD f (ResourceRegistry m -> FilePath -> m (ResourceKey m, SomeHasFSAndBlockIO m))
  }

data SomeHasFSAndBlockIO m where
  SomeHasFSAndBlockIO :: (Eq h, Typeable h) => HasFS m h -> HasBlockIO m h -> SomeHasFSAndBlockIO m

data HandleEnv m
  = InMemoryHandleEnv
  | LSMHandleEnv (ResourceKey m, Session m) (ResourceKey m)

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | LSMTrace LSMTreeTrace
  deriving Show
