{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args
  ( FlavorImplSpecificTrace (..)
  , HandleArgs (..)
  , HandleEnv (..)
  , LedgerDbFlavorArgs (..)
  , LSMHandleArgs (..)
  ) where

import Data.Void

data LedgerDbFlavorArgs f m = V2Args (HandleArgs f m)

-- | The arguments that are needed to create a 'HandleEnv' for the different
-- backends.
data HandleArgs f m
  = InMemoryHandleArgs
  | LSMHandleArgs (LSMHandleArgs f m)

data LSMHandleArgs f m = LSMArgs Void

-- | The environment used to create new handles
data HandleEnv m
  = InMemoryHandleEnv
  | -- | The environment for creating LSM handles. It carries the 'Session'
    -- together with its resource key and the resource key of the 'HasBlockIO'.
    LSMHandleEnv !Void

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  deriving (Show, Eq)
