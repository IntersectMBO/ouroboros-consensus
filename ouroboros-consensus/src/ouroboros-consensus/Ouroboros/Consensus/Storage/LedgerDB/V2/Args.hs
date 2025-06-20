{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args
  ( FlavorImplSpecificTrace (..)
  , HandleArgs (..)
  , LedgerDbFlavorArgs (..)
  ) where

import Data.Void (Void)
import GHC.Generics
import NoThunks.Class

data LedgerDbFlavorArgs f m = V2Args HandleArgs

data HandleArgs
  = InMemoryHandleArgs
  | LSMHandleArgs Void
  deriving (Generic, NoThunks)

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  deriving (Show, Eq)
