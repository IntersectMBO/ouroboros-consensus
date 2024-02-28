{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args (
    FlavorImplSpecificTrace
  , HandleArgs (..)
  , LedgerDbFlavorArgs (..)
  ) where

import           GHC.Generics
import           NoThunks.Class

data LedgerDbFlavorArgs f m = V2Args HandleArgs

data HandleArgs =
    InMemoryHandleArgs
  | LSMHandleArgs
  deriving (Generic, NoThunks)

data FlavorImplSpecificTrace =
    FlavorImplSpecificTraceInMemory
  | FlavorImplSpecificTraceOnDisk
  deriving (Show, Eq)
