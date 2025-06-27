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

import Database.LSMTree (Session)

data LedgerDbFlavorArgs f m = V2Args (HandleArgs m)

data HandleArgs m
  = InMemoryHandleArgs
  | LSMHandleArgs (Session m)

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  deriving (Show, Eq)
