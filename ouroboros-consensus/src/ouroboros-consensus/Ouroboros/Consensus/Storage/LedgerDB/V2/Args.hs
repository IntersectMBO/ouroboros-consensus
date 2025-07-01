{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Args
  ( FlavorImplSpecificTrace (..)
  , HandleArgs (..)
  , HandleEnv (..)
  , LedgerDbFlavorArgs (..)
  ) where

import Database.LSMTree (LSMTreeTrace (..), Session)

data LedgerDbFlavorArgs f = V2Args HandleArgs

data HandleArgs
  = InMemoryHandleArgs
  | LSMHandleArgs FilePath

data HandleEnv m
  = InMemoryHandleEnv
  | LSMHandleEnv (Session m)

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | LSMTrace LSMTreeTrace
  deriving Show
