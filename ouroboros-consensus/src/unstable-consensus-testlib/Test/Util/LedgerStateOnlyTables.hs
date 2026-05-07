{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A simple ledger state that only holds ledger tables (and values).
--
-- This is useful when we only need a ledger state and ledger tables, but not
-- necessarily blocks with payloads (such as defined in @Test.Util.TestBlock@).
module Test.Util.LedgerStateOnlyTables
  ( OTLedgerState
  , OTLedgerTables
  , OTBlock
  , emptyOTLedgerState
  , pattern OTLedgerState
  ) where

import Data.MemPack
import GHC.Generics
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Simple ledger state
-------------------------------------------------------------------------------}

type OTLedgerState k v = LedgerState (OTBlock k v)
type OTLedgerTables k v mk = mk (OTBlock k v)

-- | An empty type for blocks, which is only used to record the types @k@ and
-- @v@.
data OTBlock k v

data instance LedgerState (OTBlock k v) (mk :: TableKind) = OTLedgerState
  { otlsLedgerState :: Values (OTBlock k v)
  , otlsLedgerTables :: OTLedgerTables k v mk
  }
  deriving Generic

deriving instance
  (Ord k, Eq v, Eq (mk (OTBlock k v))) =>
  Eq (OTLedgerState k v mk)
deriving stock instance
  (Show k, Show v, Show (mk (OTBlock k v))) =>
  Show (OTLedgerState k v mk)
deriving instance
  (NoThunks k, NoThunks v, NoThunks (mk (OTBlock k v))) =>
  NoThunks (OTLedgerState k v mk)

emptyOTLedgerState ::
  EmptyTable mk =>
  LedgerState (OTBlock k v) mk
emptyOTLedgerState = OTLedgerState emptyTable emptyTable

instance CanUpgradeLedgerTables LedgerState (OTBlock k v) where
  upgradeTables _ _ = id

instance
  MemPack v =>
  IndexedMemPack LedgerState (OTBlock k v) v
  where
  indexedTypeName _ _ = typeName @v
  indexedPackedByteCount _ = packedByteCount
  indexedPackM _ = packM
  indexedUnpackM _ = unpackM

instance (Ord k, MemPack k, MemPack v) => SerializeTablesWithHint LedgerState (OTBlock k v) where
  encodeTablesWithHint = defaultEncodeTablesWithHint
  decodeTablesWithHint = defaultDecodeTablesWithHint

{-------------------------------------------------------------------------------
  Stowable
-------------------------------------------------------------------------------}

instance CanStowLedgerTables (OTLedgerState k v) where
  stowLedgerTables OTLedgerState{otlsLedgerTables} =
    OTLedgerState otlsLedgerTables emptyTable

  unstowLedgerTables OTLedgerState{otlsLedgerState} =
    OTLedgerState
      emptyTable
      otlsLedgerState

{-------------------------------------------------------------------------------
  Simple ledger tables
-------------------------------------------------------------------------------}

type instance TxIn (OTBlock k v) = k
type instance TxOut (OTBlock k v) = v

instance HasLedgerTables LedgerState (OTBlock k v) where
  projectLedgerTables OTLedgerState{otlsLedgerTables} =
    otlsLedgerTables

  withLedgerTables st lt =
    st{otlsLedgerTables = lt}
