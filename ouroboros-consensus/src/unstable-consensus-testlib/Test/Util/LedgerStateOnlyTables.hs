{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | A simple ledger state that only holds ledger tables (and values).
--
-- This is useful when we only need a ledger state and ledger tables, but not
-- necessarily blocks with payloads (such as defined in @Test.Util.TestBlock@).
module Test.Util.LedgerStateOnlyTables (
    OTLedgerState
  , OTLedgerTables
  , pattern OTLedgerState
  ) where

import           Data.MemPack
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)

{-------------------------------------------------------------------------------
  Simple ledger state
-------------------------------------------------------------------------------}

type OTLedgerState  k v = LedgerState  (OTBlock k v)
type OTLedgerTables k v = LedgerTables (OTLedgerState k v)

-- | An empty type for blocks, which is only used to record the types @k@ and
-- @v@.
data OTBlock k v

data instance LedgerState (OTBlock k v) (mk :: MapKind) = OTLedgerState {
    otlsLedgerState  :: ValuesMK k v
  , otlsLedgerTables :: OTLedgerTables k v mk
  }

deriving instance (Ord k, Eq v, Eq (mk k v))
               => Eq (OTLedgerState k v mk)
deriving stock instance (Show k, Show v, Show (mk k v))
                     => Show (OTLedgerState k v mk)

{-------------------------------------------------------------------------------
  Stowable
-------------------------------------------------------------------------------}

instance (Ord k, Eq v, MemPack k, MemPack v)
      => CanStowLedgerTables (OTLedgerState k v) where
  stowLedgerTables OTLedgerState{otlsLedgerTables} =
    OTLedgerState (getLedgerTables otlsLedgerTables) emptyLedgerTables

  unstowLedgerTables OTLedgerState{otlsLedgerState} =
    OTLedgerState
      emptyMK
      (LedgerTables otlsLedgerState)

{-------------------------------------------------------------------------------
  Simple ledger tables
-------------------------------------------------------------------------------}

type instance TxIn  (OTLedgerState k v) = k
type instance TxOut (OTLedgerState k v) = v

instance (Ord k, Eq v, Show k, Show v, MemPack k, MemPack v, NoThunks k, NoThunks v)
      => HasLedgerTables (OTLedgerState k v) where
  projectLedgerTables OTLedgerState{otlsLedgerTables} =
    otlsLedgerTables

  withLedgerTables st lt =
    st { otlsLedgerTables = lt }
